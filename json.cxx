/*
 * json.cxx
 * 
 * Copyright 2013 lex <lex@fred101>
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 * 
 * 
 */


#include <iostream>
#include <map>
#include <string>
#include <cstdint>
#include <iterator>
#include <utility>
#include <stdexcept>
#include <typeinfo>
#include <sstream>
#include <cctype>
#include <vector>
#include <type_traits>
#include <cstdlib>

namespace json {

typedef uint32_t code_point;
typedef uint8_t octet;
static const code_point EOF_point = 0x200000; // not in Unicode range
static const code_point NO_point = 0x200001;

inline bool islineend( code_point c ){
    return c == '\n'; // || unicode ends s/b added here
};

//
// A UTF8 adapter for the stream class S
//

template<class S>
class utf8_stream {
    struct impl {
        size_t count = 1;
        S &s;
        size_t line = 0, col = 0;
        code_point c = 0;
        code_point pc = NO_point;
        bool new_line = false;
        impl( S &si ):s(si){ c = getcp(); new_line = islineend( c ); };
        code_point getcp(){
            if( !s.good() || s.eof() )return EOF_point;
            octet o = s.get();
            code_point c = o;
            if( o > 0x7f ){
                if( o > 0xbf ) c = ( ( c & 0x1f ) << 6 ) + ( s.get() & 0x3f );
                if( o > 0xdf ) c = ( ( c & 0x3ff ) << 6 ) + ( s.get() & 0x3f );
                if( o > 0xef ) c = ( ( c & 0x7fff ) << 6 ) + ( s.get() & 0x3f );
            }
            return c;
        };
    } *p = nullptr;
public:
    utf8_stream(){};
    utf8_stream( const utf8_stream& i ){
        p = i.p; p->count++;
    };
    utf8_stream( const utf8_stream&& i ){
        p = i.p; i.p = nullptr;
    };
    utf8_stream& operator=( const utf8_stream& i ){
        impl *q = p; p = i.p; if( p )p->count++;
        if( q && q->count > 1 )q->count--;
        else delete q;
        return *this;
    };
    utf8_stream& operator=( utf8_stream&& i ){
        std::swap( p, i.p );
        return *this;
    };
    explicit utf8_stream( S& si ):p(new impl(si)){};
    code_point next(){
        if( !p )return EOF_point;
        bool was_return = p->c == '\r';
        if( p->pc != NO_point ){ p->c = p->pc; p->pc = NO_point; }
        else p->c = p->getcp();
        p->col++;
        if( p->new_line || ( was_return && p->c != '\n' ) ){ p->col = 0; p->line++; }
        p->new_line = islineend( p->c );
        return p->c;
    };
    code_point get(){
        if( !p )return EOF_point;
        return p->c;
    };
    code_point peek(){
        if( !p )return EOF_point;
        if( p->pc <= EOF_point )return p->pc;
        p->pc = p->getcp();
        return p->pc;
    };
    code_point skipspace(){
        if( !p )return EOF_point;
        code_point c = p->c;
        while( c == '\n' || c == ' ' || c == '\t' || c == '\r' ){
            c = next();
        }
        return c;
    };
    code_point nextskipspace(){
        if( !p )return EOF_point;
        next();
        return skipspace();
    };
    size_t line(){
        return p? p->line : 0;
    };
    size_t col(){
        return p? p->col: 0;
    };
    ~utf8_stream(){
        if( p && p->count > 1 )p->count--;
        else delete p;
    };
};

//
// Utility functions
//

// error exceptions, uses $ for substitutions because % is expected to be a c format

struct JSON_parse_error : public std::runtime_error {
    JSON_parse_error( const std::string& s ):std::runtime_error(s){};
};
void JSON_error_message( std::ostream& ss, const char *s ){ ss << s; };
template<typename T, typename... A> void JSON_error_message( std::ostream& ss, const char *s, T v, A... args ){
    while( s && *s ){
        if( *s == '$' ){ ss<<v; return JSON_error_message( ss, ++s, args... ); }
        ss<<*s++;
    }
};
template<typename... A >void JSON_error_throw( const char* c, A... args ){
    std::stringstream ss;
    JSON_error_message( ss, c, args... );
    throw JSON_parse_error( ss.str() );
};

// add code_point to std::string as UTF8

void addu8( std::string& s, code_point c ){
    if( c > 0x10ffff || ( c >= 0xd800 && c <= 0xdff ) )
        throw std::runtime_error("Code point outside Unicode");
    if( c < 0x80 ) s += c;
    else {
        if( c <= 0x7ff ){
            s += ( ( c & 0x7c0 ) >> 6 ) | 0xc0;
        } else {
            if( c <= 0xffff ){
                s += ( ( c & 0xf000 ) >> 12 ) | 0xe0;
            } else {
                s += ( ( c & 0x1c0000 ) >> 18 ) | 0xf0;
                s += ( ( c & 0x3f000 ) >> 12 ) | 0x80;
            }
            s += ( ( c & 0xfc0 ) >> 6 ) | 0x80;
        }
        s += ( c & 0x3f ) | 0x80;
    }
};

std::string&& u8str( code_point c ){
    std::string s; addu8( s, c );
    return std::move( s );
};

// convert n hex digits to code_point

template<class US>code_point gethex( US i, unsigned n ){
    code_point v = 0;
    for( code_point c = i.get(); n > 0; --n, c = i.next() ){
        if( c < 0x80 && std::isxdigit( c ) ){
            if( std::isdigit( c ) )v = v * 16 + c - '0';
            else if( c < 'G' )v = v * 16 + 10 + c - 'A';
            else v = v * 16 + 10 + c - 'a';
        } else JSON_error_throw("JSON parser: expected hex digit at $:$", i.line(), i.col() );
    }
    return v;
};

//
// the json object classes
//

class object; class number; class boolean; class null; class array; class string;
    
//
// The "can be anything" JSON value
//

class value {

    class data {
    public:
        virtual data* copy()=0;
        virtual void print( std::ostream& ) const =0;
        virtual std::string to_string() const = 0;
        virtual ~data(){};
    };
    
    template<class T>class data_type : public data {
        T d;
    public:
        data_type( T v ):d(v){};
        //data_type( T&& v ):d(v){};
        T* get(){ return &d; };
        void set( const T& v ){ d = v; };
        data* copy(){ return new data_type<T>(d); };
        void print( std::ostream& o ) const { o<<d; };
        std::string to_string() const {
            std::ostringstream s; s<<d; return s.str();
        };
        virtual ~data_type(){};
    };

    data *p = nullptr;
    void print( std::ostream& o ) const { if(p)p->print(o); };
    friend std::ostream& operator<<( std::ostream&, const value& );
public:
    value(){};
    value( const value& v ){ if( v.p ) p = v.p->copy(); };
    value( value&& v ):p(v.p){ v.p = nullptr; };
/*
 * the constructors below should be able to be replaced by the commented out
 * template below but gcc insists (incorrectly) on instantiating it on value 
 * as well, even though there is an explicit constructor for value above.
 * Clang++ works.
 * */
    value( object&& );
    value( string&& );
    value( number&& );
    value( null&& );
    value( array&& );
    value( boolean&& );
/*    template<class T>value( T&& v );
        :p(new data_type<typename std::remove_reference<T>::type>( v )){}; */
    value& operator=( const value& v ){
        delete p;
        if( v.p )p = v.p->copy(); else p = nullptr;
        return *this;
    };
    value& operator=( value&& v ){
        delete p;
        p = v.p;
        v.p = nullptr;
        return *this;
    };
    template<class T>T* get() const {
        data_type<T>* d = dynamic_cast< data_type<T>* >( p );
        if( d )return d->get();
        else return nullptr;
    };
    template<class T>void set( const T& v ){
        data_type<T>* d = dynamic_cast< data_type<T>* >(p);
        if( d )d->set( v );
        else { delete p; p = new data_type<T>( v ); };
    };
    template<class T>value& operator=( const T& v ){
        set( v );
        return *this;
    };
    template<class T>operator T() const {
        T* t = get<T>();
        if( !t )throw std::bad_cast();
        return *t;
    };
    template<class T>T&& take() {
        T* t = get<T>();
        if( !t )throw std::bad_cast();
        return std::move(*t);
    };

    std::string to_string(){ return p->to_string(); };
    template<class US> static value parse( US i );
    ~value(){ delete p; };
};
std::ostream& operator<<(std::ostream& o, const value& v ){
    v.print(o);
    return o;
};

//
// classes for each type of value
//

class object {
    std::map<std::string, value> m;
    friend std::ostream& operator<<( std::ostream&, const object& );
public:
    object() = default;
    object( const object& o ):m( o.m ){};
    object( object&& o ):m( std::move( o.m ) ){};
    object& operator=( const object& o ){ m = o.m; return *this; };
    object& operator=( object&& o ){ m = std::move( o.m ); return *this; };
    template<class US> static value parse( US );
    value& operator[]( const std::string& s ){ return m[s]; };
    value& operator[]( std::string&& s ){ return m[s]; };
    bool has( std::string s ){ return m.count(s); };
    ~object() = default;
};


std::ostream& operator<<( std::ostream& o, const object& ob ){
    o<<"{ ";
    bool first = true;
    for( auto a : ob.m ){
        if( !first )o<<", ";
        first = false;
        o<<'"'<<a.first<<"\" : "<<a.second;
    }
    o<<" }";
    return o;
};

class string {
    std::string s;
    friend std::ostream& operator<<( std::ostream&, const string& );
public:
    string() = default;
    string( const string& t ):s(t.s){};
    string( string&& t ):s( std::move( t.s ) ){};
    explicit string( const std::string& i ):s(i){};
    explicit string( std::string&& i ):s(i){};
    string& operator=( const string& t ){ s = t.s; return *this; };
    string& operator=( string&& t ){ s = std::move( t.s ); return *this; };
    template<class US>static value parse( US );
    string& operator+=( code_point ){
        
        return *this;
    };
    string& operator+=( const std::string& t ){ s+=t; return *this; };
    operator std::string(){ return s; };
    ~string() = default;
};

std::ostream& operator<<( std::ostream& o, const string& s ){
    o<<'"';
    for( auto i : s.s ){
        switch( i ){
        case '\n' : o<<"\\n"; break;
        case '\b' : o<<"\\b"; break;
        case '\r' : o<<"\\r"; break;
        case '\f' : o<<"\\f"; break;
        case '\t' : o<<"\\t"; break;
        case '\\' : o<<"\\\\"; break;
        case '"'  : o<<"\\\""; break;
        case '/'  : o<<"\\/"; break;
        default: o<<i; break;
        }
    }
    o<<'"';
    return o;
};

class number {
    union { long l = 0; double d; };
    bool isl = true;
    friend std::ostream& operator<<( std::ostream& o, const number& );
public:
    number() = default;
    number( const number& i ):isl(i.isl){
        if( isl )l = i.l; else d = i.d;
    };
    number( number&& i ):isl(i.isl){
        if( isl )l = i.l; else d = i.d;
    };
    explicit number( long i ):l(i), isl(true){};
    explicit number( double i ):d(i), isl(false){};
    number& operator=( const number& i ){
        isl = i.isl;
        if( isl )l = i.l; else d = i.d;
        return *this;
    };
    template<class US>static value parse( US );
    long aslong(){ if( isl )return l; else return d; };
    double asdouble(){ if( isl )return l; else return d; };
    bool isdouble(){ return !isl; };
    ~number() = default;
};

std::ostream& operator<<( std::ostream& o, const number& n ){
    if(n.isl)o<<n.l; else o<<n.d;
    return o;
};

class array {
    std::vector<value> v;
    friend std::ostream& operator<<( std::ostream& o, const array& );
public:
    array() = default;
    array( const array& a ):v(a.v){};
    array( array&& a ):v( std::move( a.v ) ){};
//    array& operator=( const array& a ){ v = a.v; return *this; };
    array& operator=( array&& a ){ v = std::move( a.v ); return *this; };
    template<class US>static value parse( US );
    value& operator[]( size_t l ){ return v[l]; };
    value& operator[]( number n ){
        long l = n.aslong();
        if( l < 0 || static_cast<size_t>(l) >= v.size() )
            throw std::range_error( "JSON array index by JSON number out of range" );
        return v[l];
    };
    void push_back( value&& o ){
        v.push_back( std::move( o ) );
    };
    ~array() = default;
};

std::ostream& operator<<( std::ostream& o, const array& a ){
    o<<"[ ";
    bool first = true;
    for( auto i : a.v ){
        if( !first )o<<", ";
        first = false;
        o<<i;
    }
    o<<" ]";
    return o;
};

class boolean {
    bool b = false;
    friend std::ostream& operator<<( std::ostream& o, const boolean& );
public:
    boolean() = default;
    boolean( const boolean& i ):b(i.b){};
    boolean( boolean&& i ):b(i.b){};
    boolean& operator=( const boolean& i ){ b = i.b; return *this; };
    boolean& operator=( bool i ){ b = i; return *this; };
    template<class US>static value parse( US );
    ~boolean() = default;
};

std::ostream& operator<<( std::ostream& o, const boolean& b ){
    o<<( b.b?"true":"false" );
    return o;
};

class null {
    friend std::ostream& operator<<( std::ostream& o, const null& );
public:
    null() = default;
    null( const null& ){};
    null( null&& ){};
    null& operator=( const null& ){ return *this; };
    template<class US>static value parse( US );
    ~null() = default;
};

std::ostream& operator<<( std::ostream& o, const null& ){
    o<<"null";
    return o;
};

/*
 * see the comment in the declaration of value
 * */
value::value( string&& o ):p( new data_type<string>( o ) ){};
value::value( object&& o ):p( new data_type<object>( o ) ){};
value::value( null&& o ):p( new data_type<null>( o ) ){};
value::value( boolean&& o ):p( new data_type<boolean>( o ) ){};
value::value( array&& o ):p( new data_type<array>( o ) ){};
value::value( number&& o ):p( new data_type<number>( o ) ){};

//
// parse from a utf8 adapted stream
//
// Note: all parses except value assume that they are entered with 
// i.get() == a legal first character
// all return with i.get() positioned on the last character

template<class US> value string::parse( US i ){
    std::string s;
    long start_line = i.line(), start_col = i.col();
    code_point c;
    for( c = i.next()
         ; c != '"' && c != EOF_point && c >= 0x1f
         ; c = i.next() ){
        if( c == '\\' ){
            switch( (c = i.next()) ){
            case '"': case '\\': case '/': s += c; break;
            case 'b': s += '\b'; break;
            case 'f': s += '\f'; break;
            case 'n': s += '\n'; break;
            case 'r': s += '\r'; break;
            case 't': s += '\t'; break;
            case 'u': i.next(); addu8( s, gethex( i, 4 ) ); break;
            case 'U': i.next(); addu8( s, gethex( i, 6 ) ); break; // Extension to JSON since Unicode is not just 16 bits you idiots
            default:
                JSON_error_throw("JSON parser: illegal escape $ at $:$", u8str( c ), i.line(), i.col());
            }
        } else addu8( s, c );
    }
    if( c != '"' )JSON_error_throw( "JSON parser: Unterminated string starting at $:$", start_line, start_col );
    return value( string( s ) );
};

template<class US> value object::parse( US i ){
    code_point c = i.next();
    object o;
    for( c = i.skipspace(); c != '}' && c != EOF_point; ){
        if( c != '"' )JSON_error_throw("JSON parser: expected string at $:$", i.line(), i.col());
        value k( string::parse( i ) );
        if( i.nextskipspace() != ':' )JSON_error_throw("JSON parser: expected ':' at $:$", i.line(), i.col() );
        i.next();
        value v = value::parse( i );
        o[std::string(k.take<string>())] = v;
        c = i.nextskipspace();
        if( c != ',' && c != '}' )JSON_error_throw("JSON parser: expected ',' or '}' at $:$", i.line(), i.col() );
        if( c == ',' )c = i.nextskipspace();
    }
    if( c != '}' )JSON_error_throw("JSON parser: unterminated object at $:$", i.line(), i.col() );
    return value( std::move( o ) );
};

template<class US> value boolean::parse( US i ){
    boolean b;
    code_point c;
    if( i.get() == 't' ){
        if( ( c = i.next() ) == 'r' && ( c = i.next() ) == 'u' 
            && ( c = i.next() ) == 'e' )b = true;
        else JSON_error_throw("JSON parser: unexpected character in boolean $ at $:$", c, i.line(), i.col() );
    } else if( i.get() == 'f' ){
        if( ( c = i.next() ) == 'a' && ( c = i.next() ) == 'l' 
            && ( c = i.next() ) == 's' && ( c = i.next() ) == 'e' )b = false;
        else JSON_error_throw("JSON parser: unexpected character in boolean $ at $:$", c, i.line(), i.col() );
    }
    return value( std::move( b ) );
};

template<class US> value null::parse( US i ){
    //null n;
    code_point c = i.get();
    if( ( c = i.next() ) != 'u' || ( c = i.next() ) != 'l' || ( c = i.next() ) != 'l' )
        JSON_error_throw("JSON parser: unexpected character in null $ at $:$", c, i.line(), i.col() );
    return value( null() );
};

template<class US> value number::parse( US i ){
    std::string s;
    code_point c = i.get();
    if( c == '-' ){
        s += c; c = i.next();
        if( c > 0x7f || ! std::isdigit( c ) )
            JSON_error_throw("JSON parser: expected digit after minus at $:$", i.line(), i.col() );
    }
    s += c;
    if( c != '0' ){
        while( ( c = i.peek() ) < 0x7f && std::isdigit( c ) )s += i.next();
    } else c = i.peek();
    if( c == '.' ){
        s += c; c = i.next();
        if( ( c = i.peek() ) > 0x7f || !std::isdigit( c ) )
            JSON_error_throw("JSON parser: expected digit after dot at $:$", i.line(), i.col() );
        while( ( c = i.peek() ) < 0x7f && std::isdigit( c ) )s += i.next();
        if( c == 'e' || c == 'E' ){
            s += i.next(); c = i.peek();
            if( c == '+' || c == '-' ){ s += i.next(); c = i.peek(); }
            if( c > 0x7f || !std::isdigit( c ) )
                JSON_error_throw("JSON parser: expected at least one digit in exponent at $:$", i.line(), i.col() );
            while( c < 0x7f && std::isdigit( c ) ){
                s += i.next(); c = i.peek();
            }
        }
        double d = std::strtod( s.c_str(), nullptr );
        return value( number( d ) );
    } else {
        long l = std::strtol( s.c_str(), nullptr, 10 );
        return value( number( l ) );
    }
};

template<class US> value array::parse( US i ){
    code_point c = i.next();
    array a;
    for( c = i.skipspace(); c != ']' && c != EOF_point; ){
        a.push_back( value::parse( i ) );
        c = i.nextskipspace();
        if( c != ',' && c != ']' )JSON_error_throw("JSON parser: expected ',' or ']' at $:$", i.line(), i.col() );
        if( c == ',' )c = i.nextskipspace();
    }
    if( c != ']' )JSON_error_throw("JSON parser: unterminated array at $:$", i.line(), i.col() );
    return value( std::move( a ) );
};

template<class US> value value::parse( US i ){
    for( code_point c = i.skipspace(); c != EOF_point; c = i.nextskipspace() ){
        switch( c ){
        case '[': return array::parse( i );
        case '{': return object::parse( i );
        case '"': return string::parse( i );
        case 't': return boolean::parse( i );
        case 'f': return boolean::parse( i );
        case 'n': return null::parse( i );
        case '-': case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9': return number::parse( i );
        case ' ': case '\t': case '\n': case '\r': break;
        default: 
            JSON_error_throw("JSON parser: unexpected code_point $$ at $$:$", std::hex, c, std::dec, i.line(), i.col() );
        }
    }
    //return value();
};

}; // namespace json

std::stringstream j(u8R"xxx( { "abc" : 123, "def" :"ab\\ncde"  , "ghi": true,
"jkl" : [ false, 123.45e15, null], "mno":-55} )xxx");
int main(int , char **)
{
    json::utf8_stream<std::stringstream> uj( j );
    std::cout<<json::value::parse( uj );
    return 0;
}

