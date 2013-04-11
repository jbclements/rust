// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use core::io::{Writer, WriterUtil, ReaderUtil};
use core::str;
use core::float;
use serialize;

// This module encodes Rust values as strings
// that are readable as s-expressions.

// It encodes strings as strings, and numbers as numbers

// It encodes structures in the form
// (s structName (fieldOne 34) (fieldTwo "zbx"))
// ... for a structure such as
// structName{fieldOne:34,fieldTwo:~"zbx"}
// note that it uses symbols rather than strings
// in places where they're known statically in the
// encoder to be strings: variant names, structure
// names, field names.

// It encodes enums in the form
// (e variantName 34 "zbx")
// ... for an enum such as
// variantName(34,"zbx")



// POLICY : encode strings as symbols in places
// where they're known statically in the encoder
// to be strings, e.g. variant names, structure
// names, field names

// LOTS of stuff here copied from json.rs:
pub struct Encoder {
    wr: @Writer,
}

// given a writer, produce a new sexp encoder
pub fn Encoder(wr: @Writer) -> Encoder {
    Encoder { wr: wr }
}


fn unimpl(err: ~str) {
    fail!(fmt!("unimplemented serialization: %?",err))
}

// note on escaping: the goal of escaping is so that it
// can later be parsed by another tool.  In my case,
// I want to choose a syntax that can be parsed by
// Racket's default parser.

// escape a string so that it can be read as a string
fn escape_str(s: &str) -> ~str {
    let mut escaped = ~"\"";
    for str::each_char(s) |c| {
        match c {
          '"' => escaped += ~"\\\"",
          '\\' => escaped += ~"\\\\",
          '\x08' => escaped += ~"\\b",
          '\x0c' => escaped += ~"\\f",
          '\n' => escaped += ~"\\n",
          '\r' => escaped += ~"\\r",
          '\t' => escaped += ~"\\t",
          _ => escaped += str::from_char(c)
        }
    };
    escaped + ~"\""
}

// escape a string containing funny chars so that it can be read as a symbol
pub fn vbar_escape_str(s: &str) -> ~str {
    let mut escaped = ~"|";
    for str::each_char(s) |c| {
        match c {
            '|' => escaped += ~"\\|",
            // is there really no nicer way than this? :
            _ => escaped += str::from_char(c)
        }
    };
    escaped + ~"|"
}

// encode a symbol as a string. leave it alone unless it contains funny
// chars.
pub fn as_symbol (s: &str) -> ~str {
    // no regex library!
    let mut symbol_clean = true;
    for str::each_char(s) |c| {
        if ((c < '0')
            || ((c > '9') && (c < 'A'))
            || ((c > 'Z') && (c < '_'))
            || ((c > '_') && (c < 'a'))
            || ((c > 'z'))) {
            symbol_clean = false;
        }
    }
    if (symbol_clean) {
        s.to_str()
    } else {
        vbar_escape_str(s)
    }
}


impl serialize::Encoder for Encoder {
    fn emit_nil(&mut self) { self.wr.write_str(~"(nil)"); }

    fn emit_uint(&mut self, v: uint) { self.wr.write_str (v.to_str()); }
    fn emit_u64(&mut self, v: u64) { self.wr.write_str (v.to_str()); }
    fn emit_u32(&mut self, v: u32) { self.wr.write_str (v.to_str()); }
    fn emit_u16(&mut self, v: u16) { self.wr.write_str (v.to_str()); }
    fn emit_u8(&mut self, v: u8)   { self.wr.write_str (v.to_str()); }

    fn emit_int(&mut self, v: int) { self.wr.write_str (v.to_str()); }
    fn emit_i64(&mut self, v: i64) { self.wr.write_str (v.to_str()); }
    fn emit_i32(&mut self, v: i32) { self.wr.write_str (v.to_str());}
    fn emit_i16(&mut self, v: i16) { self.wr.write_str (v.to_str()); }
    fn emit_i8(&mut self, v: i8)   { self.wr.write_str (v.to_str()); }

    fn emit_bool(&mut self, v: bool) {
        self.wr.write_str (if v {~"#t"} else {~"#f"});
    }

    fn emit_f64(&mut self, _v: f64) { unimpl(~"f64"); }
    fn emit_f32(&mut self, _v: f32) { unimpl(~"f32"); }
    fn emit_float(&mut self, v: float) {
        self.wr.write_str(float::to_str_digits(v, 6u));
    }

    fn emit_char(&mut self, _v: char) { unimpl(~"char"); }

    fn emit_str(&mut self, v: &str) {
        self.wr.write_str (escape_str(v)); }

    fn emit_enum(&mut self, _name: &str, f: &fn(&mut Encoder)) {
        f(self); }

    // 'e' is for enum
    fn emit_enum_variant(&mut self, name: &str, _id: uint, _cnt: uint, f: &fn(&mut Encoder)) {
        self.wr.write_str (~"(e ");
        self.wr.write_str (as_symbol (name));
        f(self);
        self.wr.write_str (~")")
    }

    fn emit_enum_variant_arg(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        self.wr.write_char(' ');
        f(self);
    }

    // 'v' is for vector
    fn emit_seq(&mut self, _len: uint, f: &fn(&mut Encoder)) {
        self.wr.write_str (~"(v");
        f(self);
        self.wr.write_char (')');
    }

    fn emit_seq_elt(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        self.wr.write_char (' ');
        f(self);
    }

    // 's' is for struct
    fn emit_struct(&mut self, name: &str, _len: uint, f: &fn(&mut Encoder)) {
        self.wr.write_str (~"(s ");
        self.wr.write_str (as_symbol (name));
        f(self);
        self.wr.write_str (~")")
    }
    fn emit_struct_field(&mut self, name: &str, _idx: uint, f: &fn(&mut Encoder)) {
        self.wr.write_str (~" (" + name + ~" ");
        f(self);
        self.wr.write_char (')');
    }

    fn emit_tuple(&mut self, _len: uint, f: &fn(&mut Encoder)) {
        self.wr.write_char('(');
        f(self);
        self.wr.write_char(')');
    }
    fn emit_tuple_arg(&mut self, _idx: uint, f: &fn(&mut Encoder)) {
        unimpl(~"tup_elt"); f(self);
    }
}


#[cfg(test)]
mod test {

    extern mod syntax;
    use super::Encoder;
    use serialize;
    use super::*;
    use core::io;
    use core::str;
    use core::io::Writer;

    // easier to write tests that use auto_encode....
    fn to_sexp_str<E : serialize::Encodable<Encoder>>(v : E) -> ~str{
        let bw = @io::BytesWriter {bytes: ~[], pos: 0};
        let bww : @Writer = (bw as @Writer);
        let e = Encoder(bww);
        v.encode(&e);
        str::from_bytes(bw.bytes.data)
    }

    #[auto_encode]
    enum TestEnum {
        A(int),
        B(~str),
        C(int,uint),
    }

    macro_rules! encode_test (
        ($v:expr,$expected:expr) => (assert_eq!(to_sexp_str ($v),$expected))
    )

    #[test]
    fn test_vbar_escape () {
        assert_eq!(vbar_escape_str (~"abc|def|"),~"|abc\\|def\\||");
    }
    #[test]
    fn test_as_symbol() {
        assert_eq!(as_symbol (~"defG987"),~"defG987");
        assert_eq!(as_symbol (~"de_fG9_87"),~"de_fG9_87");
        assert_eq!(as_symbol (~"de_fG9`_87"),~"|de_fG9`_87|");
        assert_eq!(as_symbol (~"def 879"),~"|def 879|");
    }

    #[auto_encode]
    struct TestStruct {
        f_1 : int,
        f_2 : TestEnum
    }

    #[test]
    fn test_write () {
        encode_test! (@12, ~"12");
        encode_test! (@A(12), ~"(e A 12)");
        encode_test! (@C(42,2), ~"(e C 42 2)");
        encode_test! (@B(~"abc"), ~"(e B \"abc\")");
        encode_test! (@TestStruct {f_2: B (~"def"), f_1: 16},
                     ~"(s TestStruct (f_1 16) (f_2 (e B \"def\")))");
        encode_test! (@~[43,287,2],~"(v 43 287 2)");
    }

    struct BPos(uint);

    #[auto_encode]
    pub struct HasPos { pos : BPos }

    #[test]
    fn test_write_newtype () {
        encode_test! (@HasPos {pos:BPos(78)},~"(s HasPos (pos 78))");
    }

    type allNums = (uint,u64,u32,u16,u8,int,i64,i32,i16,i8);


    #[test]
    fn test_write_nums(){
        encode_test!((3,3,3,3,3,3,3,3,3,3),
                    ~"(tup )")
    }
}
