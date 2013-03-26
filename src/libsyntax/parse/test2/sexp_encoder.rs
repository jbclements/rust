use std;
use core::io;
use core::str;
use core::float;
use core::io::{Writer, WriterUtil, ReaderUtil};


// POLICY : encode strings as symbols in places
// where they're known statically in the encoder
// to be strings, e.g. variant names, structure
// names, field names

// sigh... for the moment, I'm accepting the
// IMHO silly design of the encoder interface,
// which is strongly biased toward producing
// output rather than producing data structures...


// LOTS of stuff here copied from json.rs:
pub struct Encoder {
    wr: @io::Writer,
}

pub fn Encoder(wr: @io::Writer) -> Encoder {
    Encoder { wr: wr }
}

fn unimpl(err: ~str) {
    fail!(fmt!("unimplemented serialization: %?",err))
}

fn unimpl_blank() {
    unimpl(~"too lazy even to specify what was unspecified");
}

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


impl std::serialize::Encoder for Encoder {
    fn emit_nil(&self) { self.wr.write_str(~"(nil)"); }
    
    fn emit_uint(&self, v: uint) { self.wr.write_str (v.to_str()); }
    fn emit_u64(&self, v: u64) { self.wr.write_str (v.to_str()); }
    fn emit_u32(&self, _v: u32) { unimpl(~"u32"); }
    fn emit_u16(&self, _v: u16) { unimpl(~"u16"); }
    fn emit_u8(&self, _v: u8)   { unimpl(~"u8"); }
    
    fn emit_int(&self, v: int) { self.wr.write_str (v.to_str()); }
    fn emit_i64(&self, v: i64) { self.wr.write_str (v.to_str()); }
    fn emit_i32(&self, v: i32) { self.wr.write_str (v.to_str());}
    fn emit_i16(&self, v: i16) { self.wr.write_str (v.to_str()); }
    fn emit_i8(&self, v: i8)   { self.wr.write_str (v.to_str()); }
    
    fn emit_bool(&self, v: bool) {
        self.wr.write_str (if v {~"#t"} else {~"#f"});
    }
    
    fn emit_f64(&self, _v: f64) { unimpl(~"f64"); }
    fn emit_f32(&self, _v: f32) { unimpl(~"f32"); }
    fn emit_float(&self, v: float) {
        self.wr.write_str(float::to_str_digits(v, 6u));
    }
    
    fn emit_char(&self, _v: char) { unimpl(~"char"); }
    
    fn emit_borrowed_str(&self, v: &str) { self.emit_owned_str(v); }
    fn emit_owned_str(&self, v: &str) {
        self.wr.write_str (escape_str(v)); }
    fn emit_managed_str(&self, v: &str) { self.emit_owned_str(v); }
    
    fn emit_borrowed(&self, f: &fn()) {  f() }
    fn emit_owned(&self, f: &fn()) {  f() }
    fn emit_managed(&self, f: &fn()) {  f() }
    
    fn emit_enum(&self, _name: &str, f: &fn()) {
        f(); }

    // 'e' is for enum
    fn emit_enum_variant(&self, name: &str, _id: uint, _cnt: uint, f: &fn()) {
        self.wr.write_str (~"(e ");
        self.wr.write_str (as_symbol (name));
        f();
        self.wr.write_str (~")")
    }

    fn emit_enum_variant_arg(&self, _idx: uint, f: &fn()) {
        self.wr.write_char(' ');
        f();
    }

    // 'v' is for vector
    fn emit_borrowed_vec(&self, _len: uint, f: &fn()) {
        self.wr.write_str (~"(v");
        f();
        self.wr.write_char (')');
    }
    
    fn emit_owned_vec(&self, len: uint, f: &fn()) {
        self.emit_borrowed_vec(len,f);
    }
    fn emit_managed_vec(&self, len: uint, f: &fn()) {
        self.emit_borrowed_vec(len,f);
    }
    fn emit_vec_elt(&self, _idx: uint, f: &fn()) {
        self.wr.write_char (' ');
        f();
    }

    fn emit_rec(&self, f: &fn()) {
        unimpl(~"rec"); f();
    }

    // 's' is for struct
    fn emit_struct(&self, name: &str, _len: uint, f: &fn()) {
        self.wr.write_str (~"(s ");
        self.wr.write_str (as_symbol (name));
        f();
        self.wr.write_str (~")")
    }
    fn emit_field(&self, name: &str, _idx: uint, f: &fn()) {
        self.wr.write_str (~" (" + name + ~" ");
        f();
        self.wr.write_char (')');
    }
    
    fn emit_tup(&self, _len: uint, f: &fn()) {
        unimpl(~"tup"); f();
    }
    fn emit_tup_elt(&self, _idx: uint, f: &fn()) {
        unimpl(~"tup_elt"); f();
    }
}

    

