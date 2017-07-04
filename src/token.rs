use nom::{IResult, ErrorKind};
use std;
use nom;

#[derive(PartialEq, Debug)]
pub enum Token {
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equal,
    Unequal,
    LessThanEqual,
    GreaterThanEqual,
    LessThan,
    GreaterThan,
    ShiftLeft,
    ShiftRight,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    LeftParen,
    RightParen,
    Assign,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignAdd,
    AssignSub,
    Increment,
    Decrement,
    LogicalNot,
    BitwiseNot,
    Projection,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Identifier(Vec<u8>),
    Integer(u64),
    Float(u64, u64, Option<i64>), /* Integer, decimal, exponent */
    Boolean(bool),
    If,
    Else,
    For,
    Break,
    Continue,
    Discard,
    Return,
    Uniform,
    InOut,
    Out,
    Eos,
}

use self::Token::*;

macro_rules! string_tokens {
    ($($name:ident $token:expr => $out:expr);+) => {
        $(
            named!($name<&[u8], Token>,
                   do_parse!(tag!($token) >> ($out))
            );
        )+
    }
}

string_tokens!(op_lor "||" => LogicalOr;
               op_lxor "^^" => LogicalXor;
               op_land "&&" => LogicalAnd;
               op_bor "|" => BitwiseOr;
               op_bxor "^" => BitwiseXor;
               op_band "&" => BitwiseAnd;
               op_eq "==" => Equal;
               op_neq "!=" => Unequal;
               op_leq "<=" => LessThanEqual;
               op_geq ">=" => GreaterThanEqual;
               op_lt "<" => LessThan;
               op_gt ">" => GreaterThan;
               op_ls "<<" => ShiftLeft;
               op_rs ">>" => ShiftRight;
               op_add "+" => Plus;
               op_sub "-" => Minus;
               op_mul "*" => Multiply;
               op_div "/" => Divide;
               op_mod "%" => Modulus;
               lparen "(" => LeftParen;
               rparen ")" => RightParen;
               lbracket "[" => LeftBracket;
               rbracket "]" => RightBracket;
               lbrace "{" => LeftBrace;
               rbrace "}" => RightBrace;
               comma "," => Comma;
               semicolon ";" => Semicolon;
               projection "." => Projection;
               assign "=" => Assign;
               assign_mul "*=" => AssignMul;
               assign_div "/=" => AssignDiv;
               assign_mod "%=" => AssignMod;
               assign_add "+=" => AssignAdd;
               assign_sub "-=" => AssignSub;
               op_incr "++" => Increment;
               op_decr "--" => Decrement;
               op_lnot "!" => LogicalNot;
               op_bnot "~" => BitwiseNot);

named!(lex_assign<&[u8], Token>,
       alt_complete!(
           assign | assign_mul | assign_div | assign_mod |
           assign_add | assign_sub)
);

named!(lex_operator<&[u8], Token>,
       alt_complete!(
           op_lor | op_lxor | op_land | op_ls | op_rs |
           op_incr | op_decr | op_lnot | op_bnot |
           op_eq | op_neq | op_leq | op_geq | op_lt | op_gt |
           op_bor | op_bxor | op_band |
           op_add | op_sub | op_mul | op_div | op_mod)
);

named!(lex_punct<&[u8], Token>,
       alt_complete!(
           lparen | rparen | lbracket | rbracket | comma | projection |
           lbrace | rbrace | semicolon)
);

fn parse_ident_byte(input: &[u8], succ: bool) -> IResult<&[u8], u8> {
    let (i1, c1) = try_parse!(input, take!(1));
    let c = c1[0];
    match c {
        b'A'...b'Z' | b'a'...b'z' | b'_' => IResult::Done(i1, c),
        b'0'...b'9' if succ => IResult::Done(i1, c),
        _ => IResult::Error(error_position!(ErrorKind::Custom(0), input)),
    }
}

named!(lex_ident<&[u8], Token>,
        do_parse!(
            first: apply!(parse_ident_byte, false) >>
            rest: many0!(apply!(parse_ident_byte, true)) >>
            (Identifier({ let mut x = vec![first]; x.extend(&rest); x }))
        )
);

named!(hex_digits<&[u8], u64>,
       do_parse!(tag!("0x") >>
                 digits: map_res!(map_res!(nom::hex_digit, std::str::from_utf8),
                                  |d| u64::from_str_radix(d, 16)) >>
                 (digits))
);

named!(dec_digits<&[u8], u64>,
       map_res!(map_res!(nom::digit, std::str::from_utf8), std::str::FromStr::from_str)
);

named!(lex_integer<&[u8], Token>,
       do_parse!(
           number: alt!(hex_digits | dec_digits) >>
           exponent: opt!(complete!(float_exponent)) >>
           ({
               /* Ugly */
               if let Some(exp) = exponent {
                   Token::Float(number, 0, Some(exp))
               } else {
                   Token::Integer(number)
               }
           })
       )
);

named!(float_exponent<&[u8], i64>,
       do_parse!(
           tag!("e") >>
           neg: opt!(tag!("-")) >>
           exp: dec_digits >>
           ({
               let mut exp = exp as i64;
               if neg.is_some() {
                   exp = -exp;
               }
               exp
           })
       )
);

named!(us_float<&[u8], (u64, u64, Option<i64>)>,
       do_parse!(
           tag!(".") >>
           decimal: dec_digits >>
           exponent: opt!(complete!(float_exponent)) >>
           ((0, decimal, exponent))
       )
);

named!(rest_float<&[u8], (u64, u64, Option<i64>)>,
       do_parse!(
           integer: dec_digits >>
           tag!(".") >>
           decimal: opt!(dec_digits) >>
           exponent: opt!(complete!(float_exponent)) >>
           ((integer, decimal.unwrap_or(0), exponent))
       )
);

named!(lex_float<&[u8], Token>,
       map!(alt!(us_float | rest_float), |(i,d,e)| Token::Float(i,d,e))
);

named!(lex_reserved<&[u8], Token>, alt!(
    map!(tag!("true"), |_| Token::Boolean(true)) |
    map!(tag!("false"), |_| Token::Boolean(false)) |
    map!(tag!("if"), |_| Token::If) |
    map!(tag!("else"), |_| Token::Else) |
    map!(tag!("for"), |_| Token::For) |
    map!(tag!("break"), |_| Token::Break) |
    map!(tag!("continue"), |_| Token::Continue) |
    map!(tag!("discard"), |_| Token::Discard) |
    map!(tag!("return"), |_| Token::Return) |
    map!(tag!("uniform"), |_| Token::Uniform) |
    map!(tag!("inout"), |_| Token::InOut) |
    map!(tag!("out"), |_| Token::Out)
));

named!(lex_token<&[u8], Token>,
       alt_complete!(
           lex_assign | lex_operator | lex_reserved | lex_ident | lex_float |
           lex_integer | lex_punct
           )
);

named!(lex_tokens<&[u8], Vec<Token>>,
       do_parse!(
           tokens: ws!(many0!(lex_token)) >>
           eof!() >>
           ({
               let mut tokens = tokens;
               tokens.push(Token::Eos);
               tokens
           })
       )
);

pub fn lex(input: &[u8]) -> nom::IResult<&[u8], Vec<Token>> {
    lex_tokens(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! cmp {
        ($rule:ident , $in:expr , error) => {
            {
                let result = $rule($in).to_result();
                match result {
                    Err(_) => (),
                    Ok(v) => panic!("expected error, got {:?}", v)
                }
            }
        };
        ($rule:ident , $in:expr , $tokens:expr) => {
            {
                use self::Token::*;

                let result = match $rule($in).to_result() {
                    Ok(r) => r,
                    Err(e) => panic!("{}", e.description()),
                };
                let len = result.len();
                assert_eq!(&result[0..len-1], $tokens);
            }
        }
    }

    macro_rules! vecs {
        ($in:expr) => {
            (&($in)[..]).into()
        }
    }

    #[test]
    fn test_lexer() {
        cmp!(lex_tokens, b"+-*/", &[Plus,Minus,Multiply,Divide]);
        cmp!(lex_tokens, b"+=", &[AssignAdd]);
        cmp!(lex_tokens, b"<<", &[ShiftLeft]);

        cmp!(lex_tokens, b"foobar", &[Identifier(vecs!(b"foobar"))]);
        cmp!(lex_tokens, b"Foo_bar", &[Identifier(vecs!(b"Foo_bar"))]);

        cmp!(lex_tokens, b"42", &[Integer(42)]);
        cmp!(lex_tokens, b"-42", &[Minus,Integer(42)]);
        cmp!(lex_tokens, b"0x42", &[Integer(0x42)]);

        cmp!(lex_tokens, b"1.5", &[Float(1,5,None)]);
        cmp!(lex_tokens, b".5", &[Float(0,5,None)]);
        cmp!(lex_tokens, b"1e5", &[Float(1,0,Some(5))]);

        cmp!(lex_tokens, b"true", &[Boolean(true)]);
        cmp!(lex_tokens, b"false", &[Boolean(false)]);

        cmp!(lex_tokens, b"a.xyz", &[Identifier(vecs!(b"a")), Projection,
             Identifier(vecs!(b"xyz"))]);

        cmp!(lex_tokens, b"foo(1,2)", &[Identifier(vecs!(b"foo")), LeftParen,
             Integer(1), Comma, Integer(2), RightParen]);
    }
}
