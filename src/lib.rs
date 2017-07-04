#[macro_use]
extern crate nom;

mod token;

pub use token::Token;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Integer(i64),
    Float(i64, u64, Option<i64>),
    Boolean(bool),
    Identifier(Vec<u8>),
    Unary(Token, Box<Expression>),
    Postfix(Postfix, Box<Expression>),
    FunctionCall(Vec<u8>, Vec<Expression>),
    Assignment(Token, Box<Expression>, Box<Expression>),
    Binary(Token, Box<Expression>, Box<Expression>),
    Multi(Vec<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum Postfix {
    Token(Token),
    Swizzle(Vec<u8>),
    Index(Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Empty,
    Declaration(Type, Vec<Declarator>),
    Compound(Vec<Statement>),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    For(Box<Statement>, Option<Expression>, Box<Statement>, Box<Statement>),
    Continue,
    Break,
    Discard,
    Return(Option<Expression>),
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Passing {
    In,
    Out,
    InOut,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Type {
    pub name: Vec<u8>,
    pub uniform: bool,
    /* Passing is not really related to the type but for ease let's
     * just keep it here. */
    pub passing: Passing,
}

#[derive(PartialEq, Debug)]
pub struct Declarator {
    pub name: Vec<u8>,
    pub value: Option<Expression>,
}

#[derive(PartialEq, Debug)]
pub struct ParameterDeclaration {
    pub ty: Type,
    pub name: Vec<u8>,
}

#[derive(PartialEq, Debug)]
pub enum Item {
    Function(Type, Vec<u8>, Vec<ParameterDeclaration>, Statement),
    Variable(Type, Vec<Declarator>),
}

macro_rules! token (
  ($i:expr, $t:ident ( $($p:ident),+ ) ) => (
    {
      use nom::Slice;
      use token::Token::*;

      match ($i).iter().next() {
        None     => nom::IResult::Incomplete::<_, _>(nom::Needed::Size(1)),
        Some(&$t ( $(ref $p),+ )) => nom::IResult::Done($i.slice(1..), ( $(($p).clone()),+ )),
        Some(_)  => nom::IResult::Error(error_position!(nom::ErrorKind::Custom(0), $i)),
      }
    }
  );
  ($i:expr, $t:ident) => (
    {
      use nom::Slice;
      use token::Token::*;

      match ($i).iter().next() {
        None     => nom::IResult::Incomplete::<_, _>(nom::Needed::Size(1)),
        Some(&$t) => nom::IResult::Done($i.slice(1..), $t),
        Some(_)  => nom::IResult::Error(error_position!(nom::ErrorKind::Custom(0), $i)),
      }
    }
  );
);

named!(integer_expression<&[Token], Expression>,
       do_parse!(
           neg: opt!(token!(Minus)) >>
           x: token!(Integer(x)) >>
           ({
               let mut num = x as i64;
               if neg.is_some() {
                   num = -num;
               }
               Expression::Integer(num)
           })
       )
);

named!(float_expression<&[Token], Expression>,
       do_parse!(
           neg: opt!(token!(Minus)) >>
           xs: token!(Float(x,y,e)) >>
           ({
               let (x,y,e) = xs;
               let mut num = x as i64;
               if neg.is_some() {
                   num = -num;
               }
               Expression::Float(num,y,e)
           })
       )
);

named!(boolean_expression<&[Token], Expression>,
       map!(token!(Boolean(x)), Expression::Boolean)
);

named!(identifier_expression<&[Token], Expression>,
       map!(token!(Identifier(x)), Expression::Identifier)
);

named!(parenthesized_expression<&[Token], Expression>,
       do_parse!(
           token!(LeftParen) >>
           expr: expression >>
           token!(RightParen) >>
           (expr)
       )
);

named!(primary_expression<&[Token], Expression>,
       alt!(
           integer_expression | float_expression | boolean_expression |
           identifier_expression | parenthesized_expression
       )
);

named!(unary_operator<&[Token], Token>,
       alt!(token!(Increment) | token!(Decrement) | token!(Plus) |
            token!(Minus) | token!(LogicalNot) | token!(BitwiseNot))
);

named!(unary_expression<&[Token], Expression>,
       alt!(
           do_parse!(
               op: unary_operator >>
               expr: unary_expression >>
               (Expression::Unary(op, Box::new(expr)))
           ) |
           postfix_expression |
           function_call_expression |
           primary_expression
       )
);

named!(swizzle_postfix<&[Token], Postfix>,
       do_parse!(
           token!(Projection) >>
           swizzle: token!(Identifier(x)) >>
           (Postfix::Swizzle(swizzle))
       )
);

named!(index_postfix<&[Token], Postfix>,
       do_parse!(
           token!(LeftBracket) >>
           expr: expression >>
           token!(RightBracket) >>
           (Postfix::Index(Box::new(expr)))
       )
);

named!(postfix<&[Token], Postfix>,
       alt!(
           swizzle_postfix |
           index_postfix |
           map!(alt!(token!(Increment) | token!(Decrement)), Postfix::Token)
       )
);

named!(postfix_expression<&[Token], Expression>,
       do_parse!(
           primary: alt!(function_call_expression | primary_expression) >>
           postfix: postfix >>
           (Expression::Postfix(postfix, Box::new(primary)))
       )
);

named!(function_call_expression<&[Token], Expression>,
       do_parse!(
           name: token!(Identifier(x)) >>
           token!(LeftParen) >>
           args: separated_list!(token!(Comma), assignment_expression) >>
           token!(RightParen) >>
           (Expression::FunctionCall(name, args))
       )
);

named!(assignment_operator<&[Token], Token>,
       alt!(
           token!(Assign) | token!(AssignMul) | token!(AssignDiv) |
           token!(AssignMod) | token!(AssignAdd) | token!(AssignSub)
       )
);

named!(assignment_expression<&[Token], Expression>,
       alt!(
           do_parse!(
               lhs: unary_expression >>
               op: assignment_operator >>
               rhs: assignment_expression >>
               (Expression::Assignment(op, Box::new(lhs), Box::new(rhs)))
           ) |
           logical_or_expression
       )
);

named!(expression<&[Token], Expression>,
       map!(separated_nonempty_list!(token!(Comma), assignment_expression),
            |mut es| {
                if es.len() == 1 {
                    es.remove(0)
                } else {
                    Expression::Multi(es)
                }
            }
       )
);

macro_rules! precedence_chain {
    ($($rule:ident -> $next:ident : $($cons:ident),+);+) => {
        $(
            named!($rule<&[Token], Expression>,
                   do_parse!(
                       first: $next >>
                       rest: many0!(do_parse!(
                           op: alt!($(token!($cons))|+) >>
                           expr: $next >>
                           ((op, expr))
                       )) >>
                       ({
                           let mut rest = rest;
                           if rest.is_empty() {
                               first
                           } else {
                               let (op, rhs) = rest.remove(0);
                               let mut e = Expression::Binary(op, Box::new(first),
                                                              Box::new(rhs));
                               for (next_op, next_rhs) in rest {
                                   e = Expression::Binary(next_op, Box::new(e),
                                                          Box::new(next_rhs));
                               }
                               e
                           }
                       })
                   )
            );
        )+
    }
}

precedence_chain! {
    logical_or_expression -> logical_xor_expression: LogicalOr;
    logical_xor_expression -> logical_and_expression: LogicalXor;
    logical_and_expression -> bitwise_or_expression: LogicalAnd;
    bitwise_or_expression -> bitwise_xor_expression: BitwiseOr;
    bitwise_xor_expression -> bitwise_and_expression: BitwiseXor;
    bitwise_and_expression -> equality_expression: BitwiseAnd;
    equality_expression -> relational_expression: Equal, Unequal;
    relational_expression -> shift_expression: LessThanEqual, GreaterThanEqual,
        LessThan, GreaterThan;
    shift_expression -> sum_expression: ShiftLeft, ShiftRight;
    sum_expression -> product_expression: Plus, Minus;
    product_expression -> unary_expression: Multiply, Divide, Modulus
}

named!(statement<&[Token], Statement>,
       alt!(
           compound_statement | if_statement | for_statement |
           declaration_statement | jump_statement | expression_statement
       )
);

named!(compound_statement<&[Token], Statement>,
       do_parse!(
           token!(LeftBrace) >>
           stmts: many0!(statement) >>
           token!(RightBrace) >>
           (Statement::Compound(stmts))
       )
);

named!(if_statement<&[Token], Statement>,
       do_parse!(
           token!(If) >>
           token!(LeftParen) >>
           cond: expression >>
           token!(RightParen) >>
           t: statement >>
           f: opt!(do_parse!(
               token!(Else) >>
               s: statement >>
               (s)
           )) >>
           ({
               let f = f.map(|s| Box::new(s));
               Statement::If(cond, Box::new(t), f)
           })
       )
);

named!(for_statement<&[Token], Statement>,
       do_parse!(
           token!(For) >>
           token!(LeftParen) >>
           init: alt!(declaration_statement | expression_statement) >>
           cond: opt!(expression) >>
           token!(Semicolon) >>
           incr: opt!(expression) >>
           token!(RightParen) >>
           body: statement >>
           ({
               let incr = incr.map_or(Statement::Empty, Statement::Expression);
               Statement::For(Box::new(init), cond, Box::new(incr), Box::new(body))
           })
       )
);

named!(expression_statement<&[Token], Statement>,
       do_parse!(
           expr: opt!(expression) >>
           token!(Semicolon) >>
           ({
               if let Some(e) = expr {
                   Statement::Expression(e)
               } else {
                   Statement::Empty
               }
           })
       )
);

named!(declaration_statement<&[Token], Statement>,
       do_parse!(
           ty: fully_specified_type >>
           decls: separated_nonempty_list!(token!(Comma), variable_declarator) >>
           token!(Semicolon) >>
           (Statement::Declaration(ty, decls))
       )
);

named!(type_qualifier<&[Token], Token>,
       alt!(token!(Uniform) | token!(InOut) | token!(Out))
);

named!(fully_specified_type<&[Token], Type>,
       do_parse!(
           qualifiers: many0!(type_qualifier) >>
           specifier: token!(Identifier(x)) >>
           (Type {
               name: specifier,
               uniform: qualifiers.contains(&Token::Uniform),
               passing: if qualifiers.contains(&Token::InOut) {
                   Passing::InOut
               } else if qualifiers.contains(&Token::Out) {
                   Passing::Out
               } else {
                   Passing::In
               }
           })
       )
);

named!(variable_declarator<&[Token], Declarator>,
       do_parse!(
           name: token!(Identifier(x)) >>
           /* TODO array specifier */
           value: opt!(do_parse!(
               token!(Assign) >>
               expr: assignment_expression >>
               (expr)
           )) >>
           (Declarator { name, value })
       )
);

named!(jump_statement<&[Token], Statement>,
       do_parse!(
           stmt: alt!(
               map!(token!(Continue), |_| Statement::Continue) |
               map!(token!(Break), |_| Statement::Break) |
               map!(token!(Discard), |_| Statement::Discard) |
               do_parse!(
                   token!(Return) >>
                   expr: opt!(expression) >>
                   (Statement::Return(expr))
               )
           ) >>
           token!(Semicolon) >>
           (stmt)
       )
);

named!(parameter_declaration<&[Token], ParameterDeclaration>,
       do_parse!(
           ty: fully_specified_type >>
           name: token!(Identifier(x)) >>
           (ParameterDeclaration { ty, name })
       )
);

named!(function_item<&[Token], Item>,
       do_parse!(
           ty: fully_specified_type >>
           name: token!(Identifier(x)) >>
           token!(LeftParen) >>
           params: separated_list!(token!(Comma), parameter_declaration) >>
           token!(RightParen) >>
           body: compound_statement >>
           (Item::Function(ty, name, params, body))
       )
);

named!(variable_item<&[Token], Item>,
       map!(declaration_statement,
            |s| if let Statement::Declaration(ty,decls) = s {
                Item::Variable(ty, decls)
            } else {
                unreachable!()
            })
);

named!(translation_unit<&[Token], Vec<Item>>,
       do_parse!(
           items: many0!(alt!(function_item | variable_item)) >>
           token!(Eos) >>
           (items)
       )
);

pub fn parse_translation_unit(s: &[u8]) -> Result<Vec<Item>, String> {
    let tokens = token::lex(s).to_result().map_err(|e| format!("{:?}", e))?;
    translation_unit(&tokens)
        .to_result()
        .map_err(|e| format!("{:?}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! cmp {
        ($rule:ident , $in:expr , error) => {
            {
                let tokens = token::lex($in).to_result().unwrap();
                let result = $rule(&tokens).to_result();
                match result {
                    Err(_) => (),
                    Ok(v) => panic!("expected error, got {:?}", v)
                }
            }
        };
        ($rule:ident , $in:expr , $tokens:expr) => {
            {
                let tokens = token::lex($in).to_result().unwrap();
                let result = match $rule(&tokens).to_result() {
                    Ok(r) => r,
                    Err(e) => panic!("{} (tokens: {:?})", e.description(), tokens),
                };
                assert_eq!(result, $tokens);
            }
        }
    }

    macro_rules! vecs {
        ($in:expr) => {
            (&($in)[..]).into()
        }
    }

    #[test]
    fn test_primary_expression() {
        cmp!(primary_expression, b"42", Expression::Integer(42));
        cmp!(primary_expression, b"-42", Expression::Integer(-42));
        cmp!(primary_expression, b"1.0", Expression::Float(1, 0, None));
        cmp!(primary_expression, b"-1.0", Expression::Float(-1, 0, None));
        cmp!(primary_expression, b"true", Expression::Boolean(true));
        cmp!(
            primary_expression,
            b"hello",
            Expression::Identifier(vecs!(b"hello"))
        );
        cmp!(primary_expression, b"( 42 )", Expression::Integer(42));
    }

    #[test]
    fn test_function_call_expression() {
        use Expression::*;

        cmp!(
            function_call_expression,
            b"foo()",
            FunctionCall(vecs!(b"foo"), vec![])
        );
        cmp!(
            function_call_expression,
            b"foo(1,2)",
            FunctionCall(vecs!(b"foo"), vec![Integer(1), Integer(2)])
        );
    }

    #[test]
    fn test_expression() {
        use Expression::*;

        named!(rule<&[Token], Expression>, do_parse!(
            expr: expression >>
            token!(Eos) >>
            (expr)
        ));

        cmp!(rule, b"1,2", Multi(vec![Integer(1), Integer(2)]));
        cmp!(
            rule,
            b"1<2",
            Binary(Token::LessThan, Box::new(Integer(1)), Box::new(Integer(2)))
        );
    }

    #[test]
    fn test_statement() {
        use Expression::*;
        use Statement::*;
        use Token::{LessThan, Increment};

        named!(rule<&[Token], Statement>, do_parse!(
            stmt: statement >>
            token!(Eos) >>
            (stmt)
        ));

        cmp!(rule, b";", Empty);
        cmp!(rule, b"{}", Compound(vec![]));
        cmp!(
            rule,
            b"{return 1; }",
            Compound(vec![Return(Some(Integer(1)))])
        );
        cmp!(rule, b"unif;", Expression(Identifier(vecs!(b"unif"))));
        cmp!(rule, b"flo;", Expression(Identifier(vecs!(b"flo"))));
        cmp!(rule, b"1;", Expression(Integer(1)));

        cmp!(
            rule,
            b"if (true) {}",
            If(Boolean(true), Box::new(Compound(vec![])), None)
        );
        cmp!(
            rule,
            b"if (true) {} else {}",
            If(
                Boolean(true),
                Box::new(Compound(vec![])),
                Some(Box::new(Compound(vec![])))
            )
        );

        cmp!(
            rule,
            b"for (1;x<2;++y) {}",
            For(
                Box::new(Expression(Integer(1))),
                Some(Binary(
                    LessThan,
                    Box::new(Identifier(vecs!(b"x"))),
                    Box::new(Integer(2))
                )),
                Box::new(Expression(
                    Unary(Increment, Box::new(Identifier(vecs!(b"y"))))
                )),
                Box::new(Compound(vec![]))
            )
        );

        let ty_int = Type {
            name: vecs!(b"int"),
            uniform: false,
            passing: Passing::In,
        };
        let ty_tex = Type {
            name: vecs!(b"sampler2D"),
            uniform: true,
            passing: Passing::In,
        };

        cmp!(
            rule,
            b"int x;",
            Declaration(
                ty_int.clone(),
                vec![
                    Declarator {
                        name: vecs!(b"x"),
                        value: None,
                    },
                ]
            )
        );
        cmp!(
            rule,
            b"int x=1;",
            Declaration(
                ty_int,
                vec![
                    Declarator {
                        name: vecs!(b"x"),
                        value: Some(Integer(1)),
                    },
                ]
            )
        );
        cmp!(
            rule,
            b"uniform sampler2D tex, tex2;",
            Declaration(
                ty_tex,
                vec![
                    Declarator {
                        name: vecs!(b"tex"),
                        value: None,
                    },
                    Declarator {
                        name: vecs!(b"tex2"),
                        value: None,
                    },
                ]
            )
        );

        cmp!(rule, b"return ;", Return(None));
        cmp!(rule, b"return 42 ;", Return(Some(Integer(42))));
    }

    #[test]
    fn test_unary_postfix_op() {
        use Expression::*;
        use Token::{Minus, Decrement};
        use Postfix::Index;

        named!(rule<&[Token], Expression>, do_parse!(
            stmt: expression >>
            token!(Eos) >>
            (stmt)
        ));

        cmp!(rule, b"- 1", Unary(Minus, Box::new(Integer(1))));
        cmp!(
            rule,
            b"--a",
            Unary(Decrement, Box::new(Identifier(vecs!(b"a"))))
        );
        cmp!(
            rule,
            b"a[0]",
            Postfix(
                Index(Box::new(Integer(0))),
                Box::new(Identifier(vecs!(b"a")))
            )
        );
        cmp!(
            rule,
            b"--a[0]",
            Unary(
                Decrement,
                Box::new(Postfix(
                    Index(Box::new(Integer(0))),
                    Box::new(Identifier(vecs!(b"a")))
                ))
            )
        );
    }

    #[test]
    fn test_binary_op() {
        use Expression::*;
        use Token::{Plus, Minus, LessThan, LogicalOr, Multiply};

        named!(rule<&[Token], Expression>, do_parse!(
            stmt: expression >>
            token!(Eos) >>
            (stmt)
        ));

        cmp!(
            rule,
            b"1 + 2",
            Binary(Plus, Box::new(Integer(1)), Box::new(Integer(2)))
        );
        cmp!(
            rule,
            b"1 + 2 - 3",
            Binary(
                Minus,
                Box::new(Binary(Plus, Box::new(Integer(1)), Box::new(Integer(2)))),
                Box::new(Integer(3))
            )
        );
        cmp!(
            rule,
            b"1 + 2 * 3",
            Binary(
                Plus,
                Box::new(Integer(1)),
                Box::new(Binary(Multiply, Box::new(Integer(2)), Box::new(Integer(3))))
            )
        );
        cmp!(
            rule,
            b"1 < 2",
            Binary(LessThan, Box::new(Integer(1)), Box::new(Integer(2)))
        );
        cmp!(
            rule,
            b"1 || 3",
            Binary(LogicalOr, Box::new(Integer(1)), Box::new(Integer(3)))
        );
    }

    #[test]
    fn test_function_item() {
        use Item::*;
        use Statement::*;
        use Type;

        named!(rule<&[Token], Item>, do_parse!(
            stmt: function_item >>
            token!(Eos) >>
            (stmt)
        ));

        let ty_int = Type {
            name: vecs!(b"int"),
            uniform: false,
            passing: Passing::In,
        };

        cmp!(
            rule,
            b"int foo() {}",
            Function(ty_int.clone(), vecs!(b"foo"), vec![], Compound(vec![]))
        );
        cmp!(
            rule,
            b"int foo(int x, int y) {}",
            Function(
                ty_int.clone(),
                vecs!(b"foo"),
                vec![
                    ParameterDeclaration {
                        ty: ty_int.clone(),
                        name: vecs!(b"x"),
                    },
                    ParameterDeclaration {
                        ty: ty_int.clone(),
                        name: vecs!(b"y"),
                    },
                ],
                Compound(vec![])
            )
        );
    }
}
