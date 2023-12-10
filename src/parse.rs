use std::sync::OnceLock;

use crate::{
    sexp::{parse_sexp, SExp},
    tokenize::tokenize,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'a> {
    Not(Box<Expr<'a>>),
    Lt(Box<Expr<'a>>, Box<Expr<'a>>),
    Gt(Box<Expr<'a>>, Box<Expr<'a>>),
    Eq(Box<Expr<'a>>, Box<Expr<'a>>),
    Add(Box<Expr<'a>>, Box<Expr<'a>>),
    Sub(Box<Expr<'a>>, Box<Expr<'a>>),
    Mul(Box<Expr<'a>>, Box<Expr<'a>>),
    Div(Box<Expr<'a>>, Box<Expr<'a>>),
    Concat(Box<Expr<'a>>, Box<Expr<'a>>),
    If(Box<Expr<'a>>, Box<Expr<'a>>, Box<Expr<'a>>),
    Lambda(&'a str, Box<Expr<'a>>),
    Call(Box<Expr<'a>>, Box<Expr<'a>>),
    Id(&'a str),
    Lit(LiteralKind<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind<'a> {
    Double(f64),
    String(&'a str),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub source: &'a str,
    pub kind: ExprKind<'a>,
}

fn sexp_to_expr<'a>(sexp: &SExp<'a>) -> Expr<'a> {
    match sexp {
        SExp::Leaf(source @ "true") => Expr {
            source,
            kind: ExprKind::Lit(LiteralKind::Bool(true)),
        },
        SExp::Leaf(source @ "false") => Expr {
            source,
            kind: ExprKind::Lit(LiteralKind::Bool(false)),
        },
        SExp::Leaf(a) => match a.parse() {
            Ok(d) => Expr {
                source: a,
                kind: ExprKind::Lit(LiteralKind::Double(d)),
            },
            Err(_) => Expr {
                source: a,
                kind: ExprKind::Id(a),
            },
        },
        SExp::StringLeaf(s) => Expr {
            source: s,
            kind: ExprKind::Lit(LiteralKind::String(&s[1..s.len() - 1])),
        },
        SExp::Branch(source, elements) => match &elements[0] {
            SExp::Leaf("concat") => Expr {
                source: source,
                kind: ExprKind::Concat(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("!") => Expr {
                source: source,
                kind: ExprKind::Not(Box::new(sexp_to_expr(&elements[1]))),
            },
            SExp::Leaf("+") => Expr {
                source: source,
                kind: ExprKind::Add(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("-") => Expr {
                source: source,
                kind: ExprKind::Sub(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("*") => Expr {
                source: source,
                kind: ExprKind::Mul(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("/") => Expr {
                source: source,
                kind: ExprKind::Div(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("=") => Expr {
                source: source,
                kind: ExprKind::Eq(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("<") => Expr {
                source: source,
                kind: ExprKind::Lt(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf(">") => Expr {
                source: source,
                kind: ExprKind::Gt(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("if") => Expr {
                source: source,
                kind: ExprKind::If(
                    Box::new(sexp_to_expr(&elements[1])),
                    Box::new(sexp_to_expr(&elements[2])),
                    Box::new(sexp_to_expr(&elements[3])),
                ),
            },
            SExp::Leaf("let") => Expr {
                source: source,
                kind: ExprKind::Call(
                    Box::new(Expr {
                        source: source,
                        kind: ExprKind::Lambda(
                            match &elements[1] {
                                SExp::Leaf(x) => x,
                                _ => panic!("not a name to bind"),
                            },
                            Box::new(sexp_to_expr(&elements[3])),
                        ),
                    }),
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            SExp::Leaf("letrec") => {
                static Y: OnceLock<Expr<'static>> = OnceLock::new();
                let y = Y.get_or_init(|| {
                    parse("(lambda f ((lambda x (x x)) (lambda x (f (lambda y ((x x) y))))))")
                });
                let n = match &elements[1] {
                    SExp::Leaf(x) => x,
                    _ => panic!("not a name to bind"),
                };
                let call_y = ExprKind::Call(
                    Box::new(y.clone()),
                    Box::new(Expr {
                        source: source,
                        kind: ExprKind::Lambda(n, Box::new(sexp_to_expr(&elements[2]))),
                    }),
                );

                Expr {
                    source: source,
                    kind: ExprKind::Call(
                        Box::new(Expr {
                            source: source,
                            kind: ExprKind::Lambda(n, Box::new(sexp_to_expr(&elements[3]))),
                        }),
                        Box::new(Expr {
                            source: source,
                            kind: call_y,
                        }),
                    ),
                }
            }
            SExp::Leaf("lambda") => Expr {
                source: source,
                kind: ExprKind::Lambda(
                    match &elements[1] {
                        SExp::Leaf(x) => x,
                        _ => panic!("not a name to bind"),
                    },
                    Box::new(sexp_to_expr(&elements[2])),
                ),
            },
            a => Expr {
                source: source,
                kind: ExprKind::Call(
                    Box::new(sexp_to_expr(&a)),
                    Box::new(sexp_to_expr(&elements[1])),
                ),
            },
        },
    }
}

pub fn parse<'a>(program: &'a str) -> Expr<'a> {
    let mut tokens = tokenize(program);
    let exp = parse_sexp(&mut tokens);
    sexp_to_expr(&exp)
}

#[test]
fn test_parse_true() {
    let raw = r#"  true"#;
    assert_eq!(
        parse(raw),
        Expr {
            source: r#"true"#,
            kind: ExprKind::Lit(LiteralKind::Bool(true))
        }
    );
}

#[test]
fn test_parse_false() {
    assert_eq!(
        parse(r#"  false"#),
        Expr {
            source: r#"false"#,
            kind: ExprKind::Lit(LiteralKind::Bool(false))
        }
    );
}

#[test]
fn test_parse_string() {
    assert_eq!(
        parse(r#""hello""#),
        Expr {
            source: r#""hello""#,
            kind: ExprKind::Lit(LiteralKind::String("hello"))
        }
    );

    assert_eq!(
        parse(r#"(+ "hello" "world")"#),
        Expr {
            source: r#"(+ "hello" "world")"#,
            kind: ExprKind::Add(
                Box::new(Expr {
                    source: r#""hello""#,
                    kind: ExprKind::Lit(LiteralKind::String("hello"))
                }),
                Box::new(Expr {
                    source: r#""world""#,
                    kind: ExprKind::Lit(LiteralKind::String("world"))
                })
            )
        }
    )
}

#[test]
fn test_parse_program() {
    assert_eq!(
        parse("(+ 1 1)"),
        Expr {
            source: "(+ 1 1)",
            kind: ExprKind::Add(
                Box::new(Expr {
                    source: "1",
                    kind: ExprKind::Lit(LiteralKind::Double(1.0))
                }),
                Box::new(Expr {
                    source: "1",
                    kind: ExprKind::Lit(LiteralKind::Double(1.0))
                })
            )
        }
    )
}

#[test]
fn test_parse_clos() {
    assert_eq!(
        parse("(lambda x (+ 1 x))"),
        Expr {
            source: "(lambda x (+ 1 x))",
            kind: ExprKind::Lambda(
                "x",
                Box::new(Expr {
                    source: "(+ 1 x)",
                    kind: ExprKind::Add(
                        Box::new(Expr {
                            source: "1",
                            kind: ExprKind::Lit(LiteralKind::Double(1.0))
                        }),
                        Box::new(Expr {
                            source: "x",
                            kind: ExprKind::Id("x")
                        })
                    )
                })
            )
        }
    )
}
#[test]
fn test_parse_id() {
    assert_eq!(
        parse("x"),
        Expr {
            source: "x",
            kind: ExprKind::Id("x")
        }
    )
}
#[test]
fn test_parse_id_uni() {
    assert_eq!(
        parse("∂x∂"),
        Expr {
            source: "∂x∂",
            kind: ExprKind::Id("∂x∂")
        }
    )
}
#[test]
fn test_parse_lit() {
    assert_eq!(
        parse("1"),
        Expr {
            source: "1",
            kind: ExprKind::Lit(LiteralKind::Double(1.0))
        }
    )
}
#[test]
fn test_parse_call() {
    assert_eq!(
        parse("(f x)"),
        Expr {
            source: "(f x)",
            kind: ExprKind::Call(
                Box::new(Expr {
                    source: "f",
                    kind: ExprKind::Id("f")
                }),
                Box::new(Expr {
                    source: "x",
                    kind: ExprKind::Id("x")
                })
            )
        }
    )
}

#[test]
fn test_tracks_locations() {
    //       012345678901234567890
    let s = "( + 17 (*  abce 6  ))";
    let mess = parse(s);

    let base = s.as_ptr() as usize;
    match mess.kind {
        ExprKind::Add(_, r) => match r.kind {
            ExprKind::Mul(l, _) => match l.kind {
                ExprKind::Id(x) => {
                    let id_loc = x.as_ptr() as usize;
                    assert_eq!(x, "abce");
                    assert_eq!(id_loc - base, 11)
                }
                _ => panic!(),
            },
            _ => panic!(),
        },
        _ => panic!(),
    }
}
