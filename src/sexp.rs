use crate::tokenize::{tokenize, Token, TokenStream};

#[derive(Debug, PartialEq)]
pub enum SExp<'a> {
    Leaf(&'a str),
    StringLeaf(&'a str),
    Branch(&'a str, Vec<SExp<'a>>),
}

fn parse_sexp_branch<'a, 'b>(mut tokens: &'b mut TokenStream<'a>) -> (usize, Vec<SExp<'a>>) {
    let mut res = Vec::new();
    let last_index: usize;
    loop {
        if let Some(token) = tokens.next() {
            match token {
                Token::LeftParen(idx) => {
                    let (last, child) = parse_sexp_branch(&mut tokens);
                    res.push(SExp::Branch(tokens.source_range(idx, last + 1), child))
                }
                Token::String(left, right) => {
                    res.push(SExp::StringLeaf(tokens.source_range(left, right)))
                }
                Token::Word(left, right) => res.push(SExp::Leaf(tokens.source_range(left, right))),
                Token::RightParen(idx) => {
                    last_index = idx;
                    break;
                }
            }
        } else {
            panic!("missing close paren")
        }
    }

    (last_index, res)
}

pub fn parse_sexp<'a, 'b>(mut tokens: &'b mut TokenStream<'a>) -> SExp<'a> {
    let res = match tokens.next().unwrap() {
        Token::LeftParen(idx) => {
            let (last, child) = parse_sexp_branch(&mut tokens);
            SExp::Branch(tokens.source_range(idx, last + 1), child)
        }
        Token::Word(left, right) => SExp::Leaf(tokens.source_range(left, right)),
        Token::String(left, right) => SExp::StringLeaf(tokens.source_range(left, right)),
        Token::RightParen(_) => panic!("unopened right paren"),
    };

    assert!(tokens.next() == None);
    res
}

#[test]
fn test_parse_simple() {
    let mut tokens = tokenize("(hello)");
    let exp = parse_sexp(&mut tokens);

    assert_eq!(exp, SExp::Branch("(hello)", vec![SExp::Leaf("hello")]))
}
#[test]
fn test_parse_less_simple() {
    let mut tokens = tokenize("(hello(  world you   ))");
    let exp = parse_sexp(&mut tokens);

    assert_eq!(
        exp,
        SExp::Branch(
            "(hello(  world you   ))",
            vec![
                SExp::Leaf("hello"),
                SExp::Branch(
                    "(  world you   )",
                    vec![SExp::Leaf("world"), SExp::Leaf("you")]
                )
            ]
        )
    )
}

#[test]
fn test_parse_with_string() {
    let mut tokens = tokenize(r#"(hello("a"  world "are"you   )"boy")"#);
    let exp = parse_sexp(&mut tokens);

    assert_eq!(
        exp,
        SExp::Branch(
            r#"(hello("a"  world "are"you   )"boy")"#,
            vec![
                SExp::Leaf("hello"),
                SExp::Branch(
                    r#"("a"  world "are"you   )"#,
                    vec![
                        SExp::StringLeaf(r#""a""#),
                        SExp::Leaf("world"),
                        SExp::StringLeaf(r#""are""#),
                        SExp::Leaf("you")
                    ]
                ),
                SExp::StringLeaf(r#""boy""#)
            ]
        )
    )
}

#[test]
fn test_parse_complex() {
    let mut tokens = tokenize("(hello ((darkness) my (old friend (i've come))))");
    let exp = parse_sexp(&mut tokens);

    assert_eq!(
        exp,
        SExp::Branch(
            "(hello ((darkness) my (old friend (i've come))))",
            vec![
                SExp::Leaf("hello"),
                SExp::Branch(
                    "((darkness) my (old friend (i've come)))",
                    vec![
                        SExp::Branch("(darkness)", vec![SExp::Leaf("darkness"),]),
                        SExp::Leaf("my"),
                        SExp::Branch(
                            "(old friend (i've come))",
                            vec![
                                SExp::Leaf("old"),
                                SExp::Leaf("friend"),
                                SExp::Branch(
                                    "(i've come)",
                                    vec![SExp::Leaf("i've"), SExp::Leaf("come"),]
                                )
                            ]
                        )
                    ]
                )
            ]
        )
    )
}
