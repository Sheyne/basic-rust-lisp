use im::hashmap::HashMap;
use std::ops;
use std::rc::Rc;
use std::str::CharIndices;

type Env<'a> = HashMap<&'a str, Value<'a>>;

#[derive(Debug, Clone, PartialEq)]
enum Expr<'a> {
    Add(Box<Expr<'a>>, Box<Expr<'a>>),
    Sub(Box<Expr<'a>>, Box<Expr<'a>>),
    Let(&'a str, Box<Expr<'a>>, Box<Expr<'a>>),
    Clos(&'a str, Box<Expr<'a>>),
    Call(Box<Expr<'a>>, Box<Expr<'a>>),
    Id(&'a str),
    Lit(f64),
}

#[derive(Debug, Clone)]
enum Value<'a> {
    Num(f64),
    Clos(&'a str, Rc<Expr<'a>>),
}

impl<'a> PartialEq for Value<'a> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Num(l) => match other {
                Value::Num(r) => l == r,
                _ => false,
            },
            _ => false,
        }
    }
}

impl<'a> ops::Add<Value<'a>> for Value<'a> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        match self {
            Value::Num(l) => match rhs {
                Value::Num(r) => Value::Num(l + r),
                _ => panic!("can only add nums"),
            },
            _ => panic!("can only add nums"),
        }
    }
}

impl<'a> ops::Sub<Value<'a>> for Value<'a> {
    type Output = Self;

    fn sub(self, rhs: Value) -> Value {
        match self {
            Value::Num(l) => match rhs {
                Value::Num(r) => Value::Num(l - r),
                _ => panic!("can only sub nums"),
            },
            _ => panic!("can only sub nums"),
        }
    }
}

fn eval<'a, 'b>(e: &Expr<'a>, env: &'b Env<'a>) -> Value<'a> {
    match e {
        Expr::Lit(x) => Value::Num(*x),
        Expr::Add(left, right) => eval(left, env) + eval(right, env),
        Expr::Sub(left, right) => eval(left, env) - eval(right, env),
        Expr::Let(id, value, expr) => eval(expr, &add_to_env(id, eval(value, env), env)),
        Expr::Id(id) => get_id(id, &env),
        Expr::Clos(id, expr) => Value::Clos(id, Rc::new(*expr.clone())),
        Expr::Call(func, arg) => {
            let argv = eval(arg, env);
            if let Value::Clos(argn, body) = eval(func, env) {
                eval(&body, &add_to_env(argn, argv, env))
            } else {
                panic!("cannot call non-closure")
            }
        }
    }
}

fn add_to_env<'a, 'b>(id: &'a str, value: Value<'a>, env: &'b Env<'a>) -> Env<'a> {
    let mut new_env = env.clone();
    new_env.insert(id, value);
    new_env
}

fn get_id<'a, 'b>(id: &str, env: &'b Env<'a>) -> Value<'a> {
    match env.get(id) {
        None => panic!("{} not declared", id),
        Some(a) => a.clone(),
    }
}

fn lit<'a>(x: f64) -> Box<Expr<'a>> {
    Box::new(Expr::Lit(x))
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
    LeftParen,
    RightParen,
    Word(&'a str),
}

struct TokenStream<'a> {
    source: &'a str,
    iter: CharIndices<'a>,
    pending: Option<(usize, char)>,
}

impl<'a> TokenStream<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some(x) = self.pending {
            self.pending = None;
            return Some(x);
        }

        self.iter.next()
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut in_progress = false;
        let mut start_idx: usize = 0;

        loop {
            match self.next_char() {
                Some((index, current)) => match current {
                    '(' => {
                        if in_progress {
                            self.pending = Some((index, current));
                            return Some(Token::Word(&self.source[start_idx..index]));
                        }
                        return Some(Token::LeftParen);
                    }
                    ')' => {
                        if in_progress {
                            self.pending = Some((index, current));
                            return Some(Token::Word(&self.source[start_idx..index]));
                        }
                        return Some(Token::RightParen);
                    }
                    ' ' | '\t' | '\r' | '\n' => {
                        if in_progress {
                            return Some(Token::Word(&self.source[start_idx..index]));
                        }
                    }
                    _ => {
                        if !in_progress {
                            start_idx = index
                        }
                        in_progress = true
                    }
                },
                None => {
                    if in_progress {
                        return Some(Token::Word(&self.source[start_idx..]));
                    } else {
                        return None;
                    }
                }
            }
        }
    }
}

fn tokenize<'a>(program: &'a str) -> TokenStream<'a> {
    TokenStream {
        source: program,
        pending: None,
        iter: program.char_indices(),
    }
}

#[derive(Debug, PartialEq)]
enum SExp<'a> {
    Leaf(&'a str),
    Branch(Vec<SExp<'a>>),
}

fn parse_sexp_branch<'a, 'b>(mut tokens: &'b mut TokenStream<'a>) -> Vec<SExp<'a>> {
    let mut res = Vec::new();

    loop {
        if let Some(token) = tokens.next() {
            match token {
                Token::LeftParen => res.push(SExp::Branch(parse_sexp_branch(&mut tokens))),
                Token::RightParen => break,
                Token::Word(a) => res.push(SExp::Leaf(a)),
            }
        } else {
            panic!("missing close paren")
        }
    }

    res
}

fn parse_sexp<'a, 'b>(mut tokens: &'b mut TokenStream<'a>) -> SExp<'a> {
    let res = match tokens.next().unwrap() {
        Token::LeftParen => SExp::Branch(parse_sexp_branch(&mut tokens)),
        Token::Word(a) => SExp::Leaf(a),
        Token::RightParen => panic!("unopened right paren"),
    };

    assert!(tokens.next() == None);
    res
}

fn sexp_to_expr<'a>(sexp: &SExp<'a>) -> Expr<'a> {
    match sexp {
        SExp::Leaf(a) => match a.parse() {
            Ok(d) => Expr::Lit(d),
            Err(_) => Expr::Id(a),
        },
        SExp::Branch(elements) => match &elements[0] {
            SExp::Leaf("+") => Expr::Add(
                Box::new(sexp_to_expr(&elements[1])),
                Box::new(sexp_to_expr(&elements[2])),
            ),
            SExp::Leaf("-") => Expr::Sub(
                Box::new(sexp_to_expr(&elements[1])),
                Box::new(sexp_to_expr(&elements[2])),
            ),
            SExp::Leaf("let") => Expr::Let(
                match &elements[1] {
                    SExp::Leaf(x) => x,
                    _ => panic!("not a name to bind"),
                },
                Box::new(sexp_to_expr(&elements[2])),
                Box::new(sexp_to_expr(&elements[3])),
            ),
            SExp::Leaf("lambda") => Expr::Clos(
                match &elements[1] {
                    SExp::Leaf(x) => x,
                    _ => panic!("not a name to bind"),
                },
                Box::new(sexp_to_expr(&elements[2])),
            ),
            a => Expr::Call(
                Box::new(sexp_to_expr(&a)),
                Box::new(sexp_to_expr(&elements[1])),
            ),
        },
    }
}

fn parse<'a>(program: &'a str) -> Expr<'a> {
    let mut tokens = tokenize(program);
    let exp = parse_sexp(&mut tokens);
    sexp_to_expr(&exp)
}

fn main() {
    let y = Expr::Add(lit(17.0), Box::new(Expr::Sub(lit(7.5), lit(7.0))));

    println!("Match value: {:?}", eval(&y, &HashMap::new()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[test]
    fn test_add() {
        assert_eq!(
            eval(&Expr::Add(lit(17.0), lit(3.2)), &HashMap::new()),
            Value::Num(20.2)
        );
    }

    #[test]
    fn test_add_sub() {
        assert_eq!(
            eval(
                &Expr::Add(lit(17.0), Box::new(Expr::Sub(lit(7.5), lit(7.0)))),
                &HashMap::new()
            ),
            Value::Num(17.5)
        );
    }

    #[test]
    fn test_lookup() {
        assert_eq!(
            eval(&Expr::Id("x"), &HashMap::unit("x", Value::Num(14.0))),
            Value::Num(14.0)
        )
    }

    #[test]
    #[should_panic(expected = "y not declared")]
    fn test_lookup_not_found() {
        assert_eq!(
            eval(&Expr::Id("y"), &HashMap::unit("x", Value::Num(14.0))),
            Value::Num(14.0)
        )
    }

    #[test]
    fn test_let() {
        assert_eq!(
            eval(
                &Expr::Let("x", lit(17.0), Box::new(Expr::Id("x"))),
                &HashMap::new()
            ),
            Value::Num(17.0)
        )
    }

    #[test]
    fn test_lambda() {
        let func = Expr::Clos("x", Box::new(Expr::Add(lit(17.0), Box::new(Expr::Id("x")))));
        if let Value::Clos(arg, body) = eval(&func, &HashMap::new()) {
            assert_eq!(arg, "x");
            assert_eq!(*body, Expr::Add(lit(17.0), Box::new(Expr::Id("x"))));
        } else {
            panic!("didn't eval to closure")
        }
    }

    #[test]
    fn test_lambda_call() {
        let func = Box::new(Expr::Clos(
            "x",
            Box::new(Expr::Add(lit(17.0), Box::new(Expr::Id("x")))),
        ));
        let arg = lit(11.0);

        assert_eq!(
            eval(&Expr::Call(func, arg), &HashMap::new()),
            Value::Num(28.0)
        )
    }

    #[test]
    fn test_tokenization() {
        let tokens: Vec<Token> = tokenize("( hello )").collect();

        assert_eq!(
            tokens,
            vec![Token::LeftParen, Token::Word("hello"), Token::RightParen]
        )
    }

    #[test]
    fn test_tokenization_no_spaces() {
        let tokens: Vec<Token> = tokenize("(hello)").collect();

        assert_eq!(
            tokens,
            vec![Token::LeftParen, Token::Word("hello"), Token::RightParen]
        )
    }

    #[test]
    fn test_parse_simple() {
        let mut tokens = tokenize("(hello)");
        let exp = parse_sexp(&mut tokens);

        assert_eq!(exp, SExp::Branch(vec![SExp::Leaf("hello")]))
    }
    #[test]
    fn test_parse_less_simple() {
        let mut tokens = tokenize("(hello(  world you   ))");
        let exp = parse_sexp(&mut tokens);

        assert_eq!(
            exp,
            SExp::Branch(vec![
                SExp::Leaf("hello"),
                SExp::Branch(vec![SExp::Leaf("world"), SExp::Leaf("you")])
            ])
        )
    }

    #[test]
    fn test_parse_complex() {
        let mut tokens = tokenize("(hello ((darkness) my (old friend (i've come))))");
        let exp = parse_sexp(&mut tokens);

        assert_eq!(
            exp,
            SExp::Branch(vec![
                SExp::Leaf("hello"),
                SExp::Branch(vec![
                    SExp::Branch(vec![SExp::Leaf("darkness"),]),
                    SExp::Leaf("my"),
                    SExp::Branch(vec![
                        SExp::Leaf("old"),
                        SExp::Leaf("friend"),
                        SExp::Branch(vec![SExp::Leaf("i've"), SExp::Leaf("come"),])
                    ])
                ])
            ])
        )
    }

    #[test]
    fn test_parse_program() {
        assert_eq!(parse("(+ 1 1)"), Expr::Add(lit(1.), lit(1.)))
    }

    #[test]
    fn test_parse_let() {
        assert_eq!(
            parse("(let x 1 x)"),
            Expr::Let("x", lit(1.), Box::new(Expr::Id("x")))
        )
    }
    #[test]
    fn test_parse_clos() {
        assert_eq!(
            parse("(lambda x (+ 1 x))"),
            Expr::Clos("x", Box::new(parse("(+ 1 x)")))
        )
    }
    #[test]
    fn test_parse_id() {
        assert_eq!(parse("x"), Expr::Id("x"))
    }
    #[test]
    fn test_parse_lit() {
        assert_eq!(parse("1"), Expr::Lit(1.))
    }
    #[test]
    fn test_parse_call() {
        assert_eq!(
            parse("(f x)"),
            Expr::Call(Box::new(Expr::Id("f")), Box::new(Expr::Id("x")))
        )
    }

    #[test]
    fn test_eval_simple() {
        let prog = parse("(+ 1 1)");
        assert_eq!(eval(&prog, &HashMap::new()), (Value::Num(2.)))
    }

    #[test]
    fn test_eval_complex() {
        let prog = parse("(let x (lambda x (+ x 1)) (x 4))");
        assert_eq!(eval(&prog, &HashMap::new()), Value::Num(5.))
    }

    #[test]
    fn test_eval_complexer() {
        let prog = parse("((lambda x (+ x 1)) 3)");
        assert_eq!(eval(&prog, &HashMap::new()), (Value::Num(4.)))
    }
}
