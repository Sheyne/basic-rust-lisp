use atomic_counter::{AtomicCounter, RelaxedCounter};
use im::hashmap::HashMap;
use std::cmp::Ordering;
use std::ops;
use std::rc::Rc;
use std::str::CharIndices;

#[macro_use]
extern crate lazy_static;

type Env<'a> = HashMap<&'a str, Value<'a>>;
type TEnv<'a> = HashMap<&'a str, Type>;
type TypeConstraints = HashMap<Sym, Type>;

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
    source: &'a str,
    kind: ExprKind<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Double(f64),
    Bool(bool),
    String(String),
    Clos(&'a str, Rc<Env<'a>>, Rc<Expr<'a>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Sym(usize);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Var(Sym),
    Poly,
    Double,
    Bool,
    String,
    Clos(Box<Type>, Box<Type>),
}

impl Sym {
    fn new() -> Sym {
        lazy_static! {
            static ref C: RelaxedCounter = RelaxedCounter::new(0);
        }
        Sym(C.inc())
    }
}

impl<'a> PartialOrd for Value<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Value::Double(l) => match other {
                Value::Double(r) => l.partial_cmp(r),
                _ => None,
            },
            Value::Bool(l) => match other {
                Value::Bool(r) => l.partial_cmp(r),
                _ => None,
            },
            Value::String(l) => match other {
                Value::String(r) => l.partial_cmp(r),
                _ => None,
            },
            _ => None,
        }
    }
}

impl<'a> ops::Add<Value<'a>> for Value<'a> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        match self {
            Value::Double(l) => match rhs {
                Value::Double(r) => Value::Double(l + r),
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
            Value::Double(l) => match rhs {
                Value::Double(r) => Value::Double(l - r),
                _ => panic!("can only sub nums"),
            },
            _ => panic!("can only sub nums"),
        }
    }
}
impl<'a> ops::Div<Value<'a>> for Value<'a> {
    type Output = Self;

    fn div(self, rhs: Value) -> Value {
        match self {
            Value::Double(l) => match rhs {
                Value::Double(r) => Value::Double(l / r),
                _ => panic!("can only sub nums"),
            },
            _ => panic!("can only sub nums"),
        }
    }
}
impl<'a> ops::Mul<Value<'a>> for Value<'a> {
    type Output = Self;

    fn mul(self, rhs: Value) -> Value {
        match self {
            Value::Double(l) => match rhs {
                Value::Double(r) => Value::Double(l * r),
                _ => panic!("can only sub nums"),
            },
            _ => panic!("can only sub nums"),
        }
    }
}

fn assert_type(source: Type, expected: Type, constraints: TypeConstraints) -> TypeConstraints {
    if source != expected {
        match &source {
            Type::Var(s) => {
                let mut new = constraints.clone();
                if let Some(old) = new.get(s) {
                    panic!("already has {:?}: {:?} -> {:?}", s, old, expected);
                };

                new.insert(*s, expected);
                new
            }
            Type::Clos(arg_s, res_s) => match &expected {
                Type::Var(_) => assert_type(expected, source, constraints),
                Type::Clos(arg_e, res_e) => {
                    let constraints = assert_type(*arg_s.clone(), *arg_e.clone(), constraints);
                    assert_type(*res_s.clone(), *res_e.clone(), constraints)
                }
                _ => panic!("Unresolvable types {:?} != {:?}", source, expected),
            },
            _ => match &expected {
                Type::Var(_) => assert_type(expected, source, constraints),
                _ => panic!("Unresolvable types {:?} != {:?}", source, expected),
            },
        }
    } else {
        constraints
    }
}

fn lookup(constraints: TypeConstraints, t: Type) -> (TypeConstraints, Type) {
    match t {
        Type::Var(s) => {
            let t = if let Some(t) = constraints.get(&s) {
                t.clone()
            } else {
                t
            };
            (constraints, t)
        }
        Type::Clos(a, b) => {
            let (constraints, a) = lookup(constraints, *a);
            let (constraints, b) = lookup(constraints, *b);
            (constraints, Type::Clos(Box::new(a), Box::new(b)))
        }
        _ => (constraints, t),
    }
}

pub fn typecheck<'a, 'b>(
    e: &Expr<'a>,
    env: &'b TEnv<'a>,
    constraints: TypeConstraints,
) -> (TypeConstraints, Type) {
    let constrain_binary = |left: &Expr<'a>,
                            right: &Expr<'a>,
                            t_in: Type,
                            t_out: Type,
                            constraints: TypeConstraints|
     -> (TypeConstraints, Type) {
        let (constraints, left_t) = typecheck(left, env, constraints);
        let constraints = assert_type(left_t, t_in.clone(), constraints);
        let (constraints, right_t) = typecheck(right, env, constraints);
        let constraints = assert_type(right_t, t_in, constraints);
        lookup(constraints, t_out)
    };

    match &e.kind {
        ExprKind::Add(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Double, constraints)
        }
        ExprKind::Sub(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Double, constraints)
        }
        ExprKind::Mul(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Double, constraints)
        }
        ExprKind::Div(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Double, constraints)
        }
        ExprKind::Lt(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Bool, constraints)
        }
        ExprKind::Gt(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Bool, constraints)
        }
        ExprKind::Eq(left, right) => {
            constrain_binary(left, right, Type::Double, Type::Bool, constraints)
        }
        ExprKind::Not(left) => {
            let (constraints, t) = typecheck(left, env, constraints);
            lookup(assert_type(t, Type::Bool, constraints), Type::Bool)
        }
        ExprKind::Lit(x) => match x {
            LiteralKind::Double(_) => (constraints, Type::Double),
            LiteralKind::Bool(_) => (constraints, Type::Bool),
            LiteralKind::String(_) => (constraints, Type::String),
        },
        ExprKind::Concat(left, right) => {
            constrain_binary(left, right, Type::String, Type::String, constraints)
        }
        ExprKind::If(cond, t, f) => {
            let (constraints, cond_t) = typecheck(cond, env, constraints);
            let constraints = assert_type(cond_t, Type::Bool, constraints);
            let (constraints, t_t) = typecheck(t, env, constraints);
            let (constraints, f_t) = typecheck(f, env, constraints);
            let constraints = assert_type(t_t.clone(), f_t, constraints);
            lookup(constraints, t_t)
        }
        ExprKind::Lambda(id, expr) => {
            let arg_sym = Sym::new();
            let arg = Type::Var(arg_sym);
            let mut new_env = env.clone();
            new_env.insert(id, arg.clone());
            let (constraints, body_t) = typecheck(expr, &new_env, constraints);

            if let Some(x) = constraints.get(&arg_sym) {
                println!("Binding {:?} = {:?}", &arg_sym, x);
            } else {
                println!("Binding {:?} = None", &arg_sym);
            }

            lookup(constraints, Type::Clos(Box::new(arg), Box::new(body_t)))
        }
        ExprKind::Call(func, arg) => {
            let (constraints, func_t) = typecheck(func, env, constraints);
            let (constraints, arg_t) = typecheck(arg, env, constraints);
            let res_t = Type::Var(Sym::new());
            let constraints = assert_type(
                func_t,
                Type::Clos(Box::new(arg_t), Box::new(res_t.clone())),
                constraints,
            );
            lookup(constraints, res_t)
        }
        ExprKind::Id(x) => (constraints, env.get(x).unwrap().clone()),
    }
}

pub fn eval<'a, 'b>(e: &Expr<'a>, env: &'b Env<'a>) -> Value<'a> {
    match &e.kind {
        ExprKind::Lit(x) => match x {
            LiteralKind::Double(d) => Value::Double(*d),
            LiteralKind::Bool(b) => Value::Bool(*b),
            LiteralKind::String(b) => Value::String(String::from(*b)),
        },
        ExprKind::Not(v) => Value::Bool(!match eval(v, env) {
            Value::Bool(b) => b,
            _ => panic!("not a bool"),
        }),
        ExprKind::Lt(left, right) => Value::Bool(eval(left, env) < eval(right, env)),
        ExprKind::Gt(left, right) => Value::Bool(eval(left, env) > eval(right, env)),
        ExprKind::Eq(left, right) => Value::Bool(eval(left, env) == eval(right, env)),
        ExprKind::Add(left, right) => eval(left, env) + eval(right, env),
        ExprKind::Sub(left, right) => eval(left, env) - eval(right, env),
        ExprKind::Mul(left, right) => eval(left, env) * eval(right, env),
        ExprKind::Div(left, right) => eval(left, env) / eval(right, env),
        ExprKind::Concat(left, right) => match eval(left, env) {
            Value::String(l) => match eval(right, env) {
                Value::String(r) => Value::String(format!("{}{}", l, r)),
                _ => panic!("not a string"),
            },
            _ => panic!("not a string"),
        },
        ExprKind::If(cond, t, f) => {
            if let Value::Bool(b) = eval(cond, env) {
                if b {
                    eval(t, env)
                } else {
                    eval(f, env)
                }
            } else {
                panic!("not a boolean")
            }
        }
        ExprKind::Id(id) => get_id(id, &env),
        ExprKind::Lambda(id, expr) => Value::Clos(id, Rc::new(env.clone()), Rc::new(*expr.clone())),
        ExprKind::Call(func, arg) => {
            let argv = eval(arg, env);
            if let Value::Clos(argn, lex_env, body) = eval(func, env) {
                eval(&body, &add_to_env(argn, argv, &*lex_env))
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

#[derive(Debug, PartialEq)]
enum Token {
    LeftParen(usize),
    RightParen(usize),
    String(usize, usize),
    Word(usize, usize),
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
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut in_progress = false;
        let mut start_idx: usize = 0;

        let mut string_in_progress = false;

        loop {
            match self.next_char() {
                Some((index, current)) => {
                    if string_in_progress {
                        if current == '"' {
                            return Some(Token::String(start_idx, index + 1));
                        }
                    } else {
                        match current {
                            '(' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                return Some(Token::LeftParen(index));
                            }
                            ')' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                return Some(Token::RightParen(index));
                            }
                            '"' => {
                                if in_progress {
                                    self.pending = Some((index, current));
                                    return Some(Token::Word(start_idx, index));
                                }
                                string_in_progress = true;
                                start_idx = index;
                            }
                            ' ' | '\t' | '\r' | '\n' => {
                                if in_progress {
                                    return Some(Token::Word(start_idx, index));
                                }
                            }
                            _ => {
                                if !in_progress {
                                    start_idx = index
                                }
                                in_progress = true
                            }
                        }
                    }
                }
                None => {
                    if in_progress {
                        return Some(Token::Word(start_idx, self.source.len()));
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
                    res.push(SExp::Branch(&tokens.source[idx..last + 1], child))
                }
                Token::String(left, right) => {
                    res.push(SExp::StringLeaf(&tokens.source[left..right]))
                }
                Token::Word(left, right) => res.push(SExp::Leaf(&tokens.source[left..right])),
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

fn parse_sexp<'a, 'b>(mut tokens: &'b mut TokenStream<'a>) -> SExp<'a> {
    let res = match tokens.next().unwrap() {
        Token::LeftParen(idx) => {
            let (last, child) = parse_sexp_branch(&mut tokens);
            SExp::Branch(&tokens.source[idx..last + 1], child)
        }
        Token::Word(left, right) => SExp::Leaf(&tokens.source[left..right]),
        Token::String(left, right) => SExp::StringLeaf(&tokens.source[left..right]),
        Token::RightParen(_) => panic!("unopened right paren"),
    };

    assert!(tokens.next() == None);
    res
}

fn sexp_to_expr<'a>(sexp: &SExp<'a>) -> Expr<'a> {
    match sexp {
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
            kind: ExprKind::Lit(LiteralKind::String(s)),
        },
        SExp::Branch(source, elements) => match &elements[0] {
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
                lazy_static! {
                    static ref Y: Expr<'static> =
                        parse("(lambda f ((lambda x (x x)) (lambda x (f (lambda y ((x x) y))))))");
                }
                let n = match &elements[1] {
                    SExp::Leaf(x) => x,
                    _ => panic!("not a name to bind"),
                };
                let call_y = ExprKind::Call(
                    Box::new(Y.clone()),
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

fn main() {
    let y = parse("(letrec f 1 2)");

    println!("{:?}", y);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn not<'a>(l: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Not(Box::new(l)),
        }
    }
    fn eq<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Eq(Box::new(l), Box::new(r)),
        }
    }
    fn lt<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Lt(Box::new(l), Box::new(r)),
        }
    }
    fn gt<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Gt(Box::new(l), Box::new(r)),
        }
    }
    fn add<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Add(Box::new(l), Box::new(r)),
        }
    }
    fn sub<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Sub(Box::new(l), Box::new(r)),
        }
    }
    fn mul<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Mul(Box::new(l), Box::new(r)),
        }
    }
    fn div<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Div(Box::new(l), Box::new(r)),
        }
    }
    fn concat<'a>(l: Expr<'a>, r: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Concat(Box::new(l), Box::new(r)),
        }
    }

    fn double_v<'a>(x: f64) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Lit(LiteralKind::Double(x)),
        }
    }
    fn bool_v<'a>(x: bool) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Lit(LiteralKind::Bool(x)),
        }
    }
    fn string_v<'a>(x: &'a str) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Lit(LiteralKind::String(x)),
        }
    }
    fn id<'a>(x: &'a str) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Id(x),
        }
    }
    fn lambda<'a>(x: &'a str, e: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Lambda(x, Box::new(e)),
        }
    }
    fn call<'a>(a: Expr<'a>, b: Expr<'a>) -> Expr<'a> {
        Expr {
            source: "",
            kind: ExprKind::Call(Box::new(a), Box::new(b)),
        }
    }

    #[test]
    fn test_add() {
        assert_eq!(
            eval(&add(double_v(17.0), double_v(3.2)), &HashMap::new()),
            Value::Double(20.2)
        );
    }

    #[test]
    fn test_add_sub() {
        assert_eq!(
            eval(
                &add(double_v(17.0), sub(double_v(7.5), double_v(7.0))),
                &HashMap::new()
            ),
            Value::Double(17.5)
        );
    }

    #[test]
    fn test_concat() {
        assert_eq!(
            eval(&concat(string_v("1"), string_v("2")), &HashMap::new()),
            Value::String("12".to_string())
        );
    }

    #[test]
    fn test_compare_str() {
        assert_eq!(
            eval(&eq(string_v("x"), string_v("x")), &HashMap::new()),
            Value::Bool(true)
        );
        assert_eq!(
            eval(&eq(string_v("x"), string_v("y")), &HashMap::new()),
            Value::Bool(false)
        );
    }

    #[test]
    fn test_lookup() {
        let env = HashMap::unit("x", Value::Double(14.0));

        assert_eq!(eval(&id("x"), &env), Value::Double(14.0))
    }

    #[test]
    #[should_panic(expected = "y not declared")]
    fn test_lookup_not_found() {
        let env = HashMap::unit("x", Value::Double(14.0));

        assert_eq!(eval(&id("y"), &env), Value::Double(14.0))
    }

    #[test]
    fn test_lambda() {
        let func = lambda("x", add(double_v(17.0), id("x")));
        assert_eq!(
            Value::Clos(
                "x",
                Default::default(),
                Rc::new(add(double_v(17.0), id("x")))
            ),
            eval(&func, &HashMap::new())
        );
    }

    #[test]
    fn test_math() {
        assert_eq!(
            eval(&add(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Double(3. + 5.)
        );
        assert_eq!(
            eval(&sub(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Double(3. - 5.)
        );
        assert_eq!(
            eval(&mul(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Double(3. * 5.)
        );
        assert_eq!(
            eval(&div(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Double(3. / 5.)
        );
    }

    #[test]
    fn test_logic() {
        assert_eq!(
            eval(&eq(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Bool(false),
            "(= 3 5)"
        );
        assert_eq!(
            eval(&eq(double_v(3.), double_v(3.)), &HashMap::new()),
            Value::Bool(true),
            "(= 3 3)"
        );
        assert_eq!(
            eval(&lt(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Bool(true),
            "(< 3 5)"
        );
        assert_eq!(
            eval(&lt(double_v(5.), double_v(3.)), &HashMap::new()),
            Value::Bool(false),
            "(< 5 3)"
        );
        assert_eq!(
            eval(&gt(double_v(3.), double_v(5.)), &HashMap::new()),
            Value::Bool(false),
            "(> 3 5)"
        );
        assert_eq!(
            eval(&gt(double_v(5.), double_v(3.)), &HashMap::new()),
            Value::Bool(true),
            "(> 5 3)"
        );
        assert_eq!(
            eval(&not(bool_v(false)), &HashMap::new()),
            Value::Bool(true),
            "(! false)"
        );
        assert_eq!(
            eval(&not(bool_v(true)), &HashMap::new()),
            Value::Bool(false),
            "(! true)"
        );
    }

    #[test]
    fn test_lambda_call() {
        let func = lambda("x", add(double_v(17.0), id("x")));
        let arg = double_v(11.0);

        assert_eq!(eval(&call(func, arg), &HashMap::new()), Value::Double(28.0))
    }

    #[test]
    fn test_tokenization() {
        let tokens: Vec<Token> = tokenize("( hello )").collect();

        assert_eq!(
            tokens,
            vec![
                Token::LeftParen(0),
                Token::Word(2, 2 + 5),
                Token::RightParen(8)
            ]
        )
    }

    #[test]
    fn test_tokenization_no_spaces() {
        let tokens: Vec<Token> = tokenize("(hello)").collect();

        assert_eq!(
            tokens,
            vec![Token::LeftParen(0), Token::Word(1, 6), Token::RightParen(6)]
        )
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

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse(r#""hello""#),
            Expr {
                source: r#""hello""#,
                kind: ExprKind::Lit(LiteralKind::String(r#""hello""#))
            }
        );

        assert_eq!(
            parse(r#"(+ "hello" "world")"#),
            Expr {
                source: r#"(+ "hello" "world")"#,
                kind: ExprKind::Add(
                    Box::new(Expr {
                        source: r#""hello""#,
                        kind: ExprKind::Lit(LiteralKind::String(r#""hello""#))
                    }),
                    Box::new(Expr {
                        source: r#""world""#,
                        kind: ExprKind::Lit(LiteralKind::String(r#""world""#))
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
    fn test_eval_simple() {
        let prog = parse("(+ 1 1)");

        assert_eq!(
            typecheck(&prog, &HashMap::new(), HashMap::new()),
            (HashMap::new(), Type::Double)
        );

        assert_eq!(eval(&prog, &HashMap::new()), (Value::Double(2.)))
    }

    #[test]
    fn test_eval_complex() {
        let prog = parse("(let x (lambda x (+ x 1)) (x 4))");

        assert_eq!(
            typecheck(&prog, &HashMap::new(), HashMap::new()),
            (HashMap::new(), Type::Double)
        );

        assert_eq!(eval(&prog, &HashMap::new()), Value::Double(5.))
    }

    #[test]
    fn test_eval_complexer() {
        let prog = parse("((lambda x (+ x 1)) 3)");

        assert_eq!(
            typecheck(&prog, &HashMap::new(), HashMap::new()),
            (HashMap::new(), Type::Double)
        );

        assert_eq!(eval(&prog, &HashMap::new()), (Value::Double(4.)))
    }

    #[test]
    fn test_if() {
        assert!(eval(&parse("(if (< 0 1) 1 2)"), &HashMap::new()) == Value::Double(1.));
        assert!(eval(&parse("(if (< 1 0) 1 2)"), &HashMap::new()) == Value::Double(2.));
    }

    #[test]
    fn test_nesting() {
        let nested = parse("(((lambda y (lambda x (+ x y))) 1) 2)");
        assert_eq!(eval(&nested, &HashMap::new()), Value::Double(3.0))
    }

    #[test]
    fn test_y_combin() {
        let y = parse("(lambda f ((lambda x (x x)) (lambda x (f (lambda y ((x x) y))))))");

        let sum = parse(
            "(lambda f (lambda x 
                (if (= x 0) 
                    0
                    (+ x (f (- x 1))))))",
        );

        let call_with_3 = call(call(y, sum), double_v(3.0));

        assert_eq!(eval(&call_with_3, &HashMap::new()), Value::Double(6.0));
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

    #[test]
    fn test_letrec() {
        let sum = parse(
            "(letrec f (lambda x 
                (if (= x 0) 
                    0
                    (+ x (f (- x 1))))) (f 5))",
        );

        assert_eq!(
            typecheck(&sum, &HashMap::new(), HashMap::new()),
            (HashMap::new(), Type::Double)
        );

        assert_eq!(
            eval(&sum, &HashMap::new()),
            Value::Double(1. + 2. + 3. + 4. + 5.)
        );
    }
}
