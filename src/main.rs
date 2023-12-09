use im::hashmap::HashMap;
use parse::{parse, Expr, ExprKind, LiteralKind};
use std::cmp::Ordering;
use std::ops;
use std::rc::Rc;
use typecheck::typecheck;
#[cfg(test)]
use typecheck::ConcreteType;

mod parse;
mod sexp;
mod tokenize;
mod typecheck;

type Env<'a> = HashMap<&'a str, Value<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'a> {
    Double(f64),
    Bool(bool),
    String(String),
    Clos(&'a str, Rc<Env<'a>>, Rc<Expr<'a>>),
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

fn main() {
    let y = parse("(letrec f 1 2)");

    typecheck(&y);

    println!("{:?}", y);
}

#[cfg(test)]
mod tests {
    use super::*;
    use typecheck::typecheck;

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
    fn test_eval_simple() {
        let prog = parse("(+ 1 1)");

        assert_eq!(typecheck(&prog), ConcreteType::Double);

        assert_eq!(eval(&prog, &HashMap::new()), (Value::Double(2.)))
    }

    #[test]
    fn test_eval_complex() {
        let prog = parse("(let x (lambda x (+ x 1)) (x 4))");

        assert_eq!(typecheck(&prog), ConcreteType::Double);

        assert_eq!(eval(&prog, &HashMap::new()), Value::Double(5.))
    }

    #[test]
    fn test_eval_complexer() {
        let prog = parse("((lambda x (+ x 1)) 3)");

        assert_eq!(typecheck(&prog), ConcreteType::Double);

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
    fn test_letrec() {
        let sum = parse(
            "(letrec f (lambda x 
                (if (= x 0) 
                    0
                    (+ x (f (- x 1))))) (f 5))",
        );

        assert_eq!(typecheck(&sum), ConcreteType::Double);

        assert_eq!(
            eval(&sum, &HashMap::new()),
            Value::Double(1. + 2. + 3. + 4. + 5.)
        );
    }
}
