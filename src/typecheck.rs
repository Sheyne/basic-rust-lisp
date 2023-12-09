#[cfg(test)]
use crate::parse::parse;
use crate::parse::{Expr, ExprKind, LiteralKind};
use atomic_counter::{AtomicCounter, RelaxedCounter};
use im::hashmap::HashMap;
use std::sync::OnceLock;

pub type TEnv<'a> = HashMap<&'a str, Type>;
pub type TypeConstraints = HashMap<Sym, Type>;

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
        static COUNTER: OnceLock<RelaxedCounter> = OnceLock::new();
        Sym(COUNTER.get_or_init(|| RelaxedCounter::new(0)).inc())
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

#[test]
fn test_eval_simple() {
    let prog = parse("(+ 1 1)");

    assert_eq!(
        typecheck(&prog, &HashMap::new(), HashMap::new()),
        (HashMap::new(), Type::Double)
    );
}

#[test]
fn test_eval_complex() {
    let prog = parse("(let x (lambda x (+ x 1)) (x 4))");

    assert_eq!(
        typecheck(&prog, &HashMap::new(), HashMap::new()),
        (HashMap::new(), Type::Double)
    );
}

#[test]
fn test_eval_complexer() {
    let prog = parse("((lambda x (+ x 1)) 3)");

    assert_eq!(
        typecheck(&prog, &HashMap::new(), HashMap::new()),
        (HashMap::new(), Type::Double)
    );
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
}
