#[cfg(test)]
use crate::parse::parse;
use crate::parse::{Expr, ExprKind, LiteralKind};
use atomic_counter::{AtomicCounter, RelaxedCounter};
use im::hashmap::HashMap;
use std::{fmt::Debug, marker::PhantomData};

type TEnv<'symbol, 'a> = HashMap<&'a str, Type<'symbol>>;
type TypeConstraints<'symbol> = HashMap<Sym<'symbol>, Type<'symbol>>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Sym<'a>(usize, PhantomData<&'a mut &'a usize>);

impl<'a> Debug for Sym<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Sym").field(&self.0).finish()
    }
}

#[derive(Default)]
struct SymbolGenerator(RelaxedCounter);

impl SymbolGenerator {
    fn get(&self) -> Sym {
        Sym(self.0.inc(), Default::default())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Type<'a> {
    Var(Sym<'a>),
    Double,
    Bool,
    String,
    Clos(Box<Type<'a>>, Box<Type<'a>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConcreteType {
    Double,
    Bool,
    String,
    Clos(Box<ConcreteType>, Box<ConcreteType>),
}

impl<'a> TryFrom<Type<'a>> for ConcreteType {
    type Error = ();

    fn try_from(value: Type<'a>) -> Result<Self, Self::Error> {
        match value {
            Type::Var(_) => Err(()),
            Type::Double => Ok(ConcreteType::Double),
            Type::Bool => Ok(ConcreteType::Bool),
            Type::String => Ok(ConcreteType::String),
            Type::Clos(a, b) => Ok(ConcreteType::Clos(
                Box::new(TryInto::<ConcreteType>::try_into(*a)?),
                Box::new(TryInto::<ConcreteType>::try_into(*b)?),
            )),
        }
    }
}

fn assert_type<'symbol>(
    source: Type<'symbol>,
    expected: Type<'symbol>,
    constraints: TypeConstraints<'symbol>,
    depth: i32,
) -> TypeConstraints<'symbol> {
    if depth > 1000 {
        panic!("Possibly infinite type")
    }
    if source != expected {
        match &source {
            Type::Var(left_sym) => {
                if let Type::Var(right_sym) = &expected {
                    match (constraints.get(left_sym), constraints.get(right_sym)) {
                        (None, None) => {
                            let mut new: HashMap<Sym<'_>, Type<'_>> = constraints.clone();
                            new.insert(*left_sym, expected.clone());
                            new.insert(*right_sym, source.clone());
                            new
                        }
                        (Some(left_existing), None) => {
                            let mut new: HashMap<Sym<'_>, Type<'_>> = constraints.clone();
                            new.insert(*right_sym, left_existing.clone());
                            new
                        }
                        (None, Some(right_existing)) => {
                            let mut new: HashMap<Sym<'_>, Type<'_>> = constraints.clone();
                            new.insert(*left_sym, right_existing.clone());
                            new
                        }
                        (Some(left_existing), Some(right_existing)) => assert_type(
                            left_existing.clone(),
                            right_existing.clone(),
                            constraints,
                            depth + 1,
                        ),
                    }
                } else {
                    if let Some(existing) = constraints.get(left_sym) {
                        assert_type(existing.clone(), expected, constraints, depth + 1)
                    } else {
                        let mut new: HashMap<Sym<'_>, Type<'_>> = constraints.clone();
                        new.insert(*left_sym, expected.clone());
                        new
                    }
                }
            }
            Type::Clos(arg_s, res_s) => match &expected {
                Type::Var(_) => assert_type(expected, source, constraints, depth + 1),
                Type::Clos(arg_e, res_e) => {
                    let constraints =
                        assert_type(*arg_s.clone(), *arg_e.clone(), constraints, depth + 1);
                    assert_type(*res_s.clone(), *res_e.clone(), constraints, depth + 1)
                }
                _ => panic!("Unresolvable types {:?} != {:?}", source, expected),
            },
            _ => match &expected {
                Type::Var(_) => assert_type(expected, source, constraints, depth + 1),
                _ => panic!("Unresolvable types {:?} != {:?}", source, expected),
            },
        }
    } else {
        constraints
    }
}

fn lookup<'symbol>(
    constraints: TypeConstraints<'symbol>,
    t: Type<'symbol>,
    depth: i32,
) -> (TypeConstraints<'symbol>, Type<'symbol>) {
    if depth > 1000 {
        panic!("Possibly infinite type")
    }
    match t {
        Type::Var(s) => {
            if let Some(t) = constraints.get(&s) {
                lookup(constraints.clone(), t.clone(), depth + 1)
            } else {
                (constraints, t)
            }
        }
        Type::Clos(a, b) => {
            let (constraints, a) = lookup(constraints, *a, depth + 1);
            let (constraints, b) = lookup(constraints, *b, depth + 1);
            (constraints, Type::Clos(Box::new(a), Box::new(b)))
        }
        _ => (constraints, t),
    }
}

#[derive(Default)]
struct Typechecker {
    gensym: SymbolGenerator,
}

impl Typechecker {
    fn typecheck<'me, 'a, 'b>(
        &'me self,
        e: &Expr<'a>,
        env: &'b TEnv<'me, 'a>,
        constraints: TypeConstraints<'me>,
    ) -> (TypeConstraints<'me>, Type<'me>) {
        let constrain_binary = |left: &Expr<'a>,
                                right: &Expr<'a>,
                                t_in: Type<'me>,
                                t_out: Type<'me>,
                                constraints: TypeConstraints<'me>|
         -> (TypeConstraints<'me>, Type<'me>) {
            let (constraints, left_t) = self.typecheck(left, env, constraints);
            let constraints = assert_type(left_t, t_in.clone(), constraints, 0);
            let (constraints, right_t) = self.typecheck(right, env, constraints);
            let constraints = assert_type(right_t, t_in, constraints, 0);
            lookup(constraints, t_out, 0)
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
                let (constraints, t) = self.typecheck(left, env, constraints);
                lookup(assert_type(t, Type::Bool, constraints, 0), Type::Bool, 0)
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
                let (constraints, cond_t) = self.typecheck(cond, env, constraints);
                let constraints = assert_type(cond_t, Type::Bool, constraints, 0);
                let (constraints, t_t) = self.typecheck(t, env, constraints);
                let (constraints, f_t) = self.typecheck(f, env, constraints);
                let constraints = assert_type(t_t.clone(), f_t, constraints, 0);
                lookup(constraints, t_t, 0)
            }
            ExprKind::Lambda(id, expr) => {
                let arg_sym = self.gensym.get();
                let arg = Type::Var(arg_sym);
                let mut new_env = env.clone();
                new_env.insert(id, arg.clone());
                let (constraints, body_t) = self.typecheck(expr, &new_env, constraints);

                if let Some(x) = constraints.get(&arg_sym) {
                    println!("Binding {:?} = {:?}", &arg_sym, x);
                } else {
                    println!("Binding {:?} = None", &arg_sym);
                }

                lookup(constraints, Type::Clos(Box::new(arg), Box::new(body_t)), 0)
            }
            ExprKind::Call(func, arg) => {
                let (constraints, func_t) = self.typecheck(func, env, constraints);
                let (constraints, arg_t) = self.typecheck(arg, env, constraints);
                let res_t = Type::Var(self.gensym.get());
                let constraints = assert_type(
                    func_t,
                    Type::Clos(Box::new(arg_t), Box::new(res_t.clone())),
                    constraints,
                    0,
                );
                lookup(constraints, res_t, 0)
            }
            ExprKind::Id(x) => (constraints, env.get(x).unwrap().clone()),
        }
    }
}

pub fn typecheck<'a, 'b>(e: &Expr<'a>) -> ConcreteType {
    let checker = Typechecker::default();
    let (_, typ) = checker.typecheck(e, &Default::default(), Default::default());

    typ.try_into().unwrap()
}

#[test]
fn test_eval_simple() {
    let prog = parse("(+ 1 1)");

    assert_eq!(typecheck(&prog), ConcreteType::Double);
}

#[test]
fn test_eval_complex() {
    let prog = parse("(let x (lambda x (+ x 1)) (x 4))");

    assert_eq!(typecheck(&prog), ConcreteType::Double);
}

#[test]
fn test_eval_complexer() {
    let prog = parse("((lambda x (+ x 1)) 3)");

    assert_eq!(typecheck(&prog), ConcreteType::Double);
}

#[test]
fn test_eval_complexer_2() {
    let prog = parse("(if (= 10 ((lambda x (+ x 1)) 3)) 7 1)");

    assert_eq!(typecheck(&prog), ConcreteType::Double);
}

#[test]
#[should_panic(expected = "Possibly infinite type")]
fn test_letrec() {
    let sum = parse(
        "(letrec f (lambda x 
            (if (= x 0) 
                0
                (+ x (f (- x 1))))) (f 5))",
    );

    assert_eq!(typecheck(&sum), ConcreteType::Double);
}
