#[cfg(test)]
use crate::parse::parse;
use crate::parse::{Expr, ExprKind, LiteralKind};
use atomic_counter::{AtomicCounter, RelaxedCounter};
use im::hashmap::HashMap;
use std::{fmt::Debug, marker::PhantomData};

type TEnv<'expr, 'symbol> = HashMap<&'expr str, Type<'expr, 'symbol>>;
type TypeConstraints<'expr, 'symbol> = HashMap<Sym<'symbol>, Type<'expr, 'symbol>>;

#[derive(Debug, PartialEq)]
pub enum TypeError<'a> {
    UnresolvableTypes(Expr<'a>, ConcreteType),
    InfiniteType,
}

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

#[derive(Debug, Clone)]
enum TypeOption<'source, 'a> {
    Var(Sym<'a>),
    Double,
    Bool,
    String,
    Clos(Box<Type<'source, 'a>>, Box<Type<'source, 'a>>),
}

impl<'source, 'a> PartialEq for TypeOption<'source, 'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Var(l0), Self::Var(r0)) => l0 == r0,
            (Self::Clos(l0, l1), Self::Clos(r0, r1)) => l0.typ == r0.typ && l1.typ == r1.typ,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl<'source, 'a> PartialEq for Type<'source, 'a> {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConcreteType {
    Double,
    Bool,
    String,
    Clos(Box<ConcreteType>, Box<ConcreteType>),
}

#[derive(Debug, Clone)]
struct TypedExpr<'a, T> {
    expr: Expr<'a>,
    typ: T,
}

impl<'source, 'a> TryFrom<TypeOption<'source, 'a>> for ConcreteType {
    type Error = ();

    fn try_from(value: TypeOption<'source, 'a>) -> Result<Self, Self::Error> {
        match value {
            TypeOption::Var(_) => Err(()),
            TypeOption::Double => Ok(ConcreteType::Double),
            TypeOption::Bool => Ok(ConcreteType::Bool),
            TypeOption::String => Ok(ConcreteType::String),
            TypeOption::Clos(a, b) => Ok(ConcreteType::Clos(
                Box::new(TryInto::<ConcreteType>::try_into(a.typ.clone())?),
                Box::new(TryInto::<ConcreteType>::try_into(b.typ.clone())?),
            )),
        }
    }
}

type Type<'a, 'b> = TypedExpr<'a, TypeOption<'a, 'b>>;

fn assert_type<'symbol, 'expr>(
    source: Type<'expr, 'symbol>,
    expected: Type<'expr, 'symbol>,
    constraints: TypeConstraints<'expr, 'symbol>,
    depth: i32,
) -> Result<TypeConstraints<'expr, 'symbol>, TypeError<'expr>> {
    if depth > 1000 {
        Err(TypeError::InfiniteType)?
    }
    Ok(if source != expected {
        match &source.typ {
            TypeOption::Var(left_sym) => {
                if let TypeOption::Var(right_sym) = &expected.typ {
                    match (constraints.get(left_sym), constraints.get(right_sym)) {
                        (None, None) => {
                            let mut new: HashMap<Sym<'_>, Type<'_, '_>> = constraints.clone();
                            new.insert(*left_sym, expected.clone());
                            new.insert(*right_sym, source.clone());
                            new
                        }
                        (Some(left_existing), None) => {
                            let mut new: HashMap<Sym<'_>, Type<'_, '_>> = constraints.clone();
                            new.insert(*right_sym, left_existing.clone());
                            new
                        }
                        (None, Some(right_existing)) => {
                            let mut new: HashMap<Sym<'_>, Type<'_, '_>> = constraints.clone();
                            new.insert(*left_sym, right_existing.clone());
                            new
                        }
                        (Some(left_existing), Some(right_existing)) => assert_type(
                            left_existing.clone(),
                            right_existing.clone(),
                            constraints,
                            depth + 1,
                        )?,
                    }
                } else {
                    if let Some(existing) = constraints.get(left_sym) {
                        assert_type(existing.clone(), expected, constraints, depth + 1)?
                    } else {
                        let mut new: HashMap<Sym<'_>, Type<'_, '_>> = constraints.clone();
                        new.insert(*left_sym, expected.clone());
                        new
                    }
                }
            }
            TypeOption::Clos(arg_s, res_s) => match &expected.typ {
                TypeOption::Var(_) => assert_type(expected, source, constraints, depth + 1)?,
                TypeOption::Clos(arg_e, res_e) => {
                    let constraints =
                        assert_type(*arg_s.clone(), *arg_e.clone(), constraints, depth + 1)?;
                    assert_type(*res_s.clone(), *res_e.clone(), constraints, depth + 1)?
                }
                _ => Err(TypeError::UnresolvableTypes(source.expr.clone(), expected.typ.try_into().unwrap()))?,
            },
            _ => match &expected.typ {
                TypeOption::Var(_) => assert_type(expected, source, constraints, depth + 1)?,
                _ => Err(TypeError::UnresolvableTypes(source.expr.clone(), expected.typ.try_into().unwrap()))?,
            },
        }
    } else {
        constraints
    })
}

fn lookup<'symbol, 'expr>(
    constraints: TypeConstraints<'expr, 'symbol>,
    t: Type<'expr, 'symbol>,
    depth: i32,
) -> Result<(TypeConstraints<'expr, 'symbol>, Type<'expr, 'symbol>), TypeError<'expr>> {
    if depth > 1000 {
        Err(TypeError::InfiniteType)?
    }
    Ok(match t.typ {
        TypeOption::Var(s) => {
            if let Some(t) = constraints.get(&s) {
                lookup(constraints.clone(), t.clone(), depth + 1)?
            } else {
                (constraints, t)
            }
        }
        TypeOption::Clos(a, b) => {
            let (constraints, a) = lookup(constraints, *a, depth + 1)?;
            let (constraints, b) = lookup(constraints, *b, depth + 1)?;
            (
                constraints,
                TypedExpr {
                    typ: TypeOption::Clos(Box::new(a), Box::new(b)),
                    expr: t.expr,
                },
            )
        }
        _ => (constraints, t),
    })
}

#[derive(Default)]
struct Typechecker {
    gensym: SymbolGenerator,
}

impl Typechecker {
    fn typecheck<'expr, 'me, 'b>(
        &'me self,
        e: &Expr<'expr>,
        env: &'b TEnv<'expr, 'me>,
        constraints: TypeConstraints<'expr, 'me>,
    ) -> Result<(TypeConstraints<'expr, 'me>, Type<'expr, 'me>), TypeError<'expr>> {
        let constrain_binary =
            |left: &Expr<'expr>,
             right: &Expr<'expr>,
             t_in: Type<'expr, 'me>,
             t_out: Type<'expr, 'me>,
             constraints: TypeConstraints<'expr, 'me>|
             -> Result<(TypeConstraints<'expr, 'me>, Type<'expr, 'me>), TypeError> {
                let (constraints, left_t) = self.typecheck(left, env, constraints)?;
                let constraints = assert_type(left_t, t_in.clone(), constraints, 0)?;
                let (constraints, right_t) = self.typecheck(right, env, constraints)?;
                let constraints = assert_type(right_t, t_in, constraints, 0)?;
                Ok(lookup(constraints, t_out, 0)?)
            };

        Ok(match &e.kind {
            ExprKind::Add(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                constraints,
            )?,
            ExprKind::Sub(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                constraints,
            )?,
            ExprKind::Mul(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                constraints,
            )?,
            ExprKind::Div(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                constraints,
            )?,
            ExprKind::Lt(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Bool,
                },
                constraints,
            )?,
            ExprKind::Gt(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Bool,
                },
                constraints,
            )?,
            ExprKind::Eq(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Double,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::Bool,
                },
                constraints,
            )?,
            ExprKind::Not(left) => {
                let (constraints, t) = self.typecheck(left, env, constraints)?;
                lookup(
                    assert_type(
                        t,
                        TypedExpr {
                            expr: *left.clone(),
                            typ: TypeOption::Bool,
                        },
                        constraints,
                        0,
                    )?,
                    TypedExpr {
                        expr: e.clone(),
                        typ: TypeOption::Bool,
                    },
                    0,
                )?
            }
            ExprKind::Lit(x) => match x {
                LiteralKind::Double(_) => (
                    constraints,
                    TypedExpr {
                        expr: e.clone(),
                        typ: TypeOption::Double,
                    },
                ),
                LiteralKind::Bool(_) => (
                    constraints,
                    TypedExpr {
                        expr: e.clone(),
                        typ: TypeOption::Bool,
                    },
                ),
                LiteralKind::String(_) => (
                    constraints,
                    TypedExpr {
                        expr: e.clone(),
                        typ: TypeOption::String,
                    },
                ),
            },
            ExprKind::Concat(left, right) => constrain_binary(
                left,
                right,
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::String,
                },
                TypedExpr {
                    expr: e.clone(),
                    typ: TypeOption::String,
                },
                constraints,
            )?,
            ExprKind::If(cond, t, f) => {
                let (constraints, cond_t) = self.typecheck(cond, env, constraints)?;
                let constraints = assert_type(
                    cond_t,
                    TypedExpr {
                        expr: e.clone(),
                        typ: TypeOption::Bool,
                    },
                    constraints,
                    0,
                )?;
                let (constraints, t_t) = self.typecheck(t, env, constraints)?;
                let (constraints, f_t) = self.typecheck(f, env, constraints)?;
                let constraints = assert_type(t_t.clone(), f_t, constraints, 0)?;
                lookup(constraints, t_t, 0)?
            }
            ExprKind::Lambda(id, expr) => {
                let arg_sym = self.gensym.get();
                let arg = TypedExpr {
                    typ: TypeOption::Var(arg_sym),
                    expr: e.clone(),
                };
                let mut new_env = env.clone();
                new_env.insert(id, arg.clone());
                let (constraints, body_t) = self.typecheck(expr, &new_env, constraints)?;

                lookup(
                    constraints,
                    TypedExpr {
                        typ: TypeOption::Clos(Box::new(arg), Box::new(body_t)),
                        expr: *expr.clone(),
                    },
                    0,
                )?
            }
            ExprKind::Call(func, arg) => {
                let (constraints, func_t) = self.typecheck(func, env, constraints)?;
                let (constraints, arg_t) = self.typecheck(arg, env, constraints)?;
                let res_t = TypedExpr {
                    typ: TypeOption::Var(self.gensym.get()),
                    expr: e.clone(),
                };
                let constraints = assert_type(
                    func_t,
                    TypedExpr {
                        typ: TypeOption::Clos(Box::new(arg_t), Box::new(res_t.clone())),
                        expr: e.clone(),
                    },
                    constraints,
                    0,
                )?;
                lookup(constraints, res_t, 0)?
            }
            ExprKind::Id(x) => (constraints, env.get(x).unwrap().clone()),
        })
    }
}

pub fn typecheck<'a, 'b>(e: &Expr<'a>) -> Result<ConcreteType, TypeError<'a>> {
    let checker = Typechecker::default();
    let (_, typ) = checker.typecheck(e, &Default::default(), Default::default())?;

    Ok(typ.typ.try_into().unwrap())
}

#[test]
fn test_eval_simple<'a>() -> Result<(), TypeError<'a>> {
    let prog = parse("(+ 1 1)");

    assert!(matches!(typecheck(&prog)?, ConcreteType::Double));
    Ok(())
}

#[test]
fn test_eval_complex<'a>() -> Result<(), TypeError<'a>> {
    let prog = parse("(let x (lambda x (+ x 1)) (x 4))");

    assert!(matches!(typecheck(&prog)?, ConcreteType::Double));
    Ok(())
}

#[test]
fn test_eval_complexer<'a>() -> Result<(), TypeError<'a>> {
    let prog = parse("((lambda x (+ x 1)) 3)");

    assert!(matches!(typecheck(&prog)?, ConcreteType::Double));
    Ok(())
}

#[test]
fn test_eval_complexer_2<'a>() -> Result<(), TypeError<'a>> {
    let prog = parse("(if (= 10 ((lambda x (+ x 1)) 3)) 7 1)");

    assert!(matches!(typecheck(&prog)?, ConcreteType::Double));
    Ok(())
}

#[test]
fn test_letrec() {
    let sum = parse(
        "(letrec f (lambda x 
            (if (= x 0) 
                0
                (+ x (f (- x 1))))) (f 5))",
    );

    assert_eq!(typecheck(&sum).unwrap_err(), TypeError::InfiniteType);
}
