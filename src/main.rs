use eval::eval;
use parse::parse;
use std::io;
use std::io::Read;
use typecheck::typecheck;

mod eval;
mod parse;
mod sexp;
mod tokenize;
mod typecheck;

fn main() {
    let mut buf = vec![];
    io::stdin().read_to_end(&mut buf).unwrap();
    let prog = String::from_utf8(buf).unwrap();
    let y = parse(&prog);

    typecheck(&y);

    println!("{:?}", eval(&y, &Default::default()));
}
