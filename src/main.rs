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

    if let Err(err) = typecheck(&y) {
        match err {
            typecheck::TypeError::UnresolvableTypes {
                expr,
                actual,
                expected,
            } => {
                let mut prev: Option<&str> = None;
                let expr_start = expr.source.as_ptr() as usize;
                let mut left_to_print: Option<usize> = None;
                let mut line_num = 1;
                for line in prog.lines() {
                    if left_to_print == Some(0) {
                        break;
                    } else if let Some(left) = left_to_print {
                        println!("{:>5} | {}", line_num, line);
                        left_to_print = Some(left - 1);
                    } else if (line.as_ptr() as usize) + line.len() > expr_start {
                        if let Some(prev) = prev {
                            println!("{:>5} | {}", line_num - 1, prev);
                        }
                        println!("{:>5} | {}", line_num, line);
                        for _ in 0..(8 + expr_start - (line.as_ptr() as usize)) {
                            print!(" ");
                        }
                        for _ in 0..(expr.source.chars().count()) {
                            print!("~");
                        }
                        println!(" <-- {:?} (was {:?})", expected, actual);
                        left_to_print = Some(1);
                    }

                    line_num += 1;
                    prev = Some(line);
                }
            }
            typecheck::TypeError::InfiniteType => todo!(),
        }
    } else {
        println!("{:?}", eval(&y, &Default::default()));
    }
}
