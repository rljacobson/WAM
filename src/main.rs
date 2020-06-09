#![feature(type_ascription, pattern)]
// nom's type inference requirements
// See https://github.com/rust-lang/rust/issues/54540
#![type_length_limit="4180863"]
#![allow(dead_code)]

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;
extern crate strum;
#[macro_use] extern crate strum_macros;
extern crate nom;
extern crate bimap;

mod wvm;
mod cell;
mod address;
mod parser;
mod chariter;
mod functor;
mod term;
mod token;
mod bytecode;

use crate::wvm::WVM;
// use crate::bytecode::parse_assembly;

fn main() {

  #[cfg(feature = "trace_computation")]
  println!("Computation Tracing ENABLED");

  /*
    It is important in M_1 that no two facts have the same head atom. Otherwise, the failure of
    the first version of the atom will have changed the registers because of its partial
    unification, and the matching program will not be matching against the original query but
    rather a query partially unified with some other program.
  */
  /*
  let text = "\t?-p(Z,h(Z,W),f(W))
  \tj(f(Y), r(X), f(P, Q))
  \ty(f(Y), r(X), f(P, Q))
  \tp(f(X), h(Y, f(a)), Y)";
  println!("Input Expression: \n{}", text);

  let mut machine = WVM::new();
  machine.compile(text, true, false);
  */

  let path = "/Users/rjacobson/Development/wam/ignore/assembly.txt";
  let assembly = std::fs::read_to_string(path).unwrap();
  // println!("File contents: \n{}\n", assembly);
  let mut machine = WVM::new();
  machine.compile_assembly(assembly.as_str(), true);


}
