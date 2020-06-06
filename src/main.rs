#![feature(type_ascription, pattern)]

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

fn main() {

  #[cfg(feature = "trace_computation")]
  println!("Computation Tracing ENABLED");

  /*
    It is important that in M_1 no two facts have the same head atom. Otherwise, the failure of
    the first version of the atom will have changed the registers because of its partial
    unification, and the matching program will not be matching against the original query but
    rather a query partially unified with some other program.
  */
  let text = "\t?-p(Z,h(Z,W),f(W))
  \tj(f(Y), r(X), f(P, Q))
  \ty(f(Y), r(X), f(P, Q))
  \tp(f(X), h(Y, f(a)), Y)";
  println!("Input Expression: \n{}", text);

  let mut machine = WVM::new();
  machine.compile(text, true, true);

  // println!("Internal Machine State:\n{}", machine);

}
