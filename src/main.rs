#![feature(type_ascription, pattern)]
#![allow(dead_code)]
// ToDo: Remove the above `allow` attribute.

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

  let text = "?-p(Z,h(Z,W),f(W))\n
  \tj(f(Y), r(X), f(P, Q))\n
  \tp(f(Y), r(X), f(P, Q))\n
  \tp(f(X), h(Y, f(a)), Y)\n";
  println!("Input Expression: \n\t{}", text);

  let mut machine = WVM::new();
  machine.compile(text, true, true);

  // println!("Internal Machine State:\n{}", machine);

}
