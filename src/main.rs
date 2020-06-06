#![feature(type_ascription, pattern)]
// #![allow(dead_code)]
// ToDo: Remove the above `allow` attribute.

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;
extern crate string_cache;

mod wvm;
mod cell;
mod address;
mod parser;
mod chariter;
mod term;
mod functor;
mod token;

use crate::wvm::WVM;

fn main() {
  #[cfg(feature = "trace_computation")]
  println!("Computation Tracing ENABLED");

  let text = "?-p(Z,h(Z,W),f(W))";
  println!("Query Expression: {}", text);

  let mut machine = WVM::new();
  machine.compile(text);

  // println!("Internal Machine State:\n{}", machine);


  let text = "p(f(X), h(Y, f(a)), Y)";
  println!("Program Expression: {}", text);

  machine.compile(text);

  // println!("Internal Machine State:\n{}", machine);

}
