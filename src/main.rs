#![feature(type_ascription, pattern)]
// #![allow(dead_code)]
// ToDo: Remove the above `allow` attribute.

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;
extern crate strum;
#[macro_use] extern crate strum_macros;
mod wvm;
mod cell;
mod address;
mod parser;
mod chariter;
mod functor;
mod term;
mod token;
mod instructions;

use crate::wvm::WVM;

fn main() {
  #[cfg(feature = "trace_computation")]
  println!("Computation Tracing ENABLED");

  let text = "?-p(Z,h(Z,W),f(W))";
  // let ast = parser::parse(text);
  println!("Query Expression: {}", text);
  // println!("Parse Tree:\n{}\n", ast);

  let mut machine = WVM::new();
  machine.compile(text, true, true, true);

  // println!("Internal Machine State:\n{}", machine);

  let text = "p(f(X), h(Y, f(a)), Y)";
  // let ast = parser::parse(text);
  println!("Program Expression: {}", text);
  // println!("Parse Tree:\n{}\n", ast);

  machine.compile(text, true, true, true);

  // println!("Internal Machine State:\n{}", machine);

}
