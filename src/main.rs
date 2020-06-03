#![feature(type_ascription, pattern)]
#![allow(dead_code)]
// ToDo: Remove the above `allow` attribute.

#[macro_use] extern crate prettytable;
#[macro_use] extern crate lazy_static;
extern crate strum;
#[macro_use] extern crate strum_macros;
#[macro_use] extern crate nom;

// mod wvm;
// mod cell;
mod address;
// mod parser;
// mod chariter;
// mod functor;
// mod term;
// mod token;
mod bytecode;

// use crate::wvm::WVM;

fn main() {

  /*
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
  */


  let text = "SetVariable(2)
SetVariable(5   )
PutStructure(1, 4)
      SetValue   (  5  )
      PutStructure(3,1)
    SetValue(2)
    SetValue(3)     #### This is a thing
    SetValue(4)
 Deallocate
    GetStructure(3,1) # This line happens when we get a structure
    Robert(2)
    UnifyVariable(3)
 Allocate ( 6  )
    UnifyVariable(4)



  #  Alexia(2,3)


    UnifyValue(4)
    Lily(6)
    GetStructure(1,6)
UnifyVariable(7)
GetStructure(1,2)
UnifyVariable(5)
GetStructure(345,7)


    # almost done

";


//   let text = "PutStructure(1,4)
// PutStructure(3,1)";
  let result = bytecode::parse_assembly(text);
  match result {
    Ok((rest, list)) => {
      for x in list{
        println!("{}", x);
      }
      println!("Rest: {}", rest);
    }
    Err(e) => {
      print!("{}", e);
    }
  }

}
