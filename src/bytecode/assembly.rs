/*!
  The human readable textual form of bytecode is called assembly. This module leverages the
  `strum` derives of the instruction related enums to serialize and deserialize bytecode to
  assembly.`
*/

use std::cell::RefCell;
use std::fmt::Display;
use std::str::FromStr;

use nom::{
  bytes::complete::is_not,
  character::complete::{alpha1, char as one_char, digit1, line_ending, space0},
  combinator::{map, opt},
  error::ErrorKind,
  IResult,
  lib::std::fmt::Formatter,
  multi::{many0, separated_list},
  multi::many1,
  sequence::{
    delimited,
    pair,
    separated_pair,
    tuple
  },
  sequence::preceded,
  sequence::terminated
};

use crate::address::{AddressType, Address};
use crate::bytecode::{Instruction, Operation};

// An `Either`-like enum to transparently collect source code errors.
#[derive(Debug)]
pub enum ParsedInstruction<'a> {
  Binary((&'a str, usize, usize)),
  Unary((&'a str, usize)),
  Nullary(&'a str)
}

pub enum ParsedAssemblySyntax<'a> {
  Instruction(Instruction),
  NotAnOperation{
    line: u32,
    name: &'a str
  },
  WrongArity{
    line: u32,
    operation: Operation,
    args: Vec<AddressType>
  }
}
use ParsedAssemblySyntax as Syntax;
use nom::branch::alt;

impl<'a> Display for ParsedAssemblySyntax<'a>{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self{
      Syntax::Instruction(i) => {
        write!(f, "{}", i)
      }
      Syntax::NotAnOperation {line, name} => {
        write!(f, "Error on line {}: {} is not an operation.", line, name)
      }
      Syntax::WrongArity{line, operation, args} => {
        write!(f,
          "Error on line {}: {} requires {} arguments but was given {}: ({})",
          line, operation, operation.arity(), operation,
           args.iter()
               .map(AddressType::to_string)
               .collect::<Vec<String>>()
               .join(", ")
        )
      }
    }
  }
}

// Abbreviated name internally

pub fn parse_assembly(text: &str) -> IResult<&str, Vec<Syntax>, (&str, ErrorKind)>{
  // Primitive error handling
  let line_number: RefCell<u32> = RefCell::new(0);

  let comment = pair(one_char('%'), is_not("\n\r"));
  let rest_of_line = terminated(space0, opt(&comment));
  let binary_inst_p = {
    tuple::<&str, _, (_, ErrorKind), _>((
      alpha1,
      delimited(
        delimited(space0, one_char('('), space0),
        separated_pair(
          digit1,
          delimited(space0, one_char(','), space0),
          digit1,
        ),
        delimited(space0, one_char(')'), &rest_of_line),
      )
    ))
  };
  let unary_inst_p = {
    tuple::<&str, _, (_, ErrorKind), _>((
      alpha1,
      delimited(
        delimited(space0, one_char('('), space0),
        digit1,
        delimited(space0, one_char(')'), &rest_of_line),
      )
    ))
  };
  let nullary_inst_p = {
    tuple::<_, _, (_, ErrorKind), _>((
      alpha1,
      opt(
        terminated(
          pair(
            preceded(space0, one_char('(')),
            preceded(space0, one_char(')'))
          ), // end pair
          &rest_of_line
        )// end terminated
      )// end opt
    )) // end tuple
  };

  let inst_list_p = separated_list(
    many1(line_ending),
    delimited(
      opt(tuple((space0, &comment, many1(line_ending)))),
      alt((
        map(preceded(space0, binary_inst_p),
          | out | {
            let mut ln_ref = line_number.borrow_mut();
            *ln_ref += 1;
            let opcode_result = Operation::from_str(out.0);
            match opcode_result {
              Ok(operation) if operation.arity() == 2 =>
                Syntax::Instruction(Instruction::Binary {
                  opcode: operation,
                  address1: (out.1).0.parse::<AddressType>().unwrap(),
                  address2: (out.1).1.parse::<AddressType>().unwrap(),
                }),
              Ok(operation) => {
                Syntax::WrongArity {
                  line: *ln_ref,
                  operation,
                  args: vec![
                    (out.1).0.parse::<AddressType>().unwrap(),
                    (out.1).1.parse::<AddressType>().unwrap()
                  ]
                }
              }
              _e => {
                Syntax::NotAnOperation {
                  line: *ln_ref,
                  name: out.0
                }
              }
            }
          }),
        map(preceded(space0, unary_inst_p),
          |out| {
            let mut ln_ref = line_number.borrow_mut();
            *ln_ref += 1;
            let opcode_result = Operation::from_str(out.0);
            match opcode_result {
              Ok(operation) if operation.arity() == 1 =>
                Syntax::Instruction(Instruction::Unary{
                  opcode: operation,
                  address: out.1.parse::<AddressType>().unwrap()
                }),
              Ok(operation) => {
                Syntax::WrongArity {
                  line: *ln_ref,
                  operation,
                  args: vec![
                    out.1.parse::<AddressType>().unwrap()
                  ]
                }
              }
              _e => {
                Syntax::NotAnOperation {
                  line: *ln_ref,
                  name: out.0
                }
              }
            }
          }
        ),
        map(preceded(space0, nullary_inst_p),
          |out| {
            let mut ln_ref = line_number.borrow_mut();
            *ln_ref += 1;
            let opcode_result = Operation::from_str(out.0);
            match opcode_result {
              Ok(operation) if operation.arity() == 0 =>
                Syntax::Instruction(Instruction::Nullary(operation)),
              Ok(operation) => {
                Syntax::WrongArity {
                  line: *ln_ref,
                  operation,
                  args: vec![]
                }
              }
              _e => {
                Syntax::NotAnOperation {
                  line: *ln_ref,
                  name: out.0
                }
              }
            }
          }
        )
      )),
      opt(tuple((space0, &comment, many0(line_ending))))
    ));

  inst_list_p(text)
}
