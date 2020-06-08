/*!
  The human readable textual form of bytecode is called assembly. This module leverages the
  `strum` derives of the instruction related enums to serialize and deserialize bytecode to
  assembly.`
*/

use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use string_cache::DefaultAtom;
use nom::{
  branch::alt,
  bytes::complete::is_not,
  character::complete::{
    alpha1,
    char as one_char,
    digit1,
    line_ending,
    space0
  },
  combinator::{map, opt},
  error::ErrorKind,
  multi::{
    many0,
    separated_list,
    many1
  },
  sequence::{
    delimited,
    pair,
    separated_pair,
    tuple,
    preceded,
    terminated
  },
  bytes::complete::tag,
  error::ParseError
};

use crate::address::{AddressNumberType, Address};
use crate::bytecode::{Instruction, Operation};
use crate::functor::Functor;

pub enum ParsedAssemblySyntax<'a> {
  Instruction(Instruction),
  NotAnOperation{
    line: u32,
    name: &'a str
  },
  WrongArity{
    line: u32,
    operation: Operation,
    args: Vec<AddressNumberType>
  }
}
// Abbreviated name internally
use ParsedAssemblySyntax as Syntax;

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
          line, operation, operation.arity(), args.len(),
           args.iter()
               .map(AddressNumberType::to_string)
               .collect::<Vec<String>>()
               .join(", ")
        )
      }
    }
  }
}

fn parse_address(address_text: &str) -> Address{
  let address_idx: AddressNumberType = address_text.parse::<AddressNumberType>().unwrap();
  Address::Register(address_idx)
}

fn parse_functor(functor_pair: Vec<&str>) -> Functor {
  let name = DefaultAtom::from(functor_pair[0]);
  let arity: u32 =
    match functor_pair.len(){
      2 => functor_pair[1].parse::<AddressNumberType>().unwrap() as u32,
      _ => 0
    };
  Functor{ name, arity  }
  // println!("FUNCTORSTRING::  {:?}", functor_pair);
  // Functor{ name: DefaultAtom::from("dummy".to_string()), arity: 0  }
}

pub fn parse_assembly(text: &str) -> Result<Vec<Syntax>, nom::Err<(&str, nom::error::ErrorKind)>>{
  // Primitive error handling
  // ToDo: Make this more accurate.
  let line_number: RefCell<u32> = RefCell::new(1);

  let comment_p = pair(one_char('%'), is_not("\n\r"));
  let newline_p = map(preceded(opt(tuple((space0, comment_p))),line_ending), |out| {
    let mut line_number_ref = line_number.borrow_mut();
    *line_number_ref = *line_number_ref + 1;
    out
  });
  let register_p = {
    map(
      delimited::<&str, _, _, _, (&str, ErrorKind), _, _, _>(
        tag("X["), digit1, one_char(']'),
      ),
      |out: &str| vec![out],
    )
  };
  let address_p = map(digit1, |out| vec![out]);
  let functor_p = {
    alt((
      map(
        separated_pair::<&str, _, _, _, (&str, ErrorKind), _, _, _>(
          alpha1, one_char('/'), digit1,
        ),
        |out: (&str, &str)| vec![out.0, out.1],
      ),
      map(alpha1, |out: &str| vec![out])
    ))
  };
  // let rest_of_line_p = terminated(space0, opt(&comment_p));
  let binary_inst_p = {
    tuple::<&str, _, (_, ErrorKind), _>((
      alpha1,
      delimited(
        delimited(space0, one_char('('), space0),
        separated_pair(
          alt((&address_p, &register_p, &functor_p)),
          delimited(space0, one_char(','), space0),
          alt((&address_p, &register_p)),
        ),
        preceded(space0, one_char(')')),
      )
    ))
  };
  let unary_inst_p = {
    tuple::<&str, _, (_, ErrorKind), _>((
      alpha1,
      delimited(
        delimited(space0, one_char('('), space0),
        alt((digit1, map(&register_p, |out: Vec<&str>| out[0]))),
        preceded(space0, one_char(')')),
      )
    ))
  };
  let nullary_inst_p = {
    terminated(
      alpha1,
      opt(
        delimited(
          space0,
          one_char('('),
            preceded(space0, one_char(')'))
          )
      )// end opt
    ) // end terminated
  };

  let inst_list_p = {
    delimited(
      many0(&newline_p),
      separated_list::<&str, _, _, (&str, ErrorKind), _, _>(
      many1(&newline_p),
        delimited(
          space0,
          alt((
            map(preceded(space0, binary_inst_p),
              | out: (&str, _) | {
                let opcode_result = Operation::from_str(out.0);
                match opcode_result {
                  Ok(operation) if operation.is_functor() =>
                    Syntax::Instruction(Instruction::BinaryFunctor {
                      opcode: operation,
                      functor: parse_functor((out.1).0),
                      address: parse_address((out.1).1[0]),
                    }),
                  Ok(operation) if operation.arity() !=2 => {
                    Syntax::WrongArity {
                      line: *line_number.borrow(),
                      operation,
                      args: vec![
                        (out.1).0[0].parse::<AddressNumberType>().unwrap(),
                        (out.1).1[0].parse::<AddressNumberType>().unwrap()
                      ]
                    }
                  }
                  Ok(operation) =>
                    Syntax::Instruction(Instruction::Binary {
                      opcode: operation,
                      address1: parse_address((out.1).0[0]),
                      address2: parse_address((out.1).1[1]),
                    }),
                  _e => {
                    Syntax::NotAnOperation {
                      line: *line_number.borrow(),
                      name: out.0
                    }
                  }
                }
              }),
            map(preceded(space0, unary_inst_p),
              |out| {
                let opcode_result = Operation::from_str(out.0);
                match opcode_result {
                  Ok(operation) if operation.arity() == 1 =>
                    Syntax::Instruction(Instruction::Unary{
                      opcode: operation,
                      address: parse_address(out.1)
                    }),
                  Ok(operation) => {
                    Syntax::WrongArity {
                      line: *line_number.borrow(),
                      operation,
                      args: vec![
                        out.1.parse::<AddressNumberType>().unwrap()
                      ]
                    }
                  }
                  _e => {
                    Syntax::NotAnOperation {
                      line: *line_number.borrow(),
                      name: out.0
                    }
                  }
                }
              }
            ),
            map(preceded(space0, nullary_inst_p),
              |out| {
                let opcode_result = Operation::from_str(out);
                match opcode_result {

                  Ok(operation) if operation.arity() == 0 =>
                    Syntax::Instruction(Instruction::Nullary(operation)),

                  Ok(operation) => {
                    Syntax::WrongArity {
                      line: *line_number.borrow(),
                      operation,
                      args: vec![]
                    }
                  }

                  _e => {
                    Syntax::NotAnOperation {
                      line: *line_number.borrow(),
                      name: out
                    }
                  }

                }
              }
            )
          )),
          space0
        )
      ),
      many0(&newline_p)
    )
  };

  match inst_list_p(text) {
    Ok((_s, syntax_vec)) => Ok(syntax_vec),
    Err(_e) => Err(_e)
  }
}
