/*!

This module parses prolog source code.

The language is given by the following EBNF:
    ```
    <clauses>          ::=  <clauses> <clause> | <clauses>
    <query>            ::=  ':-' <clause>
    <clause>           ::=  <fact> | <rule>
    <fact>             ::=  <predicate> '.'
    <rule>             ::=  <predicate> ':-' <predicate_list> '.'
    <predicate_list>   ::=  <predicate_list> ',' <predicate> (',')? | <predicate>
    <predicate>        ::= = <variable> | <structure>
    <structure>        ::=  <small_atom> '(' <predicate_list> ')' | <small_atom>
    <small_atom>       ::=  <lowercase> <alphanumeric>*
    <variable>         ::=  <uppercase> <alphanumeric>*
    ```

Special lexical forms, which are ignored:
    ```
    <eol_comment>     ::= '%' .* ('\n' | EOF)
    <inline_comment>  ::= '(*' (.* | '\n')* '*)'
    <whitespace>      ::= [ \t\cr\n\lf]+
    ```

Whitespace and comments are ignored. Note that comments are not nested. We allow comma-delineated
lists to have a trailing comma without warning. Constants are represented as a structure with a
zero-length argument list.

*/
extern crate nom;

use nom::{
  branch::alt,
  bytes::complete::{
    is_not,
    take_until,
    take_while1,
    tag
  },
  character::complete::{
    char as one_char,
    alphanumeric0,
    multispace1
  },
  combinator::{
    recognize,
    map,
    opt
  },
  multi::{
    separated_nonempty_list,
    many0
  },
  sequence::{
    delimited,
    pair,
    preceded,
    separated_pair,
    terminated,
    tuple
  },
  error::ParseError,
  Err as NomErr,
  IResult
};
use string_cache::DefaultAtom;

use crate::functor::{ArityType, Functor};
use super::term::{Term, TermVec};

struct Parser<'a>{
  text   : &'a str,
  errors : Vec<String>
}


/// Parses text to produce abstract syntax trees made of `Term`s.
/// Returns a tuple (atoms, queries).
pub fn parse(input: &str) -> Result<(TermVec, Option<Term>), ()> {
  let mut parser = Parser::new(input);

  match parser.parse(){
    Ok(t) => Ok(t),
    Err(()) => {
      eprintln!("Encountered these errors while parsing:");
      for e in parser.errors{
        eprintln!("{}", e);
      }
      Err(())
    }
  }
}

impl<'a> Parser<'a> {
  pub fn new(input: &'a str) -> Self {
    Parser {
      text: input,
      errors: vec![]
    }
  }

  /**
    Parses text to produce abstract syntax trees made of `Term`s.
    Returns a tuple pf (clauses, query).

    <clauses> ::= <clauses> <clause> | <clauses>
    <query>   ::= ':-' <clause>

  */
  pub fn parse(&mut self) -> Result<(TermVec, Option<Term>), ()> {
    let mut query: Option<Term> = None;
    let mut clauses: TermVec = Vec::new();
    let mut success: bool = true;

    if self.text.is_empty() {
      eprintln!("Input is empty.");
      return Err(());
    }

    loop {
      let parse_result = pclause(self.text);
      match parse_result {

        Ok((i, term)) => {
          self.text = i;
          match term {

            Term::Query(_) => {
              query = Some(term);
            }

            term => {
              clauses.push(term);
            }

          };
        }

        Err(NomErr::Incomplete(_)) => {
          // This shouldn't happen.
          self.errors.push("Error: Input incomplete.".to_string());
          // Fatal error, abort.
          return Err(());
        },

        | Err(NomErr::Error((i, errorkind)))
        | Err(NomErr::Failure((i, errorkind))) => {
          self.errors.push(format!("Error: {}", errorkind.description()));
          // We keep attempting to parse to accumulate all errors.
          success = false;
          self.text = i;
          if self.panic_mode() {
            // Fatal error, abort.
            return Err(());
          }
        }

      };

      // Allow trailing whitespace, but nothing more.
      self.text = self.text.trim_start();
      if self.text.is_empty() {
        break;
      }
    } // end loop

    match success {
      true => Ok((clauses, query)),
      false => {
        for error in &self.errors {
          eprintln!("{}", error);
        }
        Err(())
      }
    }
  }

  /**
    Gobbles the rest of the line if possible. If another error prevents this, it gives up.

    Returns whether or not the error should be considered fatal.
  */
  fn panic_mode(&mut self) -> bool{
    let result: Result<(&str, &str), nom::Err<(&str, nom::error::ErrorKind)>>
      = is_not("\n")(self.text);
    match result {
      Ok((i, _)) => {
        self.text = i;
        false
      },
      Err(_) => {
        eprintln!("Could not recover from parse error.");
        true
      },
    }
  }
}


/**
  <clause>          ::= <fact> | <rule> | query
  <fact>            ::= <predicate> '.'
  <rule>            ::= <predicate> ':-' <predicate_list> '.'
  <query>           ::= '?-' <predicate_list> '.'
*/
fn pclause(text: &str) -> IResult<&str, Term>{
  alt((
    // A fact
    terminated(ppredicate, wst(one_char('.'))),
    // A query
    map(
      terminated(
        preceded(ws(tag("?-")),
          ppredicate_list
        ),
        wst(one_char('.'))
      ),
      |out|{
        Term::Query(out)
      }
    ),
    // A rule
    map(
      terminated(
        separated_pair(
          ppredicate, tag(":-"),
          ppredicate_list
        ),
        wst(one_char('.'))
      ),
      |out|{
        Term::Rule {
          predicate: Box::new(out.0),
          goals: out.1,
        }
      }
    )
  ))
  (text)
}


/// <predicate_list> ::= <predicate_list> ',' <predicate> (',')? | <predicate>
fn ppredicate_list(text: &str)-> IResult<&str, TermVec>{
  terminated(
    separated_nonempty_list(one_char(','), ppredicate),
    opt(wst(one_char(',')))
  )(text)
}


/**
  <predicate> ::== <variable> | <structure>

  Eventually we will have:
    <predicate> ::== <variable> | <structure> | <string> | <integer>
*/
fn ppredicate(text: &str) -> IResult<&str, Term>{
  ws(alt((
    pvariable, pstructure, // pstring, pinteger,
  )))
  (text)
}

/**
  <structure> ::= <small_atom> '(' <predicate_list> ')' | <small_atom>
*/
fn pstructure(text: &str) -> IResult<&str, Term>{
  alt((
    // Nonconstant functor
    map(
      tuple((
        wst(psmall_atom),
        one_char('('),
        terminated(
          separated_nonempty_list(one_char(','), ppredicate),
          opt(one_char(','))),
        one_char(')')
      )),
      |out|{
        let args: TermVec = out.2;
        let functor = Functor{
          name: DefaultAtom::from(out.0),
          arity: args.len() as ArityType
        };
        Term::Predicate {
          functor,
          args
        }
      }
    ),
    // Constant
    map(ws(psmall_atom), |out| {
      let functor = Functor{
        name: DefaultAtom::from(out),
        arity: 0
      };
      Term::Predicate {
        functor,
        args: vec![]
      }
    }
    )
  ))
  (text)
}

/**
  <small_atom> ::= <lowercase> <alphanumeric>*
*/
fn psmall_atom(text: &str) -> IResult<&str, &str>{
  recognize(pair(take_while1(char::is_lowercase), alphanumeric0))
  (text)
}

/**
  <variable> ::= <uppercase> <alphanumeric>*
*/
fn pvariable(text: &str) -> IResult<&str, Term>{
  map(recognize(pair(take_while1(char::is_uppercase), alphanumeric0)),
      |out| Term::Variable(DefaultAtom::from(out))
  )(text)
}


fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
  where
  F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  move |i| {
    delimited(
      &pskip,
      &inner,
      &pskip
    )(i)
  }
}


fn wst<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
  where
  F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  move |i| {
    terminated(
      &inner,
      &pskip
    )(i)
  }
}


pub fn pskip<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
  map(
    many0(
      alt((map(multispace1, |_| ()), pinline_comment, peol_comment))
    ),
    |_| ()
  )(i)
}

/*
  <eol_comment> ::= '%' [^\n\r]*
*/
pub fn peol_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
  map(pair(one_char('%'), is_not("\n\r")),
      |_| ()
  )(i)
}

/*
  <eol_comment> ::= '%' [^\n\r]*
*/
pub fn pinline_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
  map(
    tuple((
      tag("(*"),
      take_until("*)"),
      tag("*)")
    )),
    |_| () // Output is thrown away.
  )(i)
}
