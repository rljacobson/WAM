# Infrastructure: Parsing and “Flattening” Terms

Our strategy will be to build increasingly sophisticated versions of our theorem prover as we go along. We will name them $M_n$, where $n$ is a natural number. The simplest, $M_0$, will already have all of the essential features. We will see that it can prove many statements of formal logic despite the simplicity of the program. Indeed, most of our work will be in engineering the supporting infrastructure such as the parser and virtual machine. It may feel over-engineered for such a simple program, but we are laying the foundation for future work and making it easy to add in optional features if we want them.

## The language

The input syntax is very bare bones. Terms have the following recursively defined lexical structure:

   * **Variables:** A single uppercase letter
   * **Constants:** A single lowercase letter
   * **Functors:** A single lowercase letter followed by a comma-delineated list of terms enclosed in
               parentheses
   * **A Program:** The top-most (outer-most) term with no prefix (cf. Query)
   * **A Query:** The literal `?-` followed by a top-most (outer-most) term

Thus, the word *term* refers to any one of the things on this list. *Whitespace* is ignored. It is an error to have two terms adjacent except if separated by a comma as functor arguments. We allow comma-delineated lists of arguments to have a trailing comma without warning. (I wish every programming language allowed this.)

A typical expression in this language looks like `?-p(f(X), h(Y, f(a)), Y)`. This is a query, not a program, because it starts with `?-`. The word functor is borrowed by mathematicians from linguistics. *Functors* are “ words that have little lexical meaning... and express grammatical relationships among other words within a sentence….”[^wiki_functor] They are meaningless symbols in themselves but express relationships between other symbols. This is in contrast to *functions*, which do have meaning in themselves: every college student knows what $\sin(x)$ means. 

## The abstract symbol tree representation

Programmers will immediately recognize the inherent tree structure of expressions in this language. As we parse an expression we build a tree out of nodes, with each node representing one term. The resulting tree is called an *abstract syntax tree*, or *AST* for short. The nodes will be variants of the `Term` enum. 

```rust
// term.rs
use string_cache::DefaultAtom;

pub type RcTerm = Rc<Term>;
pub type TermVec = Vec<Rc<Term>>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
  /// An interned string staring with an uppercase letter.
  Variable(DefaultAtom),
  Structure {
    functor : Functor,
    args    : TermVec
  },
  Query(RcTerm)
}
```

!!! tip
    You might want to add a `Register(Address)` variant to `Term` to make it easier to build terms “by hand,” say, for testing purposes, even though strictly speaking registers are an implementation detail rather than a language component.

I omit `use` statements like `use std::rc::Rc;` when there is no risk of confusion, but in the listing below you will notice we `use` the [`string_cache`](https://crates.io/crates/string_cache) library for [interning strings](https://en.wikipedia.org/wiki/String_interning). Interpreters and compilers are the quintessential application for string interning. It makes little sense to keep 400 copies of a keyword in memory or do thousands of string comparisons when they could be integer comparisons.

A constant behaves exactly like a functor that takes no arguments. By representing constants as functors of arity zero, that is, zero arguments, we avoid needing a `Constant` variant. We could also have both `Query` and `Program` variants, but it seems simpler to declare any term that is not a `Query` a program. A *functor structure*, or just *structure* for short, is (an instance of) a functor together with its arguments.

```rust
// functor.rs
pub type ArityType = u32;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Functor {
  pub name  : DefaultAtom,
  pub arity: ArityType
}
```

A functor denoted $f/n$ is uniquely identified by its name $f$ and arity $n$. As a consequence, $f/2 \neq f/3$ even though they have the same name. Go ahead and implement the `Display` trait for `Functor`. Don’t forget that constants are expressed only as a single letter, i.e. without arity.

We also want to have the ability to print a `Term` to the console, so we implement `Display` for `Term`. This involves printing a tree structure, which is a bit tricky if you haven’t done it before. Try it yourself before looking at my version.

```rust
// term.rs
	⋮
impl Display for Term{
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.fmt_aux(&"", &""))
  }
}
impl Term{
  fn fmt_aux(&self, prefix: &str, child_prefix: &str) -> String {
    match self {
      Term::Structure{ functor, args } => {
        let mut buffer =
          if args.is_empty() { format!("{}Constant<{}>", prefix, functor.name) } 
        	else{ format!("{}Functor<{}(…)>\n", prefix, functor) };
        for (i, next) in args.iter().enumerate() {
          if i != args.len() - 1 {
            buffer.push_str(
              next.fmt_aux(
                &format!("{}{}", child_prefix, "            ├── "),
                &format!("{}{}", child_prefix, "            │   ")
              ).as_str()
            );
            buffer.push('\n');
          } else {
            buffer.push_str(
              next.fmt_aux(
                &format!("{}{}", child_prefix, "            └── "),
                &format!("{}{}", child_prefix, "                ")
              ).as_str()
            );
          }
        };
        buffer
      }
      Term::Variable(c) => { format!("{}Variable<{}>", prefix, c) },
      Term::Query(inner) => { format!("{}Query<{}>", prefix, inner) }
    }
  }
}
```

The key is the auxiliary function, which takes additional arguments to keep track of the prefix. In my own version, I also included a `to_expression_string()` function that returns the boring expression string, for example `p(f(X), h(Y, f(a)), Y)`.

## Parsing the input string

In anticipation of more complex parsing tasks in the future, you might prefer to use a scanner generator or parser combinators. I chose to write a quick-and-dirty parser by hand. In any case, our goal is to transform a string like `p(Z,h(Z,W),f(W))` into a `Term` (tree) that looks like this:

```
Functor<p/3(…)>
            ├── Variable<Z>
            ├── Functor<h/2(…)>
            │               ├── Variable<Z>
            │               └── Variable<W>
            └── Functor<f/1(…)>
                            └── Variable<W>
```

### A Rust aside

But first, a Rust-specific aside, which you should skip if you are using another programming language. Whenever I need to process strings in Rust, I get a temporary case of Tourette Syndrome. In the case of this parser, I want:

1. an iterator over the UTF-8 code points[^scalar] in the input text
2. which allows peeking at the next value without consuming it
3. and which provides some elementary string utilities found in `std::str::str`. 

The Rust standard library makes it difficult to do this. I can get #1 with `str.chars()` which gives me a `Chars` iterator, but `Chars` lacks a `peek()` function. `Chars` can be turned into a `std::iter::Peekable<Chars>`, but `Peekable` isn’t a trait! Ok, so we just retain the `Peekable<Chars>` object, right? No, because we also want access to the underlying `&str`, which we have with `Chars::as_str()`, in order to have #3, and we lose access to `Chars::as_str()` when we turn `Chars` into `Peekable<Chars>`. The design of iterators is one of my least favorite things in Rust. They traded the ability to implement iterators as traits for generality. My solution was to write my own iterator struct that has the features I need, making something that is trivial in other languages into a complicated ordeal in Rust. If a rustacean can tell me a better way, please do!

My iterator merely caches the next character as needed for `peek()`. Its implementation is completely unremarkable, so I skip it here.

### The parser

 The publicly visible `parse(text: &str)` function immediately hands off the parsing task to two other mutually recursive auxiliary functions that do the real work. The `parse_aux()` function parses individual terms, while `parse_arg_list()` parses lists of `Term`s. There are no real surprises here.

```rust
// parser.rs
	⋮
fn parse_aux(text_ref: &RefCell<CharIter>) -> Option<Term> {
  let mut text = text_ref.borrow_mut();
  let mut next_char: char;

  // Handle Comments, which require resetting `next_char`.
  loop {
  	text.trim_left();
    match text.next(){ 
      Some(c) => { next_char = c; },
      None => { return None; }
    }
    // Single-line comments
    if next_char == '%' {
      // Eat until EOL or EOF.
      while text.peek() != None && text.next() != Some('\n'){/* Gobble */}
    }
    // In-line comments
    else if next_char == '/' && text.peek() == Some('*'){
      text.next();
      // Eat until EOL or EOF.
      while text.peek() != None {
        // Gobble
        next_char = text.next().unwrap();
        if next_char == '*' && text.peek() == Some('/'){
          text.next();
          // Break inner while-loop, reset `next_char`
          break;
        }
  		}
    } else {
      break; // Not a comment, break outer loop.
    }
  } // comment loop: reset `next_char` at the top of the loop.

  // A giant string of if-else's //
  if next_char.is_lowercase(){
    // Functor Structure //
    // `parse_arg_list` needs mutable access to the `CharIter`.
    drop(text);
    let args = parse_arg_list(text_ref);
    Some(Term::Structure {
      functor: Functor{DefaultAtom::from(next_char.to_string()), arity: args.len() as ArityType},
      args
    })
  }
  else if next_char == ',' || next_char == ')' {
    // The assumption here is that `parse_aux` was called recursively, and the current term has
    // been completely parsed.
    None
  }
  else if next_char.is_uppercase(){
    Some(Term::Variable(DefaultAtom::from(next_char.to_string())))
  }
  else if next_char == '?' {
    match text.next() {
      Some('-') => {
        // `parse_aux` needs a mutable borrow of text, so drop this one.
        drop(text);
        let term_option = parse_aux(text_ref);
        match term_option {
          Some(term) => Some(Term::Query(Rc::new(term))),
          None       => None
        }
      }
      _ => {
        eprintln!("Error: Unexpected character `{}`", next_char);
        panic!();
      }
    }
  }
  else {
    eprintln!("Error: Unexpected character `{}`", next_char);
    panic!();
  }
}
fn parse_arg_list(text_ref: &RefCell<CharIter>) -> TermVec {
  let mut args: Vec<RcTerm> = Vec::new();
  let mut text = text_ref.borrow_mut();

  text.trim_left();
  if let Some(c) = text.peek(){
    if c != '(' {
      // Constants can omit parentheses, as they have no arguments.
      return args;
    } else { text.next(); /* Eat `(` */ }
  }
  let mut term_option: Option<Term>;
  loop {
    // `parse_aux` needs a mutable borrow of text, so drop this one.
    drop(text);
    term_option = parse_aux(text_ref);
    text = text_ref.borrow_mut();
    if text.is_empty(){
      eprintln!("Reached EOL while looking for `)`.");
      panic!();
    }
    if term_option != None {
      args.push(Rc::new(term_option.unwrap()));
    } 
    else { return args; }
    text.trim_left();
    match text.peek(){
      Some(',') => { text.next(); },
      Some(')') => { text.next(); break; },
      _ => { /* Don't advance. */ }
    } // end match peek
  } // end while
  args // return
}
```

## Flattening a term

As an intermediate step to building the in-memory representation of a term, we transform the AST into a flattened form. The flattened form of a tree is a collection of trees each of which has depth at most one. Consider this AST:

```
Functor<p/3(…)>
            ├── Variable<Z>
            ├── Functor<h/2(…)>
            │               ├── Variable<Z>
            │               └── Variable<W>
            └── Functor<f/1(…)>
                            └── Variable<W>
```

As we flatten this AST, we create new trees with names `X1`, `X2`, `X3`, …, `X`$n$, where $n$ is the total number of trees we make. Here is the flattened version of the above AST:

```
X1 = p(X2, X3, X4)
X2 = Z
X3 = h(X2, X5)
X4 = f(X5)
X5 = W
```

The `=` is interpreted as assignment. Notice that the original variables can now be erased by using the `X`$i$ they are assigned to in the role the variables once played. In other words, we could think of `X2`, for example, as a variable now. In fact, any of the `X`$i$ that are not assigned a functor structure are variables. After erasing the variable names in this way, the flattened term looks like this:

```
X1 = p(X2, X3, X4)
X2
X3 = h(X2, X5)
X4 = f(X5)
X5
```

The algorithm to flatten a term is simple: Traverse the AST breadth first. As we encounter subterms we have not yet seen, create a new tree `X`$i$ equal to the subterm, and replace the original appearance and all subsequent appearences of the subterm with that `X`$i$. 

### Breadth-first tree traversal

I created a term iterator struct for the standard breadth-first traversal as is idiomatic in Rust:

```rust
// term.rs - or a separate file if you prefer.
	⋮
#[derive(Debug)]
pub struct TermIter{
  terms: VecDeque<RcTerm>, // A stack of terms/tokens yet to be visited.
}

impl TermIter{
  pub fn new(start: &RcTerm) -> TermIter {
    TermIter{
      terms: VecDeque::from(vec![start.clone()])
    }
  }
}

/// Iterates over the terms in the term tree breadth first.
impl Iterator for TermIter{
  type Item = RcTerm;

  fn next(&mut self) -> Option<Self::Item> {
    let option_term = self.terms.pop_front();
    match &option_term {

      Some(rc_term) => {
        match &**rc_term {

          Term::Structure{args, ..} => {
            self.terms.extend(args.iter().cloned());
            option_term
          },

          _t => option_term
        }
      }

      None => None
    }
  }
}
```

### An aside on intermediate representations

We need to decide what kind of a “thing” we use to use to represent the flattened term. A representation of a piece of code that is neither the source code nor the final executable form is called an *intermediate representation* or *IR*. We have a few choices of IRs:

1. Keep using the `Term` enum. 
2. Use the `Token` enum that the bitcode[^bitcode] compiler will consume, if it’s different from `Term`. 
3. Use whatever representation is used for in-memory data, if it’s different from either of the other two above. For $M_0$ it will be: the `Cell` enum.
4. Invent some new enum type.

The only *un*reasonable option is #4, as it only provides the disadvantage of another intermediate representation type. One might choose option #2 in the hope that it will save us some work translating to yet another intermediate representation type later on, but alas, it will turn out that we will need to walk the forest of `X`$i$‘s anyway, so if we use a representation we are going to use anyway, there’s no extra work. Option #1 has the advantage that `Term`s print out to the console in a nice tree format. But a *flattened* tree no longer looks much like a tree anymore, so this advantage is limited.

I chose option #3, because it has the advantage that we can put it directly into the VM’s memory if we desire, as part of testing, for example.

### The algorithm

The code in this subsection refers to an `Address` type and a vector of `Address`es called `registers`. The `X`$i$’s we have been discussing in this section will be implemented as registers in the virtual machine. The `Cell` type used below is the type for the values that are stored either at an address in the vm’s heap memory or in a register. The `Address` type can represent both heap addresses and register names. We will talk about those types more when we start working on coding the vm itself. For now, addresses are just indexes into an array of registers.

The first step is to assign each distinct term its own register. Which terms have already been assigned which registers is tracked with a `HashMap<RcTerm, Address>`. (Recall that `type RcTerm = Rc<Term>`.) We again iterate over each term, assigning to each new term encountered the next available register. 

```rust
// token.rs
	⋮
pub fn flatten_term(ast: &RcTerm) -> (CellVec, Vec<Address>){
  let mut seen: HashMap<RcTerm, Address> = HashMap::new();
  // We visit the AST breadth first, adding new symbols to `seen` as we go. This assigns each
  // term its own register.
  let terms: TermIter = TermIter::new(&ast);
  for term in terms{
    let default = Address::from_reg_idx(seen.len());
    seen.entry(term.clone()).or_insert(default);
  }
  let mut registers = Vec::<RcCell>::new();
  registers.resize(seen.len(), Rc::new(Cell::Empty));
	⋮
```

With the vector of registers created with enough empty cells to hold each term, we fill the registers with its corresponding term translated into a cell. We could combine this step with the register assignment step, but I feel separating them makes the code conceptually simpler and easier to read without any penalty. 

```rust
	⋮
pub fn flatten_term(ast: &RcTerm) -> (CellVec, Vec<Address>){
  let mut seen: HashMap<RcTerm, Address> = HashMap::new();

  // We visit the AST breadth first, adding new symbols to `seen` as we go. This assigns each
  // term its own register.
  let terms: TermIter = TermIter::new(&ast);
  for term in terms{
    let default = Address::from_reg_idx(seen.len());
    seen.entry(term.clone()).or_insert(default);
  }

  let mut registers = Vec::<RcCell>::new();
  registers.resize(seen.len(), Rc::new(Cell::Empty));

  // Every term has a register assignment. Now populate the "registers".
  for (term, reg_ptr) in seen.iter(){
    match &**term{

      Term::Structure{functor, args} => {
        // Trying to be a bit too clever, probably.
        let mut new_args =
          args.iter()
              .map(|t| Rc::new(Cell::REF(*seen.get(t).unwrap())))
              .collect::<Vec<RcCell>>();
        new_args.insert(0, Rc::new(Cell::Functor(functor.clone())));
        registers[reg_ptr.idx()] = Rc::new(Cell::Structure(new_args.into()));
      },

      Term::Variable(_) =>{
        registers[reg_ptr.idx()] = Rc::new(Cell::REF(*reg_ptr));
      }

      _t => {
        // This should never happen in correct code during compilation but may be useful for
        // other reasons, e.g., testing.
        registers[reg_ptr.idx()] = Rc::new(Cell::Term(term.clone()));
      }

    };
  } // end for
	⋮
```

As you can see, the IR translation is as follows:

* `Term::Structure` ⟶`Cell::Structure`
* `Term::Variable` ⟶ `Cell::REF(address)`, where `address` is the address associated to the given variable term. 

There are two important consequences of how variable terms are translated:

1. The name of the variable gets erased. The name serves no function in flattened form, as the location now serves the role the name once did. (See the example at the beginning of this section.)
2. A `Cell::REF(address)` is interpreted as a variable if and only if `address` points to the location in which the cell itself is stored, whether register or location in the heap. That is, variables are now exactly those cells that reference themselves. Other instances of `Cell:REF(address)` merely point to whatever happens to be at `address`.

The remaining code in `flatten_term()` calls the `order_registers()` function and returns the result along with the registers vector containing the cells from the flattened term.

```rust
	⋮
  let rc_registers: CellVec = registers.into();
  let order = order_registers(rc_registers.clone());
  // order
  (rc_registers, order)
} // end flatten_term()
```

## Ordering a flattened term

The components (cells) of the flattened term need to be ordered so that registers are always assigned to before they are used, that is, before they appear on the right hand side of an assignment. In this way our programming language is like many other great languages that require a variable must be declared before it is used. The difference is, the `order_registers()` function satisfies this requirement on behalf of the source code author. I encourage you to write your own version of this algorithm before looking at mine, as it is an enjoyable algorithm to come up with.

For the sake of efficiency[^efficiency] the algorithm copies around addresses rather than cells. We maintain two `HashSet<Address>`s, one for addresses we have already seen, cleverly named `seen`, and another for the rest of the addresses, creatively called `unseen`. We also have a `Vec<Address>` called `ordered` to store the result. First, observe that we can automatically put every variable cell into `seen`, because they define and only use themselves. The `unseen` `HashSet` is initialized with everything else.

```rust
// token.rs
	⋮
pub fn order_registers(flat_terms: CellVec) -> Vec<Address>{
  // Contains the register arguments we've seen before.
  let mut seen: HashSet<Address> = HashSet::new();
  let mut unseen: HashSet<Address> = HashSet::new();
  // The vector holding the result
  let mut ordered: Vec<Address> = Vec::new();
  // First collect which registers need to be ordered, marking registers containing variables
  // as seen. At the end, `unseen` is all `Cell::Structure`s, while `seen` is all `Cell::REF`s.
  for (index, cell) in flat_terms.iter().enumerate(){
    match &**cell {
      Cell::REF(address) => { seen.insert(*address); },
      Cell::Structure(_) =>{ unseen.insert(Address::from_reg_idx(index)); },
      _t => {
        unreachable!(
          "Error: Encountered a non-variable/non-struct cell after flattening a term: {}.",
          _t
        );
      }
    }
  } // end for
	⋮
```

Now comes the meat of the algorithm. Having seen a cell means either the cell is a variable or the cell has been assigned to.  A cell in `unseen` can be put next in order if all of the addresses that appear on the right hand side of the equal sign have already been seen, because that means we have already visited the cells at those addresses and, for functor structures, already assigned to them. Thus, for each cell in `unseen`, we make a `HashSet` of all of the cells that appear on the right hand side for the cell, and if the `HashSet` is a subset of `seen`, then everything on the right hand side of the equal sign has been seen by definition, and so we stage the cell for being inserted next into the `order` vector.

Doing the above only one time is not sufficient to order every cell. We must repeat the process until `unseen` is empty. As a precaution against malicious actors who might construct pathological input, the process is limited to some reasonably large maximum number of attempts to empty out `unseen`. The final result is a vector of all distinct register addresses ordered so that the corresponding cells satisfy the desired property.

```rust
	⋮
	for _loop_count in 0..MAXIMUM_ORDER_ATTEMPTS {
    let next_regs: HashSet<Address> =
      unseen.iter().filter_map(
        |address| {
          match extract_addresses(flat_terms[address.idx()].clone()) {
            Some(address_set) if address_set.is_subset(&seen) => {
              Some(*address)
            }
            _v => { None }
          }
        }
      ).collect();
  
    for reg in next_regs{
      ordered.push(reg);
      seen.insert(reg);
      unseen.remove(&reg);
    }
    if unseen.is_empty() { break; }
  } // end for

  ordered
}
```

The term is now ready to be transformed into a stream of tokens to be consumed by the compiler.

## What’s next

Next time we will start work on the $M_0$ virtual machine itself. Then we will spend $M_1$ developing the bytecode compiler and interpreter and related memory facilities. In $M_2$ we add more sophisticated control flow and a call stack and activation records. There is a twist in our programming language that doesn’t appear in other imperitive or functional languages: Our call stack will have decision points that will let us backtrack when a choice is made that ultimately leads to failure to unify.

For my own version of the code in this and the next article, check out the `M_0` branch of the GitHub repository. You will see the extra little bits I added that aren’t part of the core program, and you will find the code to be more heavily commented and whitespaced than the code embedded in the articles. If you made your own customizations or improvements, used a different implementation language, have a better way of doing something, or found a mistake, get in touch. I would love to hear about it!



------

[scalar]: For the pedants who are quick to shout, “Not code points, Unicode *scalar values!*,” you should know that a) Unicode scalar values *are* code points, and b) nobody likes a know-it-all.

[wiki_functor]: [Function Word](https://en.wikipedia.org/wiki/Function_word), Wikipedia.

[bitcode]: Actually, we won’t quite have a bitcode yet. Instead, the VM will interpret the instructions as they are formed from the input token stream.

[efficiency]: More like a compulsion to micro-optimize. I doubt it’s any more efficient.