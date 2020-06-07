# Let's build an automated theorem prover!

A lot of people have written little interpreters or emulators, but automated theorem provers have been conspicously neglected. In this series of $n$ articles, we will build an automated theorem prover by building an interpreter for a programming language. The bizarre—and fun—benefit of this approach is that we will necessarily realize the equivalence of type inference, computation, SMT solving, and proving statements of first-order logic. One of the most amazing facts in mathematics is that these are all in some sense the same activity. We will avoid deep discussion of the mathematical power of the mathematical systems we will use no matter how enticing the temptation. 

The phrase *automated theorem prover* itself is intimidating. All three of the words are scary! Let’s break it down.

- **Automated.** This part is a bit nebulous. An automated theorem prover cannot exist in a vacuum. We have to start with something. The idea is to write a computer program that can take some specification of initial facts and rules and then be able to answer queries (questions) we give it based on the data the program is able to extract from the specification.
- **Theorem.** This is the easiest. A *theorem* is just a mathematical fact for which we have a proof. That last part is important. If we don’t have a proof, we don’t (yet) have a theorem. Indeed, without a proof we can even question whether or not we can even call it a fact. Mathematicians call “facts” without proofs *conjectures*. Normal people call them guesses. Theorems cannot exist in a vacuum, either. We have to know the initial facts and the rules we are allow to use to manipulate those facts. In informal settings, the facts and rules are often known from context. Here is a typical theorem: “If $x$ and $y$ are both odd integers, then $x+y$ is an even integer."
- **Proof.** A *proof* is a step-by-step application of rules to known facts, each of which is irrefutable, to establish some mathematical fact. It is analogous to a recipe for a chocolate cake. The ingredients are the facts you start with, what mathematicians call *axioms*, and the activities like mixing and preheating the oven are the rules. At the end of the process of following the recipe, we get our theorem: a chocolate cake. An important feature of this process is that new facts are generated along the way. In our cake analogy we get new ingredients that result from acting on existing ingredients, the cake batter and frosting for example.

## Theorems about what?

Either types or first order logic, depending on how you want to interpret it. There are a few fundamental ideas to wrap your mind around before we set out. Let’s get on the same page with these ideas. We will primarily be dealing with types and formal logic.

### Types

When we use the word *type*, we will mean what every programmer means when they talk about data types. Functions also have types, though in some languages the type of a function is instead called the function’s (type) signature. For example, the type of the (pseudocode) function

```javascript
int f(int x){
  return x + 7;
}
```

is $int \Rightarrow int$. The “$\Rightarrow$” is part of the type and means function. Let’s do a more complicated example:

```javascript
List<int> g(List<int> mylist, int (*h)(int) ){
  return mylist.map(h);
}
```

This function takes both a list of integers and a function from integers to integers and produces another list of integers. It’s type is $(List\langle int \rangle,\; int \Rightarrow int) \Rightarrow List\langle int \rangle$.  

As every object-oriented programmer knows, types can have *subtypes* and *supertypes*. In the last example, the type  $List\langle int \rangle$  has supertype $List\langle T \rangle$ , where $T$ is a generic *type parameter* or *type variable*. (These kinds of supertypes are called templates in C++.) Likewise, clearly $List\langle int \rangle$ is a subtype of $List\langle T \rangle$.

### Formal logic and mathematical theorems

I assume a basic knowledge of mathematical logic. Briefly, formal logic is a mathematical system formalizing operations on statements that can either be true or false. If $p$ and $q$ are statements, then the notation $p\;\and\;q \rightarrow r$ means, “If $p$ is true and $q$ is true, then $r$ is also true.” If this sounds like the statement of a theorem, it is because theorems can generally be put into this form. Suppose $p$ is the statement, “$x$ is an odd integer,” $q$ is the statement, “$y$ is an odd integer,” and $r$ is the statement, “$x+y$ is an even integer.” Then $p\; \and \; q \rightarrow r$ means, “If $x$ and $y$ are both odd integers, then $x+y$ is an even integer.”

This is how we will state theorems in our theorem prover. Thus, our theorem prover will prove statements of formal logic. Specifically, our program will take as input a *program* consisting of a list of statements of the form 

$$\displaystyle p_1 \;\and\;p_2\;\and\;p_3\;\and\;\cdots\;\and\;p_n\;\rightarrow\;u,$$

called a [Horn clause](https://en.wikipedia.org/wiki/Horn_clause), and also a *query*, which is an expression we would like to know is true or false, and it will prove the answer to the query using the program. In fact, it will do much more, but let’s not get too carried away right at the beginning.

## Connecting types and formal logic

There is a vast literature describing how such-and-such a mathematical system is equivalent in some way to another seemingly different system. Among the most famous and startling of these results is the [Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), which says roughly that proving a theorem in intuitionist logic is equivalent to constructing an object of a specified type. It’s like looking at the same object from two different directions. Let’s show that proving something in formal logic in equivalent to showing that something is of a certain type in some make believe programming language.

#### Formal Logic Proof

Let us define the following:

$$\begin{equation}M(x,y) \text{ means, “}x\text{ is the mother of }y\text{.”}\\ P(x,y) \text{ means, “}x\text{ is the parent of }y\text{.”}\\ G(x,y) \text{ means, “}x\text{ is the grandmother of }y\text{.”}\\ F(x) \text{ means, “}x\text{ is a woman.”}   \end{equation}$$

Those definitions are only concrete examples to help our understanding. They contribute nothing whatsoever to the mathematics. What matters is how they are logically related to one another. In my family, the following statements of formal logic hold:

1. $$M(x, y)\and P(y, z)\to G(x,y)$$
2. $$F(x)\and P(x, y)\to M(x,y)$$

These are the rules. Suppose we are given the facts are true: $P(Mary, Bill)$, $F(Mary)$, and $P(Bill, Robert)$. Notice that the comma means “and” in this context. We will prove that $G(Mary, Robert)$. In fact, often the goals for our theorem prover are more general. We could ask it to “prove” $G(x, Robert)$, which really means, *show me what is provably allowed to go in for* $x$. Clearly if we can answer the second question, then we can also answer the first. But let’s start small. (In our case there is only one solution anyway.)

We have

$$\displaystyle F(Mary)\and P(Mary, Bill) \and P(Bill, Robert)\to M(Mary, Bill)\and P(Bill, Robert)$$ 

by Rule 2.  Now apply Rule 1:

$$M(Mary, Bill)\and P(Bill, Robert)\to G(Mary, Robert)$$ ,

which is what we set out to prove.

#### Type Inference

Now we switch to the universe of types in programming languages. Suppose we have a fictional programming language that generic data types, that is, data types that can be composed with other types to make new types, e.g. the `List<T>` type and the `int` type can be composed as  `List<int>`. If the generic type `e` is of type `(int, string)` (like `HashMap<U, V>` can have type `HashMap<int, string>`), for example, then we will express this fact with a colon: `e: (int, string)` means `e` has type `(int, string)`. Suppose the language has the following *typing rules*:

1. If `m: (X, Y)` and `p: (Y, Z)`, then `g: (X, Z)`.
2. If `p: (X, Y)` and `f: X`, then `m: (X, Y)`.

Typing rules like this come from the semantics of the language. They come from things like subtyping rules, type coercions, definitions of built-in operators, and other language constructs that establish logical relationships between types. Now suppose we have the following variables with types: `p1: (int, float)`, `f: int`, and `p2: (float, bool)`. Our goal is to infer the type of `g`.

Since  `p1: (int, float)` and `f: int`, Rule 2 says that `m: (int, float)`. But then we have  `m: (int, float)` and `p2: (float, bool)`, so by Rule 2, `g: (int, bool) `. 

## Interpreting a program as a strategy for automated proof

Now that we know *what* we are attempting, we need to figure out *how*. Our strategy has the very fancy sounding name *selective linear definite clause resolution with negation as failure.*  The algorithm that allows us to do this efficiently is called *unification*. The process of proving $G(Mary, Robert)$ and infering `g: (int, bool)` is the same. In both cases we found a series of substitutions we could make to convert the given facts into the goal. We *unified* the facts with the goal. The idea is to match up subexpressions and substitute more specific expressions for generic expressions as we go. In more complicated examples, it’s possible to have multiple choices of substitutions to make, and it can happen that all, some, or none of the substitutions lead to a solution.

We will encode our initial facts and goal in such a way that they can be compiled into a program to compute, using the initial facts, a proof of the goal clause. We are trying to answer the question, “Given this set of facts, is this other fact true?” We call this question, encoded into a statement of logic, the *query*. The computation that the compiled program does computes the answer to the query.

This is why we are actually building an interpreter—a virtual machine, actually—for a programming language. It turns out that the programming language won’t be Turing complete until we’re almost finished, but even in the very first version it will have the essential kernel of the finished product.

## The code from 10,000 feet

The program begins as a primitive interpreter running on a register based virtual machine. Most of the effort is spent on the basic parsing and tokenization facilities, as simple as they are. The compilation pipeline is:

```
text -> [`parser::parse`] -> `Term`s ->⋯

    ┌───────────────[`token::flatten_term`]────────────────┐
⋯->*│*-> [`TermIter`] -> `Cell`s -> [`order_registers`] ->*│*->⋯
    └──────────────────────────────────────────────────────┘

⋯-> `Token`s -> [`compile_tokens`] -> `Cell`s & instructions ->⋯

⋯-> [`unify`/execute] -> Success/Fail
```

In $M_0$, the first version of our virtual machine, our primary interest is implementing the essential mathematical algorithm and building the elementary infrastructure: parsing, intermediate representations, outputing data, and so forth. The algorithm is actually the easiest part. Most of the work will be spent slogging through the relatively straightforward parts—although I find those parts really fun to implement, too. In $M_0$, the `[unify]` step is actually part of `[compile_tokens]` and interprets the instructions to build the in-memory `Cell`s. The apparently redundant conversion to `Cell`s, then `Token`s, and back to `Cell`s again will be justified—I hope—when we get to it.

In $M_1$, the second VM, we keep the same algorithm but focus on endowing the VM with a binary bytecode that is executed seperately rather than interpreted during tokenization. Indeed, subsequent iterations are really about increasing the sophistication of the VM itself, albeit in the service of a more sophisticated programming environment. 

# Further Reading

This whole project was inspired by Hassan Aït-kaci's charming book *[Warren's Abstract Machine: A Tutorial Reconstruction](http://wambook.sourceforge.net/).* The Wikipedia articles on these and related topics are generally very good and well cited.

* [Resolution](https://en.wikipedia.org/wiki/Resolution_(logic))
* [Selective Linear Definite Resolution](https://en.wikipedia.org/wiki/SLD_resolution#SLDNF)
* [Negation as Failure](https://en.wikipedia.org/wiki/Negation_as_failure)
* [Unification](https://en.wikipedia.org/wiki/Unification_(computer_science))
* [Curry-Howard Correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence#Related_proofs-as-programs_correspondences)
* [Horn Clause](https://en.wikipedia.org/wiki/Horn_clause)
* [SMT Solvers / Satisfiability Modulo Theories](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories)
* [Turing Completeness](https://en.wikipedia.org/wiki/Turing_completeness)

