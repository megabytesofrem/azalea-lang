#set page(
  paper: "us-letter",
  numbering: "1",
)

// Title page
#align(center, text(16pt)[
  *Azalea Type System: Documentation*
])

#grid(
  columns: 1fr,
  rows: auto,
  row-gutter: 0.8em,
  align(center)[
    The Azalea Team
  ],
  align(center)[
    #datetime.today().display()
  ]
)


#let forall(a, b) = emph("∀" + a + ". " + b)

= Abstract
Azalea is a modern programming language that draws inspiration from languages such as Haskell, Lua and the ML family. As such, Azalea employs a variant of the Hindley-Milner type system, specifically _Algorithm W_; a well established type inference algorithm used in languages like Standard ML, Ocaml and Rust to provide parametric polymorphism.

This paper describes the type system in detail and how it is used and implemented in Azalea.

The main intention of this paper is to serve as a form of documentation for myself when developing the type system, as it is a fairly complex system with lots of moving parts. However, since Azalea borrows its type system from languages in the ML family (and partially Haskell), I have liberally used the syntax from those languages to aid understanding.

== Haskell Syntax: A quick primer
If you are not familiar with Haskell syntax, here is a quick primer on the syntax used in this paper.

#block(
  inset: 8pt,
  fill: rgb("#fff6da"),
  width: 100%,
  stroke: black,
  [
    - `forall a. a → a` is a polymorphic type, meaning it can work with any type _a_. It is equivalent to `∀a. a → a`.

    - `id : forall a. a → a` is a function declaration noting that `id` is a polymorphic function that takes one argument of type _a_ and returns a value of the same type _a_. It is equivalent to `id : ∀a. a → a`.

    - `List a` is a type constructor that takes a type _a_ and returns a new type.

  ],
)


#pagebreak()

== Type Variables
A type variable is a placeholder for a type to be instantiated later. In the type `List[`_a_`]`, _a_ is a type variable. We say that `List` is polymorphic over _a_.

This really means `List` is a *generic type construction* that can work with any type _a_.

== Type Constructors
A type constructor is a function that takes one or more types and returns a new type. `List` is a type constructor that takes a type _a_.

Records and enum types are also *nominal types*, and are
defined as type constructors e.g `TyCons(List, [TyVar(A)])` or in Haskell syntax `TyCons (List a)`.

=== Why are records and enums type constructors?
Records and enums are defined as type constructors because they can be parameterized by types.

In addition, this allows us to simplify the type system by not having to care about their structure during type checking. Instead, we can treat them as type constructors that take type variables as parameters.

= Type Inference Process
Type inference is performed by Azaleas type checker. The process is as follows.

1. *Assign type variables*: Assign type variables to each expression or subexpression.
  - Example: For a function `id : `_a_ $arrow$ _a_ we assign a type variable _t0_ to the argument and return type, so `id` has type _t0_ $arrow$ _t0_.

2. *Generate constraints*: Generate constraints that map type variables to types based on how expressions are used.
  - Example: For the expression `id(42)`, we generate a constraint that _t0_ must be `Int`, resulting in the constraint _t0_ $eq$ `Int`.

3. *Unification*: Solve the constraints by unifying types.
  - Example: Assume we have the constraints _t0_ $eq$ `Int` and _t0_ $eq$ `String`, we unify them to find a common type. This is not always possible, and if it fails, the type checker reports a type error.

4. *Generalization*: When a value is assigned to a variable, the type checker generalizes its type by quantifying type variables with $forall$. In order to do this, we need to find all the free type variables
  - Example: If `id` is inferred to have type _t0_ $arrow$ _t0_, it is generalized to $id$ : #forall("a", "a → a"), meaning it can work with any type _a_.

5. *Instantiation*: When a polymorphic function is used, the type checker instantiates it with a specific type, narrowing it down.


  Hydration/substitution is performed, replacing type variables with their instantiated types.

  - Example: If `id` is used with an `Int`, it is instantiated to `id` : `Int` $arrow$ `Int` from #forall("a", "a → a").

6. *Recursion*: The whole process is recursive, meaning that type inference can handle nested expressions and complex types.

#pagebreak()
== Example
Let's consider a simple example to illustrate the type inference process.

Suppose we have `head` : #forall("a", "List[a] → a").

1. `head` is assigned a type variable _t0_ for the argument and return type.
2. When we use `head` with a list of integers, we generate the constraint _t0_ $eq$ `Int`.
3. Unify _t0_ with `Int`, resulting in the type `head` : `List[Int] → Int`.
4. Head is generalized to `head` : #forall("a", "List[a] → a"), meaning for any type _a_, `head` can work with a list of _a_.

When head is used with a list of integers:

#block(
  inset: 8pt,
  fill: rgb("#fff6da"),
  width: 100%,
  stroke: black,
  [
    The environment contains:
    - `head` : #forall("a", "List[a] → a") (polymorphic function)
    - _t0_ : `Int` (type variable for the argument)
  ],
)

1. Instantiation:
  The type variable _t0_ is instantiated to `Int`, resulting in the type `head` : `List[Int] → Int`.

2. Inference:
  When we call `head([1, 2, 3])`, the type checker checks the argument against the type of `head`.

  It infers that the argument is a `List[Int]` and it returns an `Int`.

3. Unification:
  The type checker unifies `List[t1]` with `List[Int]` which means _t1_ $eq$ `Int`.

4. Substitution:
  The type variable _t1_ is substituted with the final type `Int`.

#pagebreak()
== Unification Rules
For any two types _t1_ and _t2_, the unification rules are as follows:
- *Equivalence*: If _t1_ and _t2_ are the same type, they unify.
- *Type variables*: If _t1_ is a type variable and _t2_ is not a type variable, _t1_ is unified with _t2_ by way of a substitution.
- *Type constructors*: If _t1_ and _t2_ are type constructors, first occurs check their type parameters and then perform \unification on their type parameters.
- *Function types*: If _t1_ is a function type (_fn_) and _t2_ is a function type (_fn_), unify their argument and return types.
- *Array types*: If _t1_ is an array type and _t2_ is an array type, unify their element types.
- *Record types*: If _t1_ and _t2_ are record types, first check the lengths of both records, and then perform unification on their fields if the names match.

