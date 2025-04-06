# Azalea
Toy language written in Rust that compiles to Javascript.

## Rationale
I wanted a language that I can personally use for a lot of small projects, so targeting Javascript was a necessity.

Core design decisions:
- You should almost *never* need to specify types, despite being static typed
- Everything is an expression, with few exceptions
- Records map 1:1 to Javascript objects

## Syntax

### Let and mut
`let` immutably binds a name within the current scope, `mut` mutably binds.
```rs
let name: Ty = value
let name = value

mut count = 0
```

### Functions
`fn` is used for declaring functions. They can either return a single expression, or a block of statements. 
`fn` is also used as a type for higher-order functions i.e `fn(int -> int)`.

```lua
fn greet(name: string) = "Hello " ++ name
fn blocky() = do 
  ..
end

fn adder(g: int, h: int) = g + h
fn apply_adder(f: fn(int -> int), g: int, h: int) = f(g,h)

-- Lambda functions
let add_lam = (x, y) => x + y
apply_adder(adder, 1, 2) -- => 3
apply_adder(add_lam, 1, 2) -- => 3
```

### Records and enums
`record` is used to define a record (or struct), and `enum` defines an enumeration.

```cs
record Counter = {
  count: int
}

enum Colours = {
    red,
    green,
    blue
}

fn increment(c: Counter) = c.count += 1
let primary_colour = Colours.red
```

### Typeclasses
`class` is used to define a typeclass. Typeclasses are similar to Java/C# interfaces
and are used as a form of type-safe dispatch. 

`impl` implements an instance of a typeclass for a specified type.

```lua
record MyType = {}

class Show A do
  fn show(a: A): string
end

impl Show MyType do
  fn show(a: MyType) = "MyType"
end

-- print is defined as fn print(a: A) where A: Show .. end
-- so it works on anything that has a `Show` instance
```

