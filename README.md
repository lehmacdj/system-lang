# System
A simple, low level language with a few carefully selected high level features

## Design
- values are "just bits"
- values can be given Attributes
- attributes tell the compiler what a value can do
- contexts define how certain code is evaluated

### Values
- some number of bits

| type | description |
|------|-------------|
| bit | just a single bit |
| byte | a byte, eight bits |
| word | a machine word (e.g. 32 bits on x86 and 64 bits on x86\_64) |

### Array Types
- arrays allow composing several primitive types
- primitive types are defined in terms of array types of bits

```
byte = bit[8]
// on x86_64
word = bit[64]
```

#### Indexing
- array values can be indexed
- values are zero indexed
- can be though of as pointer arithmetic (more on this later)
- values are indexed based on the type they are constructed from

```
// x : bit[8]
x[3] // the fourth bit of x
```

```
// x : byte[3]
x[2] // the third byte of x
```
### Tuples
- ways of wrapping values together easily
- stored in memory completely adjacently to one another
- an anonymous struct with no member names
- index to access members

```
// x : (byte, word, byte)
x[1] // access second element
x[2] // access third element
```

#### Labels
- represents a memory offset
- implicitly computed based on the front of a tuple
- can be used in an index
- very similar to relative offsets used for jump tables

```
// x : (hello: bit[7], world: byte[4])
x[hello] = x[0]
x[world] = x[1]
```

#### Structures
- a kind of typedef
- not called a typedef because it is actually only a size def

```
struct stuff = (hello: bit[7], world: byte[4])

// x : stuff
x[hello] // the same as x but now indexed by bits
x[world] // 7 bits from start of x indexed by bytes
x[3] // doesn't make any sense and is invalid
```

#### Pointers
- the address of memory locations
- pointers are just a value that happens to represent a memory location
- the `@` operator allows using a pointer to get the value at that location in
  memory
- the address of a location is obtained by indexing a value with `@`
- pointers are always the length of pointers on the current machine (word)

```
// x : byte
x[@] // the address of x
@x[@] // the same as x
```

#### Casting
- casting makes no effort to change the value
- only the interpretation of the value changes

```
// x : byte
x#bit[3] // x interpreted as a bit then indexing its fourth bit
```

### Functions
- functions are pretty important I guess
- every function takes a tuple as an argument and returns a tuple as a value
- parameters are passed on the call stack
- returned values are pushed onto the call stack after parameters are removed
- returns on the last line of the function are implicit
- can be declared anywhere but cannot capture stuff from the environment
- captured values would have to be passed as a parameter

```
// takes two arguments and returns the second
foo: word = (a: byte, b: word) {
    b
}
// alternatively could be written like this
foo : (byte, word) -> word
foo (a, b) = b

// a pointer to foo (e.g. the address of the location of the code)
foo[@]
```

### Attributes
- obviously it might seem limiting to only have bits
- attributes allow bits to be extended with additional capabilities
    - for example addition, subtraction, multiplication, etc.
- every operation on values is implemented as a value (even builtin ones that
  work on every value)

#### Bitwise
- provides operations for working with a value as a sequence of bits
- these operations exist for every value by default
- or `|`
- and `&`
- xor `^`
- not `~`
- shl `<<`
- shr `>>`
- rotate `><`

#### Number
- allows a series of bits to be viewed as a number
- default implementations are provided for types as they make sense
- add `+`
- subtract `-`
- multiply `*`
- divide `/`

#### Defining Attributes

### Tags
- a tag to give the compiler some kind of directive
- can be applied to a single statement, declaration, or an entire block
- special semantics can be applied by a tag

```
// when testing run the following comparisons
#test {
    x == 1 || fail
    y == 0 || fail
}

// only execute the statement in debug mode
#debug println("????")
```
