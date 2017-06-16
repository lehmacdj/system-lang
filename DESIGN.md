# System-Lang

# Expressions
Expressions are as defined as follows in System-Lang:
1. A numeric literal
2. A name
3. An application of one expression to another expression
4. A tuple
5. A list
6. A block
7. Any other expression preceded by a tag

# Tags
Tags are a kind of attribute that can be applied to any language component. They
can appear anywhere before an expression. They can modify how expressions are
parsed, typed, and evaluated. Tags are frequently used to define attributes of
certain kinds of expressions.

For example:
```
#do {
    x <- readLine
    putStrLn "Hello " ++ x
}
```
could be used as `do` notation from haskell.

Collections can use attributes on the list constructor similarly:
```
#set [1, 2, 3, 4]
```

Tags are a compile time construct and thus are strongly normalizing to guarantee
that compilation terminates.

If multiple are placed in a row they coalesce; precedence is established based
on the order they appear.
