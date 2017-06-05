# _S-_
_S-_ Is no more than a simple language that do nothing interessing

## Syntax
The language is constitued of a list of statement. Those statement can be either an expressions or assignements.

### Identifiers

An _identifier_ start with letter/number/symbol/'\_' and end with zero or many letter/number/symbol/'/'\_' See SDef in src/Parse.hs.

### Assignements

An _assignements_ is of the form `ident = expr` and set the value of `ident` to the value of `expr`, where `ident` is an identifier and `expr` an expression. The assignements return the value of `expr`. 
This allow to do things as `id1 = id2 = id3 = 3.1415962`.

### Expressions

An _expression_ is a statement that will compute and return a value. To compute a value you'll mainly use functions.
To call a function : `funcname arg1 arg2 ... argn` where funcname is the name of the function and arg1 arg2... are expression.

To create sub-expressions you can either use parentheses or `$`. 

```
S~> * 5 $ + 9 $ - 5 7
Int :: 35
S~> * 5 (+ 9 (- 5 7))
Int :: 35
```

### Creating functions

You can declare functions using the construction `func '(args...){code...}` where `args` is a list of identifiers and code a list of statements.
The return value of the function will be the return value of the last statement.
Functions are already currified. That means that you can create new functions by applying partially a function

Exemple: 
```
S~> const = func '(a b){a}
S~> const4 = const 4
S~> const4 5
Int :: 4
```

## Types

### None
Just `None` value 

```
S~> None
S~>
```

### Numerical types
Double: `1.0`
Int: `1`

When you apply +/-/\* on differents types of numbers, values will be converted in the most 'general type'
```
S~> * 1.0 2
Double :: 2.0
```

### Boolean
Either `True` or `False`.

/!\\ True/False are just identifiers and can be redefined !

### List
An heterogenous list.

Can be created easily but no op on them for the moment.
This should not be used for the moment because it sucks a lot.
```
S~> a = 1 2 3
List :: [Int :: 1,Int :: 2,Int :: 3]
S~>
S~> # But it sucks... A lot
S~> b = 1 (2 3 4) 5
List :: [Int :: 1,Int :: 2,Int :: 3,Int :: 4,Int :: 5]
S~> b = 1 a 5
List :: [Int :: 1,Int :: 1,Int :: 2,Int :: 3,Int :: 5]
S~> 
```

### Quoted
A list of identifiers. Used mainly (only) in function declaration.
Identifiers are not evaluated and replaced by their values

```
S~> '(a b c)
Quoted :: '(Ident :: a Ident :: b Ident :: c)
```

### Code Block
Used only in function declaration, a block of non evaluated code.
Can be evaluated using `ap (func '() blk)`
You can also a function that with evaluate it for you.

``` 
S~> eval = func '(blk){ap (func '() blk)}
S~> a = 1 
S~> b = 2
S~> eval {+ a b}
Int :: 3
```

## Existing functions
Look in src/DefaultEnv.hs

- `+`/`-`/`*`
- `env` -> show current scopes (globals, locals...)
- `func` -> create new functions
- `show` -> force showing a value
- `ap` -> apply a function (useful when the function take any arg as env)
- `arity` -> return the arity of the function (n° of args that the function accept)
- `type` -> return the type of the value

