# Full autograd in Haskell

Full autograd example using dual numbers and a simple not-very-type-safe list based AST

In [Main.hs](https://github.com/fabricatedmath/arca/blob/main/sandbox/full-autograd/Main.hs), using these functions:
``` Haskell
f :: Floating a => a -> a
f x = cos (x^2)

g :: Floating a => a -> a
g x = x^2 + 3*x^3 + cos (x^2)
```
outputs this

```
Running f..

Answer at 2.0 is -0.6536436

Real Part:

AST "cos"
|
`- AST "*"
   |
   +- Variable "x"
   |
   `- Variable "x"



Imag Part(Derivative):

AST "negate"
|
`- AST "*"
   |
   +- AST "+"
   |  |
   |  +- AST "*"
   |  |  |
   |  |  +- LiteralFloat 1.0
   |  |  |
   |  |  `- Variable "x"
   |  |
   |  `- AST "*"
   |     |
   |     +- Variable "x"
   |     |
   |     `- LiteralFloat 1.0
   |
   `- AST "sin"
      |
      `- AST "*"
         |
         +- Variable "x"
         |
         `- Variable "x"

Running g..

Answer at 2.0 is 27.346355

Real Part:

AST "+"
|
+- AST "+"
|  |
|  +- AST "*"
|  |  |
|  |  +- Variable "x"
|  |  |
|  |  `- Variable "x"
|  |
|  `- AST "*"
|     |
|     +- LiteralInteger 3
|     |
|     `- AST "*"
|        |
|        +- AST "*"
|        |  |
|        |  +- Variable "x"
|        |  |
|        |  `- Variable "x"
|        |
|        `- Variable "x"
|
`- AST "cos"
   |
   `- AST "*"
      |
      +- Variable "x"
      |
      `- Variable "x"



Imag Part(Derivative):

AST "+"
|
+- AST "+"
|  |
|  +- AST "+"
|  |  |
|  |  +- AST "*"
|  |  |  |
|  |  |  +- LiteralFloat 1.0
|  |  |  |
|  |  |  `- Variable "x"
|  |  |
|  |  `- AST "*"
|  |     |
|  |     +- Variable "x"
|  |     |
|  |     `- LiteralFloat 1.0
|  |
|  `- AST "+"
|     |
|     +- AST "*"
|     |  |
|     |  +- LiteralInteger 0
|     |  |
|     |  `- AST "*"
|     |     |
|     |     +- AST "*"
|     |     |  |
|     |     |  +- Variable "x"
|     |     |  |
|     |     |  `- Variable "x"
|     |     |
|     |     `- Variable "x"
|     |
|     `- AST "*"
|        |
|        +- LiteralInteger 3
|        |
|        `- AST "+"
|           |
|           +- AST "*"
|           |  |
|           |  +- AST "+"
|           |  |  |
|           |  |  +- AST "*"
|           |  |  |  |
|           |  |  |  +- LiteralFloat 1.0
|           |  |  |  |
|           |  |  |  `- Variable "x"
|           |  |  |
|           |  |  `- AST "*"
|           |  |     |
|           |  |     +- Variable "x"
|           |  |     |
|           |  |     `- LiteralFloat 1.0
|           |  |
|           |  `- Variable "x"
|           |
|           `- AST "*"
|              |
|              +- AST "*"
|              |  |
|              |  +- Variable "x"
|              |  |
|              |  `- Variable "x"
|              |
|              `- LiteralFloat 1.0
|
`- AST "negate"
   |
   `- AST "*"
      |
      +- AST "+"
      |  |
      |  +- AST "*"
      |  |  |
      |  |  +- LiteralFloat 1.0
      |  |  |
      |  |  `- Variable "x"
      |  |
      |  `- AST "*"
      |     |
      |     +- Variable "x"
      |     |
      |     `- LiteralFloat 1.0
      |
      `- AST "sin"
         |
         `- AST "*"
            |
            +- Variable "x"
            |
            `- Variable "x"
```
