# HLisp - A Lisp Interpreter in Haskell

HLisp is a lightweight Lisp interpreter written in **Haskell**, designed as a learning project to 
explore interpreter implementation, functional programming, and Lisp semantics.

A set of blogposts accompanies this repository:

* [Part 1](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-1.html)
* [Part 2](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-2.html)
* [Part 3](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-3.html)
* [Part 4](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-4.html)
* [Part 5](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-5.html)
* [Part 6](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-6.html)
* [Part 7](https://tuttlem.github.io/2025/02/15/writing-your-own-lisp-interpreter-in-haskell-part-7.html)

**Current Features**:

- **Lisp-like syntax** for expressions and function definitions.
- **First-class functions** (lambda, higher-order functions).
- **Recursion support** (e.g., `factorial` and `fib` work!).
- **Arithmetic operations** (`+`, `-`, `*`, `/`).
- **Floating point support** (e.g., `sin`, `cos`, `exp`, `sqrt`).
- **List manipulation** (`map`, `filter`, `foldl`, `foldr`, `sort`).
- **Boolean logic** (`and`, `or`, `not`).
- **Comparison operators** (`<`, `<=`, `=`, `>=`, `>`).
- **Pairs and lists** (`cons`, `car`, `cdr`).
- **Quote syntax** (`'x` expands to `(quote x)`).

---

## Getting Started

### Clone the repository

```shell
git clone https://github.com/tuttlem/hlisp.git  
cd hlisp
```

### Build the project

HLisp uses **Stack** for Haskell dependency management and builds.  

```shell
stack build
```

### Run the REPL

```shell
stack exec hlisp-exe

You'll see:  
Welcome to HLisp!  
λ>
```

You're ready to run Lisp expressions!

---

## Examples

### Basic Arithmetic

```lisp
(+ 3 5)        ;; 8  
(- 10 4)       ;; 6  
(* 2 3)        ;; 6  
(/ 7 2)        ;; 3.5
```

### Defining Variables

```lisp
(define x 42)  
x               ;; 42
```

### Defining Functions

```lisp
(define (square x) (* x x))  
(square 4)      ;; 16
```

### Recursion

```lisp
(define (factorial n)  
(if (<= n 1)  
1  
(* n (factorial (- n 1)))))

(factorial 5)  ;; 120
```

### Working with Lists

```lisp
(define nums '(1 2 3 4 5))

(map (lambda (x) (* x x)) nums)   ;; (1 4 9 16 25)  
(filter (lambda (x) (> x 2)) nums) ;; (3 4 5)  
(foldl + 0 nums)                  ;; 15  
(reverse nums)                     ;; (5 4 3 2 1)
```

### Floating Point Math

```lisp
(sin 0.0)       ;; 0.0  
(cos 0.0)       ;; 1.0  
(exp 1.0)       ;; 2.718281828  
(log 10.0)      ;; 2.302585092  
(sqrt 16.0)     ;; 4.0
```

### Boolean Logic

```lisp
(and #t #f)   ;; #f  
(or #t #f)    ;; #t  
(not #t)      ;; #f
```

### **Project Structure**

I've tried to keep the layout of this project simple:

```
/hlisp  
│── /src  
│   ├── Main.hs       # REPL entry point  
│   ├── Eval.hs       # Evaluator (interpreter logic)  
│   ├── Parser.hs     # Lisp parser  
│   ├── Expr.hs       # AST definitions  
│── hlisp.cabal       # Haskell project file  
│── stack.yaml        # Stack configuration  
│── README.md         # This file!
```

## License
HLisp is released under the **MIT License**. 

Feel free to modify and distribute!

