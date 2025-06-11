+++
title = "Writing a truth oracle in Lisp"
date = 2025-06-08T00:00:00-04:00
draft = false
[taxonomies]
+++

<div class="note">

This post assumes some familiarity with typed functional programming, Lisp, and formal logic.

</div>

<p class="first-paragraph" data-first-letter="T">
Today we will attempt to write a truth oracle in Lisp.
By "truth oracle," I mean a program that can determine whether arbitrary mathematical statements are true or false.
This might sound impossible, due to first-order logic being undecidable,
but let's try anyway.
</p>

Before that, though, we need to go over some required concepts.


## Extracting information from proofs {#extracting-information-from-proofs}

First, sometimes, we can extract information from proofs themselves, beyond just the facts that they prove.

Imagine you want to prove a statement of the form "\\(A\\) or \\(B\\)."
If you can prove either \\(A\\) or \\(B\\), then you can prove "\\(A\\) or \\(B\\)" by the [or-introduction](https://en.wikipedia.org/wiki/Disjunction_introduction) rule.
Let's say you have a proof of this form,
and you want to figure out _which_ of \\(A\\) or \\(B\\) is the one that's true.
This is pretty easy.
You can just look at the last step of the proof,
and see if it's _left-or-introduction_ or _right-or-introduction_.
For example, if you have a proof that a number is either divisible by three or by five, you can look at the last step of the proof to see which it's divisible by.

The same principle applies for proofs of existence.
If you want to prove "there exists \\(x\\) such that \\(P(x)\\),"
you can use the [existential introduction](https://en.wikipedia.org/wiki/Existential_generalization) rule.
In other words, you can give the specific value \\(x\\) satisfying \\(P(x)\\), and then prove \\(P(x)\\) with that value.
Then if you have a proof of this form that there exists a number \\(x\\) such that there is a string of digits "123456789" starting at the \\(x\\)th digit of \\(\pi\\),
you can look at the existential introduction rule of the proof to see _where_ in \\(\pi\\) you can find the digit string "123456789."

In other words, for proofs of "or" statements, we can look at the proof to see which individual statement is true, and for proofs of existence we can look at the proof to see the value that exists.


## The Curry-Howard correspondence {#the-curry-howard-correspondence}

The next concept is a relationship between proofs and programs called the
[Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).
This topic has a _lot_ of depth and I'm not going to go into the full details, but the basics aren't too complicated.
The core idea is that formal logic _proofs_ can be thought of as _expressions_ in a typed functional programming language, and vice versa.
Similarly, the _types_ of expressions can be thought of as _propositions_, or statements, that the expressions prove.
I'll describe some examples below.


### Implication {#implication}

Consider the statement "\\(A\\) implies \\(B\\)."
If you have a proof that "\\(A\\) implies \\(B\\)," and a proof of \\(A\\),
you can combine them with [implication elimination](https://en.wikipedia.org/wiki/Modus_ponens) to get a proof of \\(B\\).
In the Curry-Howard correspondence, implication corresponds to function types,
and implication elimination corresponds to function application.
If you have an expression \\(f\\) of type \\(A \to B\\) and an expression \\(x\\) of type \\(A\\),
you can combine them with function application to get an expression \\(f(x)\\) of type \\(B\\).
Incidentally, the right arrow "\\(\to\\)" symbol is commonly used for both implication and function types.


### And {#and}

You can prove "\\(A\\) and \\(B\\)" using [and-introduction](https://en.wikipedia.org/wiki/Conjunction_introduction) if you can prove \\(A\\) and you can prove \\(B\\).
You can also use [and-elimination](https://en.wikipedia.org/wiki/Conjunction_elimination) with a proof of "\\(A\\) and \\(B\\)" to get back a proof of \\(A\\) and a proof of \\(B\\).
In functional programming, this corresponds to tuples.
Given an expression \\(x\\) of type \\(A\\) and \\(y\\) of type \\(B\\),
you can construct a tuple \\((x,y)\\) of type \\((A,B)\\).
And you can also destruct a tuple of type \\((A,B)\\) to get back \\(x\\) and \\(y\\).


### Or {#or}

With [or-introduction](https://en.wikipedia.org/wiki/Disjunction_introduction), you can prove "\\(A\\) or \\(B\\)" from a proof of \\(A\\) or a proof of \\(B\\).
In programming languages, this corresponds to [sum types](https://en.wikipedia.org/wiki/Tagged_union) (types that can be one of several variants), sometimes called "either types," variants, discriminated unions, or coproducts.
In Rust, these are called `enum`, or `Result<T, E>` for the specific case where one of the types is for normal output and the other is for errors.
In Haskell, there is a type `Either A B` with constructors `Left a` and `Right b`.
In Rust, if you have an expression `val` of type `T` or an expression `err` of type `E`,
you can construct an expression `Ok(val)` or `Err(err)` of type `Result<T, E>`.
In Haskell, if you have `a : A` or `b : B`, then you can construct `Either A B` with `Left a` or `Right b`.

The opposite direction, [or-elimination](https://en.wikipedia.org/wiki/Disjunction_elimination), is a little more complicated.
Given a proof of "\\(A\\) or \\(B\\)," if you prove that both \\(A\\) and \\(B\\) imply the same conclusion \\(C\\), then you can use or-elimination to prove \\(C\\).
This corresponds to pattern matching in programming languages.

In Haskell, if you have a value `x` of type `Either a b`
and you have functions mapping both `a` and `b` to a shared type `c`,
you can use pattern matching to get a value of type `c`.

```haskell
patternMatch :: Either a b -> (a -> c) -> (b -> c) -> c
patternMatch x f g =
  case x of
    Left v -> f v
    Right v -> g v
```

And in Rust, if you have a value `x` of type `Result<A, B>`
and you have functions mapping both `A` and `B` to a shared type `C`,
you can use pattern matching to get a value of type `C`.

```rust
fn pattern_match<A, B, C>(
    x: Result<A, B>,
    f: fn(A) -> C,
    g: fn(B) -> C
) -> C {
    match x {
        Ok(v) => f(v),
        Err(v) => g(v),
    }
}
```


### Contradiction {#contradiction}

A contradiction "\\(\bot\\)" is a proposition that has no proofs (i.e. it's unprovable, and false).
Propositions correspond to types,
and proofs correspond to values.
So a contradiction corresponds to an [empty type](https://en.wikipedia.org/wiki/Empty_type),
a type with no values.
In programming languages with sum types, this can be represented with a sum type with no variants, such as [`enum Void {}`](https://docs.rs/void/1.0.2/void/enum.Void.html) in Rust.
However, Rust also includes a [primitive "never" type `!`](https://doc.rust-lang.org/std/primitive.never.html) which serves this purpose.
These types are typically used for expressions that don't return normally,
such as infinite loops, thrown exceptions,
or fatal errors.

In formal logic,
if you have a proof of a contradiction,
you can use it to prove anything,
by the [principle of explosion](https://en.wikipedia.org/wiki/Principle_of_explosion).
Similarly,
in functional programming,
an expression of the empty type can be used to get an expression of any type.
If the empty type is represented as a sum type with zero variants, you can pattern match on the expression to get an expression of any arbitrary type.
In the case of Rust's "never" type, it is coerced automatically to other types.


### Negation {#negation}

Logical negation "\\(\lnot A\\)" ("not \\(A\\)") is equivalent to implying a contradiction "\\(A \to \bot\\)" ("\\(A\\) implies a contradiction").
We can say this because
the core property we want for logical negation is that
if we know both a statement and its negation,
we can derive a contradiction.
For example,
if we know "\\(A\\)" and "\\(A\\) implies a contradiction" we can use
[implication elimination](https://en.wikipedia.org/wiki/Modus_ponens)
to derive a contradiction.
We've already shown that implication corresponds to function types, and contradictions correspond to empty types,
so we now know that logical negation is isomorphic with function types where the return type is an empty type.


### Summary {#summary}

Here's a table summarizing the correspondence, adapted from the one on [Wikipedia](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence).

| Logic              | Typed functional programming                                   |
|--------------------|----------------------------------------------------------------|
| Statement          | Type                                                           |
| Proof              | Expression/Value/Program                                       |
| Statement is true  | Type has some possible values                                  |
| Statement is false | Type has no possible values                                    |
| Implication        | Function type                                                  |
| And (conjunction)  | Tuple                                                          |
| Or (disjunction)   | Sum type                                                       |
| \\(\bot\\)         | Empty type                                                     |
| not \\(A\\)        | Type of function that takes \\(A\\) and returns the empty type |


## Truth oracle in Lisp {#truth-oracle-in-lisp}

Now that we understand how proofs in formal logic correspond to programs,
we can use this knowledge to build a function that tells us whether any logical statement is true.

Now, we know there's a correspondence between proofs and programs, and that we can extract information from proofs, such as whether \\(A\\) or \\(B\\) is true from a proof of "\\(A\\) or \\(B\\)." As it turns out, typed Racket (a Lisp with static typing) has a function `call/cc` that is Curry-Howard-isomorphic with [Peirce's law](https://en.wikipedia.org/wiki/Peirce%27s_law).
This is an axiom of logic that says that "\\(((A \to B) \to A) \to A\\)."
It happens to be equivalent to [the law of excluded middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle),
which says that every statement is either true or false (\\(A\\) or not \\(A\\)).
Since you can use Peirce's law to prove the law of excluded middle,
we should be able to use `call/cc` to implement a program that is Curry-Howard-isomorphic to the law of excluded middle.
Let's try that.

First, we'll use [Rocq Prover](https://rocq-prover.org/) to prove that Peirce's law implies excluded middle.
Rocq Prover is a tool for writing formal proofs that uses a typed functional language called [Gallina](https://rocq-prover.org/doc/V8.9.1/refman/language/gallina-specification-language.html) as its proof format.
By the Curry-Howard correspondence, this is a valid way to represent proofs.
Don't worry if you aren't familiar with the specifics of Rocq and its syntax;
the key insight is just that we are proving Peirce's law implies the law of excluded middle.
The proof looks like this:

```rocq
Theorem peirce_implies_em :
  (forall A B, ((A -> B) -> A) -> A)
  -> (forall A, A \/ ~A).
Proof.
  intros peirce A.
  apply peirce with (B := False).
  auto.
Qed.
```

This proof is not written in the Gallina functional programming language, but a higher-level tactic language for manipulating the underlying Gallina expressions.
Thankfully, Rocq lets us print out the generated Gallina program.
Let's do that, so we can translate it to typed Racket.

```rocq
Print peirce_implies_em.
```

```rocq
peirce_implies_em =
fun (peirce : forall A B : Type, ((A -> B) -> A) -> A)
    (A : Prop)
    => peirce
         (A \/ ~ A)
         False
         (fun (not_em : A \/ ~ A -> False)
              => or_intror
                 ((fun a : A => not_em (or_introl a)) : ~ A)
         )
```

Notice that the proof is that Peirce's law implies excluded middle,
and the program is a function that takes as an argument a proof of Peirce's law and returns a proof of the excluded middle.
Let's translate the body of the function to typed Racket,
replacing `peirce` with `call/cc`.

```racket
#lang typed/racket

(struct (L) Left ([v : L]))
(struct (R) Right ([v : R]))
(define-type (Either L R)
  (U (Left L) (Right R)))

(define-type (Not A) (-> A Nothing))
(define-type (EM A) (Either A (Not A)))

(: peirce (All (A B) (-> (-> (-> A B) A) A)))
(define (peirce ABA) : A
  (call/cc ABA))

(: em-raw (All (A) (-> (-> A A) (EM A))))
(define (em-raw a_implies_a)
  (peirce (lambda ([not_em : (Not (EM A))]) : (EM A)
            (Right (lambda ([a : A])
                     (not_em (Left a)))))))

(define-syntax-rule (em A)
  (em-raw (lambda ([a : A]) : A a)))
```

Here [`Nothing`](https://docs.racket-lang.org/ts-reference/type-ref.html#%28form._%28%28lib._typed-racket%2Fbase-env%2Fbase-types..rkt%29._.Nothing%29%29) is the empty type corresponding to a contradiction,
`(Either L R)` is a sum type corresponding to logical "or,"
and `(-> A B)` is a function type corresponding to logical implication. Due to what looks like a type inference limitation,
we need our excluded middle function to take an identity function as a parameter,
so we define a macro that takes a type as a parameter
and creates the necessary identity function and passes it to the `em-raw` function to get a proof that `A` is either true or false.

But wait, didn't we show at the beginning that if we have a proof of "\\(A\\) or \\(B\\)," we can look at the last step of the proof to see which one of \\(A\\) and \\(B\\) is true? In this case \\(B\\) is just "not \\(A\\)." We have a function where we can input any statement, and it evaluates to an expression corresponding to a proof that the statement is either true or false. If we print this expression, we can see whether it looks like `(Left ...)` or `(Right ...)` to see whether \\(A\\) is true or false.

We have created our truth oracle in typed Racket.
We have a function that tells us whether any logical statement is true.
If this works, what can we do with this?

We can instantly mine every new Bitcoin,
worth (as of writing this) 48 million dollars per day,
by encoding the statement,
"there exists a [nonce](https://www.investopedia.com/terms/n/nonce.asp) starting with a prefix _p_ that makes this block's hash below the target value,"
in formal logic,
and gradually expanding the prefix while the oracle says the statement is true.
Eventually, the prefix will be the full nonce and you will have mined the block.
Since the nonce is 32-bits, you would only need to call the oracle 32 times,
once for each bit, to figure out if it should be 0 or 1.

We can solve all open problems in mathematics and formally verify our solutions,
by using a similar technique to generate formal proofs.

We can encode all known laws of physics and ask for
solutions to engineering problems.

We can figure out whether any arbitrary Turing machine will halt.

We can compute the [Kolmogorov complexity](https://en.wikipedia.org/wiki/Kolmogorov_complexity) of all available stock market data, combined with possible future data,
to possibly perfectly predict stock price movements, but this is more speculative.

We can replace the inefficient gradient descent local optimization of machine learning with a process that _globally_ chooses the best possible weights, and does so much more quickly, which would let any consumer-grade computer train AI systems much more powerful than leading AI labs.

We can implement [AIXI](https://en.wikipedia.org/wiki/AIXI), an algorithm for artificial general intelligence, implemented as an optimal reinforcement learning agent.

With all these possibilities, let's try using it.

First we'll try something simple, that "false" is false.

```racket
(em Nothing)
```

```racket
#<Right>
```

Here `Nothing` corresponds to a contradiction,
and our oracle correctly says it's false!

Now we'll check "for all statements A, A is true."

```racket
(em (All (A) A))
```

```racket
#<Right>
```

Some things aren't true, so it correctly says it's false!

Next we'll try something more complicated.
Let's see if our oracle thinks 3 is even.

```racket
(em (Refine [x : Natural] (= (* 2 x) 3)))
```

```racket
#<Right>
```

This statement means "there exists natural number \\(x\\) such that \\(2x = 3\\),"
which is equivalent to saying 3 is even,
and it correctly says it's false again!

Now let's try checking if 10 is even.

```racket
(em (Refine [x : Natural] (= (* 2 x) 10)))
```

```racket
#<Right>
```

Oh, this is true but our oracle is saying it's false.
Come to think of it, our oracle has only ever said statements are false.

Let's try something simpler.
Does it think any natural number exists?
Since a type having a value corresponds to a statement having a proof,
the type of natural numbers corresponds to the statement that at least one natural number exists.

```racket
(em Natural)
```

```racket
#<Right>
```

Okay, it looks like our oracle is just saying everything is false.
Maybe there's a bug, or maybe one of our assumptions is wrong?

Wait, ...but there _can't_ be a bug, because the type signature of `em` ensures the behavior we want.
It could be a bug in typed Racket's type checker, potentially.
Let's try to combine the proof of a false statement that it gives us with a proof of its negation, to get a contradiction.
Recall that this is Curry-Howard isomorphic with a type that has no possible values.
If we print out the impossible value that has this type,
we can try to get some idea what's going on here.

First, we'll set up a way to get the inner proof that no number exists.

```racket
(let ([proof : (Either Natural (Not Natural))
       (em Natural)])
  (if (Left? proof)
    (format "A number exists: ~a"
            (Left-v proof))
    (format "No number exists: ~a"
            (Right-v proof))))
```

```racket
"No number exists: #<procedure>"
```

The proof that no number exists is a `#<procedure>` value of type `(Not Natural)`.
Recall from the definition of `Not` that this is equivalent to
`(-> Natural Nothing)`,
so this procedure value corresponds to a proof that a natural number implies a contradiction.
We can pass the function a natural number,
and it will return a value that can't exist.
This shouldn't be possible, but let's see what happens!
We'll just replace `(Right-v proof)` with `((Right-v proof) 283)`.

```racket
(let ([proof : (Either Natural (Not Natural))
       (em Natural)])
  (if (Left? proof)
    (format "A number exists: ~a"
            (Left-v proof))
    (format "Value that shouldn't exist: ~a"
            ((Right-v proof) 283))))
```

```racket
"A number exists: 283"
```

Huh, when we modify the code to create the impossible value,
the oracle now correctly says the statement is true.
But the oracle runs **_before_** the impossible value is created.
And the number it gives us to prove a number exists is the _same number_
we use to create the impossible value in the branch that _doesn't even run_???
Not only is code that runs later in the program influencing code that runs earlier, the code that runs later doesn't run at all, and it also somehow passes a value back in time?
The code somehow knows what number we'll use in the future,
in code that never even runs, and changes its answer accordingly.
What!!?!

---

Okay, it turns out that we didn't really write a truth oracle,
and `call/cc` isn't a normal function.
To understand what's going on,
we need to go over some more concepts.


## Constructive vs classical logic {#constructive-vs-classical-logic}

One detail I glossed over earlier was the distinction between constructive and classical logic.
This is critical to understanding this bizarre behavior.
The "extracting information from proofs" section was a bit misleading.
Specifically, it stops being true once you add certain axioms.
I said that if you have a proof of "\\(A\\) or \\(B\\),"
you can look at the last step of the proof to see if
it's the left-or-introduction or right-or-introduction rule,
to see which of \\(A\\) or \\(B\\) is the one that's true.
But one important detail I left out is this requires that left-or-introduction and right-or-introduction are the only rules to create proofs of logical "or" statements.
There is another rule for proving "or" statements that is commonly added as an axiom, namely, the _[law of excluded middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle)_, the _same rule that we implemented in typed Racket_.

Logic without the law of excluded middle, or anything equivalent to it, is called _[constructive logic](https://en.wikipedia.org/wiki/Intuitionistic_logic)_.
It is called "constructive," because in order to prove "\\(A\\) or \\(B\\),"
you need to decide whether \\(A\\) or \\(B\\) is true and directly construct a proof of that.
Similarly, in order to prove that a value exists that has some property,
you need to give the actual value that exists.
This makes it map nicely to functional programming in the Curry-Howard isomorphism.
Normally to get a value of a sum type, you have to know the specific variant in order to construct it.
For example, to construct a `Result<T, E>` in Rust,
you need to specifically use `Ok()` or `Err()`.

When you add the law of excluded middle, or something equivalent to it,
constructive logic becomes _[classical logic](https://en.wikipedia.org/wiki/Classical_logic)_.
It lets you prove "or" statements without you needing to know which statement is true,
and it lets you prove existence of an object without needing to actually know the object.
Here is an example classical proof to give you an idea of what this is like:

Let's say we want to prove that there exists irrational numbers \\(x\\) and \\(y\\) such that \\(x^y\\) is rational.
We know that \\(\sqrt 2\\) is irrational.
By the law of excluded middle,
we know that \\(\left(\sqrt 2 \right) ^ {\sqrt 2}\\) is either rational or irrational.
We can consider the consequences of each case separately:

1.  If it's rational, then we can say the values of \\(x\\) and \\(y\\) are
    both \\(\sqrt 2\\).
         Then \\(x\\) and \\(y\\) are both irrational and \\(x^y\\) is rational. This is what we wanted to show, so that means the proof is valid for this case.
2.  If \\(\left(\sqrt 2 \right) ^ {\sqrt 2}\\) is irrational,
    then we can say the values of \\(x\\) and \\(y\\) are
    \\(x = \left(\sqrt 2 \right) ^ {\sqrt 2}\\) and \\(y = \sqrt{2}\\).
    Then, \\(x\\) and \\(y\\) are both irrational and

    \\(x^y\\)
    \\(= \left( \left(\sqrt{2}\right)^{\sqrt{2}} \right)^{\sqrt{2}}\\)
    \\(= \left(\sqrt{2}\right)^{\sqrt{2} \times \sqrt{2}}\\)
    \\(= \left(\sqrt{2}\right)^2\\)
    \\(= 2\\),

    which is rational,
    and that finishes the proof.

(Source: [Software Foundations](https://softwarefoundations.cis.upenn.edu/lf-current/Logic.html))

Note that even though we proved that values \\(x\\) and \\(y\\) exist,
we didn't need to prove what they are.
We gave two possibilities, but from the proof alone,
we don't know which possibility is correct.
This is because of the use of the law of excluded middle in the beginning.
That makes it a _non-constructive_ proof, which lets us prove values exist without knowing what they are.


## The `call/cc` function {#the-call-cc-function}

Constructive logic corresponds nicely to functional programming under the Curry-Howard correspondence.
But Racket's `call/cc` function corresponds to
[Peirce's law](https://en.wikipedia.org/wiki/Peirce%27s_law),
which is equivalent to the law of excluded middle,
and thus part of classical logic and not constructive logic.
How does this work?
Well, [`call/cc`](https://en.wikipedia.org/wiki/Call-with-current-continuation) is not a normal function.
It has different control-flow than normally expected in functional programming languages,
which caused the strange behavior in our program.

The full name of `call/cc` is [call-with-current-continuation](https://en.wikipedia.org/wiki/Call-with-current-continuation).
It takes a function as its argument,
and it calls the function,
passing it something called the "current continuation."
For example, `(call/cc (lambda (cc) "foo"))` evaluates to `"foo"`.
But what does the "current continuation" do?

It's a function that takes as input a value,
and rolls the program state _back in time_,
making the `call/cc` expression return that value instead.

Let's look at an example to understand this better:

```racket
#lang racket

(define (main)
  (define continuation null)
  (define value
    (string-append
     "prefix | "
     (call/cc (lambda (cc)
                (set! continuation cc)
                "original value"))))
  (println value)
  (continuation "new value"))

(main)
```

```racket
"prefix | original value"
"prefix | new value"
...
```

In this example program,
we save the continuation in a variable called `continuation` so we can call it easily later.
At first, the `call/cc` expression returns the string `"original value"`,
and then a prefix `"prefix | "` is added and it's saved in a variable called `value`.
When we print it with `(println value)`,
it shows the full string `"prefix | original value"`.
Then we call the continuation with `(continuation "new value")`.
This rolls back the state of the `value` definition,
with the output of the `call/cc` expression changed to `"new value"`.
The definition expression of `value` evaluates again from that point,
adding the prefix `"prefix | "` again.
Then we print `value` again, giving the new value `"prefix | new value"`.

To summarize what happened,
the continuation saved the state of the program,
and when we called it later, it rewound the program back to the saved state and made `call/cc` return a different value.

In other words, the continuation can pass values back in time.
One way to think about this is that `call/cc` both generates a time machine (continuation) and acts as a receiver for future information that get sent backwards in time.
That's why we were seeing the strange behavior from before.

What is this function used for? Why does it exist?
As it turns out,
you can use it to implement control flow constructs that usual functional programming features can't easily represent,
like loops, break statements, return statements, generators, and coroutines.
However, that is beyond the scope of this post,
so let's keep trying to figure out what's going on.


## How the code works {#how-the-code-works}

Now that we understand `call/cc`,
we can return to the code and try to understand how it works.

```racket
(call/cc (lambda ([not_em : (Not (EM A))]) : (EM A)
            (Right (lambda ([a : A])
                     (not_em (Left a))))))
```


### The continuation {#the-continuation}

The `not_em` function is the continuation,
and it has type `(Not (EM A))`.
Recall that this means it is a function that takes an argument
of type `(EM A)` and returns a value of type `Nothing`,
which doesn't actually exist.
Under the Curry-Howard isomorphism,
`not_em` can be interpreted as a proof that "\\(A\\) or not \\(A\\)" implies a contradiction.
Also, empty types like `Nothing` in Racket and the never type in Rust are commonly used for expressions with abnormal control flow that don't evaluate to a value normally, such as `break` statements.
The `not_em` function is a continuation; it doesn't actually return a value, so it makes sense that its return type can be `Nothing`.


### The result {#the-result}

Initially (in the first timeline), the `call/cc` expression
evaluates to

```racket
(Right (lambda ([a : A])
         (not_em (Left a))))
```

The `Right` type constructor corresponds to the "false" part of the excluded middle,
which is why our oracle started out saying everything was false.
The type of this expression is `(EM A)`,
which is the same as `(Either A (Not A))`,
which we can expand further into `(Either A (-> A Nothing))`.

So the expression

```racket
(lambda ([a : A])
  (not_em (Left a)))
```

passed to the `Right` constructor must have type `(-> A Nothing)`.
The parameter does have type `A`,
and the body of the function calls a continuation,
which means the body has type `Nothing`.
So the type of `(-> A Nothing)` makes sense.
Further, since `a` has type `A`,
it must be the case that `(Left a)` has type `(EM A)`.
And `not_em`, the continuation, has type `(-> (EM A) Nothing)`,
so it also makes sense that the `not_em` call type checks.

Okay, that's why it type checks.
What about the behavior?
Initially for a statement `A`,
the oracle always returned a `Right` value,
corresponding to a proof that `(Not A)`,
or `(-> A Nothing)`,
a function that takes `A` and returns `Nothing`.
When we gave the function a value `a` of type `A`,
the state of the program got rolled back to the oracle,
and it returned `(Left a)` instead.

```racket
(Right (lambda ([a : A])
         (not_em (Left a))))
```

This is _exactly_ what this code does.
It returns `Right` with a function that takes an argument `a`,
and calls the continuation with `(Left a)`,
rolling back the state of the program to when the oracle was called
and rerunning it with the new output.


## The Devil of the Excluded Middle {#the-devil-of-the-excluded-middle}

[Philip Wadler has a story](https://homepages.inf.ed.ac.uk/wadler/papers/dual/dual.pdf) about the computational interpretation of the law of excluded middle:

> Once upon a time, the devil approached a man and made
> an offer: "Either (a) I will give you one billion dollars, or (b)
> I will grant you any wish if you pay me one billion dollars.
> Of course, I get to choose whether I offer (a) or (b)."
>
> The man was wary. Did he need to sign over his soul?
> No, said the devil, all the man need do is accept the offer.
>
> The man pondered. If he was offered (b) it was unlikely
> that he would ever be able to buy the wish, but what was
> the harm in having the opportunity available?
>
> "I accept," said the man at last. "Do I get (a) or (b)?"
>
> The devil paused. "I choose (b)."
>
> The man was disappointed but not surprised. That was
> that, he thought. But the offer gnawed at him. Imagine
> what he could do with his wish! Many years passed, and
> the man began to accumulate money. To get the money he
> sometimes did bad things, and dimly he realized that this
> must be what the devil had in mind. Eventually he had his
> billion dollars, and the devil appeared again.
>
> "Here is a billion dollars," said the man, handing over a
> valise containing the money. "Grant me my wish!"
>
> The devil took possession of the valise. Then he said,
> "Oh, did I say (b) before? I'm so sorry. I meant (a). It is
> my great pleasure to give you one billion dollars."
>
> And the devil handed back to the man the same valise
> that the man had just handed to him.

So our "truth oracle" reveals itself as a
computational manifestation of the devil's bargain from this story.
In hindsight, it makes sense that the program that Curry-Howard-corresponds to the law of excluded middle has to behave like this.
It needs to actually return a concrete value, since it's a computer program,
but it also can't ever give the wrong answer, since that would give a value of the empty type and break the type system.
So instead it hides a time machine inside the value,
and rigs it to activate, as a fail-safe mechanism, whenever code tries to use it to do something that would disastrously break the programming language.
