+++
title = "Dependent Types Demystified"
date = "2020-01-15"
tags = ["types"]
+++

Static typing
=============

Certain languages are considered "statically-typed," which means the
compiler is aware of data-types at compile-time. Python and JavaScript
are **not** statically-typed but C, Java, Haskell, and Rust *are*
statically-typed.

Curry-Howard correspondence
===========================

There is a thing called the Curry-Howard correspondence. (Also called
the Curry-Howard isomorphism) It states that statically-typed computer
programs are isomorphic with mathematical logic. By "isomorphic," this
means that each program has an equivalent proof, and each proof has an
equivalent program. Another way of thinking about this is that each
program can be thought of as a proof, and each proof can also be thought
of as a program.

The more advanced a type system is, the more advanced logic it can
represent (propositional logic, first order logic, higher-order logic).
Some languages are designed for logic proofs, such as
[Coq](https://coq.inria.fr/), [Idris](https://www.idris-lang.org/), and
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php). These languages
have much more expressive type systems than commonly-used languages,
which makes them well-suited for proofs.

This blog post will focus on Coq ~~because that\'s the only one I know
well~~.

Propositions as types
=====================

In logic, propositions are statements that can be true or false, proven
or unproven. Examples of propositions are $1 = 0$, $2 + 2 = 4$, "the
sky is orange," and $\forall x \in \mathbb N, x >= 0$ (for all values
of $x$, where $x$ is a natural number, $x >= 0$). In the Curry-Howard
Isomorphism, propositions are types and proofs are values. The concept
of "a proposition having a proof" is "a type having a value." For
example, `true` is a proof of the proposition `bool`, and `10` is a
proof of the proposition `int`. This isn\'t very meaningful. Not all
programs are useful when thought of as proofs, and not all proofs are
useful when thought of as programs. The isomorphism is more useful for
fancier types/propositions.

True as unit type
=================

In logic, $\top$ is a proposition that is defined to be true. It has a
proof, by definition. In some programming languages, there\'s a type
(called the [unit type](https://en.wikipedia.org/wiki/Unit_type)) that
has only one value. In C, it would be an empty struct, since there\'s
only one possible value an empty struct can have, except C doesn\'t
allow empty structs 😦. In Haskell and Rust, the unit type is an empty
tuple `()`. Since a type having a value is isomorphic with a proposition
having a proof, the unit type is isomorphic with $\top$. The unit type
has one basic value, and $\top$ has one trivial proof.

Implication as function types
=============================

In logic, $A \to B$ means "$A$ implies $B$." If you have a proof of
$A \to B$ and a proof of $A$, you can combine them together to get a
proof of $B$. In functional programming languages like Haskell, `A -> B`
represents a function type that takes a parameter of type `A` and
returns a parameter of type `B`. If you have a value with type `A` and
have a function of type `A -> B`, you can combine them together to get a
value of type `B`. Notice a pattern 🤔? Logical implication is isomorphic
with function types.

Universal quantification as function types
==========================================

TODO
----
