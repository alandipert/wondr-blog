---
author: "Alan Dipert"
date: "2017-09-30"
keywords: ["vectors"]
title: "The Indivisible"
id: "urn:uuid:d3d294fc-7822-44b3-b2c9-77f1ed34bcbc"
abstract: |
  TODO
---

When I cracked open [Advanced R's Data structures
chapter](http://adv-r.had.co.nz/Data-structures.html), I was surprised to learn
that R doesn't have "scalar" or "atomic" types. Things like numbers are instead
represented as vectors of numbers, 

In other languages I've used, like Lisp and Java, there's a distinction between
"atomic" and "compound" types. In Lisp, "atoms" are those types that are
"indivisible": they cannot contain other types. Numbers and symbols are atoms;
lists, which can contain numbers, symbols, or other lists (or even themselves!
But that's a whole other thing...) are not atoms.

Java defines a set of "primitive" types that, like Lisp atoms, can represent
only themselves, and have a machine representation that's opaque to the Java
programmer.

The concept of dividing types in programming languages into the categories of
atoms and compounds is so common that it's something I've taken as a given for
most of my career.

~~~{.javascript}
function sayHello(to) {
  alert("hello, " + to + "!");
}
~~~

But now I know: it's not unequivocally a good idea. At the very best, it's a
compromise. At the very worst, it's a philosophically bankrupt mass delusion on
the order of nu metal.

### The Compromise

In the case of Java, there are excellent practical reasons for primitives to
exist. Namely: computers aren't actually organized, internally, as a hierarchy
of objects with properties and methods.

A system of numerics based on the object-oriented way of programming that Java
otherwise encourages can't come close to the efficiency of working with the
numbers directly, at the machine level, as one would with assembly (or C, or
Fortran.)

And so, primitives in Java represent a technical compromise. We should mostly
program using objects and stuff, but when we're writing numerical code in loops,
we should use the primitive types. Doing so unlocks a flotilla of optimizations
that the JVM can perform on our behalf, bringing the performance of such code
within sight of C.

I'll take it.

### The Bankruptcy

There's an even deeper discrepancy between how computers work and how we tend to
think about programming, but it's a discrepancy so vast that,

### This is water.

Fans of David Foster Wallace's [Commencement Speech to Kenyon College class of
2005](https://www.youtube.com/watch?v=8CrOL-ydFMI) will be familiar with the
idea that if you swim in something long enough, what you're swimming in stops
existing for you.

The atom/compound bifurcation is like that. Part of the scenery, why are we
talking about this?

