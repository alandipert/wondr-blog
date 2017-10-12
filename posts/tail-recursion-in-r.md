---
date: "2017-10-15"
keywords: ["fp", "recursion"]
title: "Tail Recursion in R with Trampolines"
id: "urn:uuid:d3d294fc-7822-44b3-b2c9-77f1ed34bcbc"
abstract: |
  TODO
---

I recently enjoyed participating in a [discussion about recursion in R][1] on the
new [RStudio Community][2] site, and I thought to inaugurate my blog with a post
inspired by the discussion.

R supports recursive functions, but does not optimize _tail recursive_ functions
the way some other languages do. Fortunately, with a mechanism known as a
_trampoline_, the R programmer can implement something like the optimization
manually and with very little code.

To understand trampolines, one must first understand the mechanics of function
calls and recursion.

### The Call Stack

As in many other languages, functions in R may call themselves. For example,
here is a recursive function that decrements its argument until 0 is reached:

~~~{.r}
countdown <- function(n) if (n > 0) countdown(n-1) else "done"
~~~

This function has no problem with small values of `n`:

~~~
> countdown(10)
[1] "done"
> countdown(100)
[1] "done"
> countdown(1000)
[1] "done
~~~

Unfortunately, when `n` is big enough, an error is raised:

~~~
> countdown(10000)
Error: C stack usage  7969236 is too close to the limit
~~~

The problem here is that the top-most invocation of the `countdown` function, the
one we called with `countdown(10000)`, can't return until `countdown(9999)` returned,
which can't return until `countdown(9998)` returned, and so on.

R keeps track of all of these calls in a data structure called _the call stack_
or sometimes just _the stack_. The stack contains references to all outstanding
function calls, recursive or not.

Since the stack is stored in memory, and since computers only have so much
memory, the number of nested calls that can occur in a program is limited.

If we want to decrement a number 10000 or more times and print something when
we're done, we have to do it a different way. That way is to use a loop.

### Loops

Here's the `countdown` function using a loop instead of recursion:

~~~{.r}
countdown <- function(n) {
  while (n > 0) n <- n-1;
  "done"
}
~~~

It doesn't "overflow" the stack:

~~~
> countdown(10000)
[1] "done"
~~~

The new `countdown` contains the same essential pieces as the recursive version:
the `n > 0` test, decrementing `n`, and returning `"done"` at the end. The
pieces are just slightly differently arranged so that `countdown` doesn't need
to call itself.

If it does what we want, and looks only slightly different than the recursive
version... why did we care about recursion again?

Well, maybe we don't. The choice to use recursion is a stylistic one with
 arguable benefits.

Forgoing a debate of the merits of recursive style, let's assume we want it, and
that we need a technique that somehow lets us write recursive functions without
needing to worry about the stack limit.

### Trampoline

A trampoline is a function or set of functions that together give us the tools
we need to write code in a recursive style, in a way that doesn't overflow the
stack. Here's an awesome trampoline by [Jim Hester][3]:

~~~{.r}
trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (inherits(ret, "recursion")) {
      ret <- eval(as.call(c(f, unclass(ret))))
    }
    ret
  }
}

recur <- function(...) {
  structure(list(...), class = "recursion")
}
~~~

Using it, `countdown` now looks like this:

~~~{.r}
countdown <- trampoline(function(n) {
  if (n > 0) recur(n-1) else "done"
})
~~~

It's **very close** stylistically to the original recursive version, but has no stack issues:

~~~
> countdown(10000)
[1] "done"
~~~

The trampoline works because it's thin veneer over a regular loop. Compared to
our direct loop version of `countdown`, the _body_ of the trampoline's `while`
is parameterized by the `f` function instead of being hard-coded.

The only new requirement of this re-arrangement is that the _body_, or the `f`
function, return `recur` instead of calling itself if it wants to keep going.

In languages that perform this optimization automatically, applicable cases are
recognized automatically by the compiler and the recursive code is rewritten as
a loop. Compilers that do this are said to "support TCO" where TCO stands for
tail-call optimization.

### Tail call conversion

Trampolines only apply to [singly-recursive][recursion-types] functions that
call themselves in tail position, but many algorithms commonly expressed do not
meet these criteria. For example, here's a recursive `factorial` function in R
that can't immediately be trampolined:

~~~{.r}
factorial <- function(n) if (n == 0) 1 else n*factorial(n-1)
~~~

> Note: It probably wouldn't make sense to trampoline this function without
> other modifications first, because `n` is coerced to the `numeric` class if it
> wasn't already. For medium-sized `n`, `n` overflows to `Inf` before the stack
> overflows. I'll use `factorial` anyway because it's compact and the
> transformation is clear.

The "tail" of `factorial` is the expression `n*factorial(n-1)`, which places a
call to `factorial` on the stack before returning. This is exactly the operation
that eventually leads to stack overflow and that we need to eliminate.

The way forward is to introduce an _accumulator_, or a variable to store
intermediate state between calls. It's a step towards an explicit loop, but with
R's named and default argument support, can be done in a decidedly un-loopy way:

~~~{.r}
factorial <- function(n, prod = 1) {
  if (n == 0) 1 else factorial(n-1, n*prod)
}
~~~

Instead of relying on a recursive call for the number to multiply `n` by, we
store it explicitly in the `prod` argument and pass it along. In this way the
running product is maintained across invocations and the stack doesn't need to grow.

Of course, in R, the stack does grow, but now we've refactored the function
sufficiently enough to apply `trampoline`. Let's do that:

~~~{.r}
factorial <- trampoline(function(n, prod = 1) {
  if (n == 0) prod else recur(n-1, n*prod)
})
~~~

### Mutual recursion

Jim's trampoline is really efficient, but can't handle interdependent, _mutually
recursive_ functions. These are functions that call one another.

Arrangements like this don't come up much in my experience, and require a
different kind of trampoline, and so I generally prefer solutions like Jim's.
One type of program where mutual recursion seems to come up is in [parsers](https://en.wikipedia.org/wiki/Recursive_descent_parser).

But just for completeness, here's a `trampoline` function and two mutually
recursive functions from [SICP][sicp]:

~~~{.r}
trampoline <- function(f, ...) {
  function(...) {
    ret <- f(...)
    while (is.function(ret)) ret <- ret();
    ret
  }
}

even <- trampoline(function(n) {
  if (n == 0) TRUE else function() odd(n-1)
})

odd <- trampoline(function(n) {
  if (n == 0) FALSE else function() even(n-1)
})
~~~

[1]: https://community.rstudio.com/t/tidiest-way-to-do-recursion-safely-in-r/1408
[2]: https://community.rstudio.com/
[3]: http://www.jimhester.com/
[sicp]: https://mitpress.mit.edu/sicp/full-text/sicp/book/node82.html
[recursion-types]: https://en.wikipedia.org/wiki/Recursion_(computer_science)#single_recursion

