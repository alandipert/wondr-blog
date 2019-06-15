---
date: "2019-06-15"
keywords: ["postconditions", "on.exit", "returnValue"]
title: "Assertions and Postconditions"
id: "urn:uuid:3c4a9f0a-8f8c-11e9-b4c2-e2eadd5ef15c"
abstract: |
  Postconditions are assertions about a function's return value defined
  within that function. R supports them through a combination of on.exit() and
  returnValue()
---

[Assertions][assertions] are a way to convey information about a program's
correct behavior to both humans and to the computer.
[Postconditions][postconditions] are assertions about a function's return value
defined within that function. Base R supports them through a combination of
`stopifnot()`, `on.exit()` and `returnValue()`.

> Note: `returnValue()` was introduced in R 3.2.0 but is marked experimental in
its documentation, so the approach outlined in this post might not always be
possible or desirable.

Let's start by looking at assertions, and go from there to an example motivating
postconditions.

## Assertions

Assertions are great and I use them all the time in most of the languages I use.
Unlike comments, they actually run, and so are less likely to become stale.
Unlike unit tests, they require no infrastructure and are exercised in the wild
in addition to during development.

In some languages, like C with gcc, they can even be [optionally disabled at
compile-time in order to improve performance][gcc-ndebug].

In R, assertions are most easily written using `base::stopifnot()`, but
there are [a variety of packages][assert-packages] to ease their writing also.

Here's a silly example, using `stopifnot()`:

~~~{.r}
make_odd <- function(n) {
  stopifnot(n %% 2 == 0)
  n + 1
}
~~~

Our `make_odd()` function here accepts a single argument, `n`, that should be
numeric and should be even. If `n` is already odd, an error is produced:

~~~
> make_odd(11)
 Error in make_odd(11) : n%%2 == 0 is not TRUE
~~~

But what if we wanted to make an assertion about the *return value* of
`make_odd()` instead of about the argument `n`? That would be a postcondition.

The simplest way that comes to mind is something like this:

~~~{.r}
make_odd <- function(n) {
  ret <- n + 1
  stopifnot(ret %% 2 == 1)
  ret
}
~~~

Unfortunately, this approach is impossible in a function that uses `return()`
liberally. It's doubly unfortunate because [complex][cyclomatic-complexity]
functions are exactly those that would benefit the most from statements about
correct behavior.

Here's an example of a complex function, [shouldIgnore][shouldIgnore], from
[shinyloadtest][shinyloadtest]:

~~~{.r}
shouldIgnore <- function(msg) {
  canIgnore <- c('^a\\["ACK.*$', '^\\["ACK.*$', '^h$')
  if (length(unlist(stringr::str_match_all(msg, canIgnore))) > 0) return(TRUE)
  parsed <- parseMessage(msg)
  if (length(intersect(names(parsed), c("busy", "progress", "recalculating"))) > 0) return(TRUE)
  if (identical(names(parsed), c("custom"))) {
    customKeys <- names(parsed[["custom"]])
    if (isTRUE(customKeys == "reactlog")) return(TRUE)
  }
  noop <- list(errors = list(), values = list(), inputMessages = list())
  if (identical(parsed, noop)) return(TRUE)
  return(FALSE)
}
~~~

You can see from its name, `shouldIgnore()`, and from the sprinkling of
`return(TRUE)` and `return(FALSE)` in its body, that this function is probably
supposed to return a logical vector of length 1. It's not immediately obvious
that it always does, which is why my instinct is to call this function complex.

If it doesn't, code higher up the [call stack][call-stack] might misbehave and
produce errors that aren't obviously a result of this particular function
returning the wrong kind of value.

How can we mitigate the higher-than-normal risk associated with changing this
function, such as by trying to simplify it?

## Unit Tests

One thing we could do is write a unit test for a representative set of values
for `msg`, the function's only argument. However, those unit tests will only run
during development. In the wild, a problematic value of `msg` might not produce
an obvious error.

## Postcondition

Writing unit tests is rarely a bad idea, but it's not necessarily the highest
leverage idea. If we could make a runnable statement about correct return value
within the function itself, we'd be able to produce a precise error during
development *and* in the wild. We'd also add value to any existing unit tests
that exercised `shouldIgnore()` at all, even those not written specifically to
test just it.

A postcondition is also superior to checking the return value of
`shouldIgnore()` wherever it's called from, because it might be called from
numerous and ever-changing places.

## Postcondition Implementation

`on.exit()`, `stopifnot()`, and `returnValue()` are the necessary working
pieces. They come together like this:

~~~{.r}
make_odd <- function(n) {
  on.exit(stopifnot(returnValue() %% 2 == 1))
  n + 1
}
~~~

1. `on.exit()` establishes an expression that should be run when the enclosing function, `make_odd()`, exits.
1. `stopifnot()` is used as the expression &mdash; and assertion &mdash; to run
1. `returnValue()` refers to the function's return value, or the result of `n + 1`

`shouldIgnore()` could be augmented using the same technique, to error if its return
value was ever not a logical vector of length 1:

~~~{.r}
shouldIgnore <- function(msg) {

  on.exit(stopifnot(is.logical(returnValue()) && length(returnValue()) == 1))

  canIgnore <- c('^a\\["ACK.*$', '^\\["ACK.*$', '^h$')
  if (length(unlist(stringr::str_match_all(msg, canIgnore))) > 0) return(TRUE)
  parsed <- parseMessage(msg)
  if (length(intersect(names(parsed), c("busy", "progress", "recalculating"))) > 0) return(TRUE)
  if (identical(names(parsed), c("custom"))) {
    customKeys <- names(parsed[["custom"]])
    if (isTRUE(customKeys == "reactlog")) return(TRUE)
  }
  noop <- list(errors = list(), values = list(), inputMessages = list())
  if (identical(parsed, noop)) return(TRUE)
  return(FALSE)
}
~~~

## Caveats

There are a couple things to be aware of if you use this approach.

First, `returnValue()` is marked experimental, and so the code where you use it
might break in the future when you upgrade R.

Second, by default, `on.exit()` overwrites existing expressions. This can be
changed by calling `on.exit()` with `add = TRUE`, but that's a change you'd have
to make to all `on.exit()` calls in your function subsequent to your
postcondition.

## Summary

1. *Assertions* are runnable statements about correct behavior
1. `stopifnot()` is the easiest way to write assertions in R
1. `on.exit()` and `returnValue()` can be used together with `stopifnot()` to write *postconditions*, or assertions about a function's return value.

Thanks for reading, I hope you enjoyed the post!

Thank you also to [Yihui Xie][yihui] for originally suggesting the approach to
postconditions that I elaborate upon.

[assertions]: https://en.wikipedia.org/wiki/Assertion_(software_development)
[postconditions]: https://en.wikipedia.org/wiki/Postcondition
[gcc-ndebug]: https://codeyarns.com/2015/03/16/how-to-disable-assert-in-gcc/
[assert-packages]: https://www.r-bloggers.com/the-state-of-assertions-in-r/
[cyclomatic-complexity]: https://en.wikipedia.org/wiki/Cyclomatic_complexity
[shouldIgnore]: https://github.com/rstudio/shinyloadtest/blob/7572a292e21d63a73dbf6de403e2537b72f65240/R/shiny-recorder.R#L165-L177
[shinyloadtest]: https://github.com/rstudio/shinyloadtest/
[call-stack]: https://en.wikipedia.org/wiki/Call_stack 
[yihui]: https://yihui.name/en/
