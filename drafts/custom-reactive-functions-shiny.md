---
date: "2018-09-24"
keywords: ["shiny", "rlang"]
title: "Custom Reactive Functions in Shiny"
id: "urn:uuid:ed02a124-665b-46d6-89d0-642d48a58e0a"

abstract: |
  Most of the time, there's no need in Shiny applications to write custom "reactive functions"
  that accept one or more reactive expressions as arguments. Instead, you can use Shiny's own
  `reactive`, `observe`, `observeEvent`, etc. functions directly.
  
  However, if you identify in an application a pattern of usage involving reactive functions
  that could be deduplicated or abstracted, it becomes useful to be able to define a custom
  reactive function that can encapsulate that pattern.
  
  Traditionally (and in Shiny itself) this is done a particular way using shiny::exprToFunction
  or shiny::installExprFunction.
 
  It's also possible to use rlang, a more general tool for metaprogramming in R. In this post 
  I demonstrate both approaches and compare them.
---

[Shiny][shiny] is filled with many useful *reactive functions*, which I define
as functions that accept one or more *reactive expressions* as arguments.
Examples include `reactive`, `observer`, and `observeEvent`.

Most of time there's never a need in Shiny applications to write your own custom
reactive functions. Shiny's native vocabulary suffices.

Sometimes, though, it can be useful. For example, you might identify a pattern
of behavior in your application that is repeated in several places and could be
consolidated into a function.

I encountered such a pattern in an application of my own and I was able to
create my own reactive function using Shiny's native facilities for doing so. In
addition, for kicks, I did it with [rlang][rlang].

In this post, I will demonstrate and compare these two approaches.

## The `toggle` function

In my application, I needed to enable or disable one of my inputs (the
`textInput` with id `"text"`) depending on the value of a different input, the
`checkboxInput` with id `"enabled"`:

```{.r}
ui <- fluidPage(
  shinyjs::useShinyjs(),
  textInput("text", "Some text"),
  checkboxInput("enabled", "Enable text input", TRUE)
)
```

Here it is in action, in an `iframe`, running on [shinyapps.io][shinyapps.io]:

<iframe class="shinyapp" src="https://alandipert.shinyapps.io/toggler/"></iframe>

Shiny doesn't natively support enabling or disabling inputs, but the excellent
[shinyjs][shinyjs] package does.

Its `enable`, `disable`, and `toggleState` functions accept an id argument and
allow you to modify the "enabled-ness" of any Shiny input or input tag with the
specified id.

Using shinyjs, modifying the `text` input depending whether `enabled` is checked
is straightforward with `shiny::observeEvent`:

```{.r}
server <- function(input, output, session) {
  observeEvent(input$enabled, {
    if (input$enabled) {
      shinyjs::enable("text")
    } else {
      shinyjs::disable("text")
    }
  })
}
```

Great! But what if you have multiple pairs of inputs that need to toggle one
another? In that case, it might be beneficial for you to encapsulate this
behavior in your own custom reactive function. If you had such a function,
called `toggle`, you could write this instead:

```{.r}
server <- function(input, output, session) {
  toggle(input$enabled, "text")
}
```

At first blush, `toggle` seems like it should be easy to write:

```{.r}
toggle <- function(expr, id) {
  observeEvent(expr, {
    if (expr) {
      cat("enabling")
      shinyjs::enable(id)
    } else {
      cat("disabling")
      shinyjs::disable(id)
    }
  })
}
```

But if you try it, you'll find that you only ever see `enabling` printed twice
to the console, no matter how many times you check and uncheck the box.

The first `enabling` is printed because by default, `observeEvent` runs its
`handlerExpr` argument once as soon as it's called. This behavior is controlled by the
`ignoreInit` argument of `observeEvent`.

The reason `enabling` is printed once more, even when you uncheck the box, is
more subtle:

1. Shiny calls our `server` function, which 

`input$enabled` looks like it's simply accessing a field, but
really it's a function call, analagous to ```[[``(input, "enabled")`. 



## With native Shiny



[shiny]: https://shiny.rstudio.com/
[rlang]: https://github.com/r-lib/rlang
[shinyjs]: https://github.com/daattali/shinyjs
[shinyapps.io]: http://shinyapps.io
