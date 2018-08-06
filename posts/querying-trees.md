---
date: "2018-07-30"
keywords: ["query", "trees"]
title: "Querying Trees"
id: "urn:uuid:0e02f8ce-6018-45be-80c0-5cd643b6555a"
abstract: |
  Transforming trees into tables provides a way to use R like a path query
  language. The conversion requires walking the tree and storing it in memory as a
  table, and so may not be suitable for large trees. For small trees, however, the
  computation time is negligible and the programmer is able to write clear R code
  for querying the structure without explicitly walking the tree or admitting a
  separate query language.
---

R is *awesome* for working with tabular data. The language and its libraries
together offer comprehensive support for querying, visualizing, and modifying
data in the form of rows and columns.

Unfortunately, much data in the world is not tabular, and so not amenable to
direct manipulation in R. JSON, one of today's most popular interchange formats,
is hierarchical or tree-like.

The JSONPath query language has been developed to facilitate querying JSON
trees, and is accessible in R through the [rjsonpath][rjsonpath] library.

An alternative to using a purpose-built tree query language like JSONPath is to:

1. Parse the tree from JSON (such as with [jsonlite][jsonlite]) into a tree of R objects.
1. Convert the tree to a table.
1. Work with it from there, in R, without hand-writing tree-walking code or resorting to a separate query language.

## Transforming

A tree can be represented as a table of paths. For example, the following
structures have different affordances but are informationally equivalent:

~~~{.r}
# Tree
list(
  people = list(
    list(name = "Joe", age = 34),
    list(name = "Lance", age = 46),
    list(name = "Margaret")
  )
)

# Table
list(
  list("people", 0, "name", "Joe")
  list("people", 0, "age", 34)
  list("people", 1, "name", "Lance")
  list("people", 1, "age", 46)
  list("people", 2, "name", "Margaret")
)
~~~

The `tree_df` function below performs this transformation by walking the tree
and recursively accumulating a list of paths, and then converting the list of
paths to a dataframe with rows of uniform length:

~~~{.r}
# Returns a list of lists, each a path into the nested structure x. Each path
# ends with the value at the leaf.
paths <- function(parent, x) {
  paths_recurse <- function(items) {
    unlist(
      lapply(items, function(item) {
        paths(c(parent, item), x[[item]])
      }),
      recursive = FALSE
    )
  }
  if (typeof(x) == "list") {
    if (is.null(names(x))) {
      # Unnamed list aka JSON array
      paths_recurse(seq_along(x))
    } else if (length(x) > 0) {
      # Named list aka JSON object
      paths_recurse(names(x))
    }
  } else {
    # Leaf, return the path to the leaf and its value
    list(c(parent, x))
  }
}

# Returns a data frame where each row is a distinct path into a tree of lists,
# such as that produced by jsonlite::fromJSON. Useful for querying deeply-nested
# structures in a tabular fashion.
tree_df <- function(tree) {
  ps <- paths(list(), tree)

  # Ensure rows are all the same length, with NA for padding
  lengths <- vapply(ps, length, numeric(1))
  longest <- max(lengths)
  padded <- Map(ps, lengths, f = function(path, len) {
    append(path, rep(NA, longest - len))
  })

  as.data.frame(do.call(rbind, padded))
}
~~~

## Querying

With `tree_df` we can create a table from a tree:

~~~{.r}
tree <- list(
  people = list(
    list(name = "Joe", age = 34),
    list(name = "Lance", age = 46),
    list(name = "Margaret")
  )
)
 
df <- tree_df(tree)
~~~

Now, as a dataframe, the structure can be queried path-wise using `subset`.
However, because each column represents a depth, and the values at a particular
depth are not necessarily the same type, it's useful to have a helper function
to suppress the warnings `subset` produces about this:

~~~{.r}
tree_subset <- function(...) suppressWarnings(subset(...))
~~~

Finally, we can write code to tell us everyone's names:

~~~{.r}
> tree_subset(df, V3 == "name", V4)
        V4
1      Joe
3    Lance
5 Margaret
~~~

Or we could find all the ages under 40:

~~~{.r}
> tree_subset(df, V3 == "age" & V4 < 40, V4)
  V4
2 34
~~~

These results could be joined to answer questions like "What are the names of
the people under 40?"

## Summary

Transforming trees into tables provides a way to use R like a path query
language. The conversion requires walking the tree and storing it in memory as a
table, and so may not be suitable for large trees. For small trees, however, the
computation time is negligible and the programmer is able to write clear R code
for querying the structure without explicitly walking the tree or admitting a
separate query language.

[rjsonpath]: https://github.com/blmoore/rjsonpath
[jsonlite]: https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
