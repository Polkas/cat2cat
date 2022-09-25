#' Transforming a mapping (transition) table to two associative lists
#' @description to rearrange the one classification encoding into another,
#'  an associative list that maps keys to values is used.
#' More precisely, an association list is used which is a linked list in which
#' each list element consists of a key and value or values.
#' An association list where unique categories codes are keys and matching
#' categories from next or previous time point are values.
#' A mapping (transition) table is used to build such associative lists.
#' @param x `data.frame` or `matrix` - mapping (transition) table with 2 columns
#'  where first column is assumed to be the older encoding.
#' @return a list with 2 named lists `to_old` and `to_new`.
#' @examples
#' data("trans", package = "cat2cat")
#'
#' mappings <- get_mappings(trans)
#' mappings$to_old[1:4]
#' mappings$to_new[1:4]
#' @export
get_mappings <- function(x = data.frame()) {
  stopifnot(is.data.frame(x) || is.matrix(x))
  stopifnot(ncol(x) == 2)

  x <- as.matrix(x)

  ff <- x[, 1]
  ss <- x[, 2]

  from_old <- unique(ff)
  from_new <- unique(ss)

  to_old <- lapply(from_new, function(i) {
    tryCatch(
      unique(ff[ss %in% i]),
      error = function(e) {
        NA
      }
    )
  })
  names(to_old) <- from_new

  to_new <- lapply(from_old, function(i) {
    tryCatch(
      unique(ss[ff %in% i]),
      error = function(e) {
        NA
      }
    )
  })
  names(to_new) <- from_old

  list(to_old = to_old, to_new = to_new)
}

#' Getting frequencies from a vector with an optional multiplier
#' @description getting frequencies for a vector with an optional multiplier.
#' @param x `vector` categorical variable to summarize.
#' @param multiplier `numeric` vector how many times to repeat certain value,
#' additional weights.
#' @return `data.frame` with two columns `input` `Freq`
#' @note without multiplier variable it is a basic `table` function wrapped with
#'  the `as.data.frame` function.
#' The `table` function is used with the `useNA = "ifany"` argument.
#' @export
#' @examples
#' data("occup", package = "cat2cat")
#'
#' head(get_freqs(occup$code[occup$year == "2008"]))
#' head(get_freqs(occup$code[occup$year == "2010"]))
#'
#' head(
#'   get_freqs(
#'     occup$code[occup$year == "2008"],
#'     occup$multiplier[occup$year == "2008"]
#'    )
#' )
#' head(
#'   get_freqs(
#'     occup$code[occup$year == "2010"],
#'     occup$multiplier[occup$year == "2010"]
#'    )
#' )
get_freqs <- function(x, multiplier = NULL) {
  stopifnot(is.vector(x))
  stopifnot(is.null(multiplier) || length(x) == length(multiplier))

  input <- if (!is.null(multiplier)) {
    rep(x, times = as.numeric(multiplier))
  } else {
    x
  }
  res <- as.data.frame(table(input, useNA = "ifany"), stringsAsFactors = FALSE)
  res
}


#' Applying frequencies to the object returned by the `get_mappings` function
#' @description applying frequencies to the object returned by
#' the `get_mappings` function.
#' We will get a symmetric object to the one returned by
#' the `get_mappings` function, nevertheless categories are replaced
#' with frequencies.
#' Frequencies for each category/key are sum to 1, so could be interpreted
#' as probabilities.
#' @param to_x `list` object returned by `get_mappings`.
#' @param freqs `data.frame` object like the one returned by
#' the `get_freqs` function.
#' @return a `list` with a structure like `to_x` object but with probabilities
#' for each category.
#' @note
#' `freqs` arg first column (keys) and the to_x arg values have to be of
#' the same type.
#' The uniform distribution (outcomes are equally likely) is assumed
#' for no match for all possible categories.
#' @examples
#' data("trans", package = "cat2cat")
#' data("occup", package = "cat2cat")
#'
#' mappings <- get_mappings(trans)
#'
#' mappings$to_old[1:4]
#' mappings$to_new[1:4]
#'
#' mapp_p <- cat_apply_freq(
#'   mappings$to_old,
#'   get_freqs(
#'     occup$code[occup$year == "2008"],
#'     occup$multiplier[occup$year == "2008"]
#'   )
#' )
#' head(data.frame(I(mappings$to_old), I(mapp_p)))
#' mapp_p <- cat_apply_freq(
#'   mappings$to_new,
#'   get_freqs(
#'     occup$code[occup$year == "2010"],
#'     occup$multiplier[occup$year == "2010"]
#'   )
#' )
#' head(data.frame(I(mappings$to_new), I(mapp_p)))
#' @export
cat_apply_freq <- function(to_x, freqs) {
  stopifnot(is.list(to_x))
  stopifnot(ncol(freqs) == 2)
  res <- lapply(
    to_x,
    function(x) {
      alls <- freqs[, 2, drop = TRUE][match(x, freqs[, 1, drop = TRUE])]
      ff <- alls / sum(alls, na.rm = TRUE)
      # NA to 0
      ff[is.na(ff) | is.nan(ff)] <- 0
      # all equal to zero so proportional probability
      if (all(ff == 0)) {
        ff_len <- length(ff)
        ff <- rep(1 / ff_len, ff_len)
      }
      ff
    }
  )
  names(res) <- names(to_x)
  res
}
