#' Transforming a transition table with mappings to two associative lists
#' @description to rearrange the one classification encoding into another, an associative list that maps keys to values is used.
#' More precisely, an association list is used which is a linked list in which each list element consists of a key and value or values.
#' An association list where unique categories codes are keys and matching categories from next or previous time point are values.
#' A transition table is used to build such associative lists.
#' @param x data.frame or matrix - transition table with 2 columns where first column is assumed to be the older encoding.
#' @details the named list will be a more efficient solution than hash map as we are not expecting more than a few thousand keys.
#' @return a list with 2 named lists `to_old` and `to_new`.
#' @examples
#' data(trans)
#'
#' mappings <- get_mappings(trans)
#' mappings$to_old[1:4]
#' mappings$to_new[1:4]
#' @export
get_mappings <- function(x = data.frame()) {
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

#' Applying frequencies to the object returned by get_mappings function
#' @description applying frequencies to the object returned by get_mappings.
#' We will get a symmetric object to returned one by get_mappings function, nevertheless categories are replaced with frequencies.
#' Frequencies for each category/key are sum to 1, so could be interpreted as probabilities.
#' @param to_x list object returned by get_mappings.
#' @param freqs vector object returned by get_freqs.
#' @return a list with 2 named lists `to_old` and `to_new`.
#' @examples
#' data(trans)
#' data(occup)
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
  stopifnot(ncol(freqs) == 2)
  res <- lapply(
    to_x,
    function(x) {
      alls <- freqs[, 2, drop = TRUE][match(x, freqs[, 1, drop = TRUE])]
      ff <- alls / sum(alls, na.rm = T)
      # NA to 0
      ifelse(is.na(ff) | is.nan(ff), 0, ff)
    }
  )
  # all equal to zero so proportional probability
  res_out <- lapply(
    res,
    function(x) {
      ifelse(rep(all(x == 0), length(x)), 1 / length(x), x)
    }
  )
  names(res_out) <- names(to_x)
  res_out
}

#' Getting frequencies from a `character` vector with an optional multiplier argument
#' @description getting frequencies for a vector with an optional multiplier argument
#' @param x character vector categorical variable to summarize.
#' @param multiplier numeric vector how many times to repeat certain value, additional weights.
#' @return data.frame with two columns `input` `Freq`
#' @note without multiplier variable it is a basic `table` function wrapped with the `as.data.frame` function.
#' The `table` function is used with the `useNA = "ifany"` argument.
#' @export
#' @examples
#' data(occup)
#'
#' head(get_freqs(occup$code[occup$year == "2008"]))
#' head(get_freqs(occup$code[occup$year == "2010"]))
#'
#' head(get_freqs(occup$code[occup$year == "2008"], occup$multiplier[occup$year == "2008"]))
#' head(get_freqs(occup$code[occup$year == "2010"], occup$multiplier[occup$year == "2010"]))
get_freqs <- function(x, multiplier = NULL) {
  stopifnot(is.null(multiplier) || length(x) == length(multiplier))

  input <- if (!is.null(multiplier)) {
    rep(x, times = as.numeric(multiplier))
  } else {
    x
  }
  res <- as.data.frame(table(input, useNA = "ifany"), stringsAsFactors = F)
  res
}
