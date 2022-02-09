
#' @import rlang
read_eq <- function(...) {
  xx <- enexprs(...)
  ll <- length(xx)
  res <- vector("list", ll)
  for (i in seq_along(xx)) {
    children <- as.list(xx[[i]])
    for (c in seq_along(children)) {
      if (rlang::is_call(children[[c]])) {
        children[[c]] <- vapply(as.list(children[[c]])[-1], function(x) {
          expr_text(x)
        }, character(1))
      } else if (is.symbol(children[[c]])) {
        children[[c]] <- expr_text(children[[c]])
      }
    }
    res[[i]] <- children
  }
  res
}

format_trans <- function(x) {
  forw <- c("`%>%`", "`>`")
  back <- c("`%<%`", "`<`")

  direc_df <- data.frame(
    direction = c(forw, back),
    names = c(rep("forward", 2), rep("backward", 2)),
    stringsAsFactors = F
  )
  direct <- direc_df$names[match(x[[1]], direc_df$direction)]
  key_old <- x[[2]]
  key_new <- x[[3]]


  list(
    direction = direct,
    old = key_old,
    new = key_new
  )
}
