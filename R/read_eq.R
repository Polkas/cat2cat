
read_eq <- function(...) {
  xx <- as.list(substitute(list(...))[-1])
  ll <- length(xx)
  res <- vector("list", ll)
  for (i in seq_along(xx)) {
    children <- as.list(xx[[i]])
    for (c in seq_along(children)) {
      if (is.call(children[[c]])) {
        children[[c]] <- vapply(as.list(children[[c]])[-1], function(x) {
          deparse1(x)
        }, character(1))
      } else if (is.symbol(children[[c]])) {
        children[[c]] <- deparse1(children[[c]])
      }
    }
    res[[i]] <- children
  }
  res
}

format_trans <- function(x) {
  forw <- c("%>%", ">")
  back <- c("%<%", "<")

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
