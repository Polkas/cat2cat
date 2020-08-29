# aggregated data sets

# d.f/d.t
#######################################
### Aggregat


#' transforming table of mappings to list with keys
#' @description transforming table of mappings to list with keys where first column is assumed to be an old encoding.
#' @param x data.frame with 2 columns
#' @return list with 2 fields `to_old` `to_new`
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

  x <- x[(x[, 1] != "") &
    (!is.na(x[, 1])) & (x[, 2] != "") & (!is.na(x[, 2])), ]

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

#' applying frequencies toobject returned by get_mappings
#' @description applying frequencies to object returned by get_mappings
#' @param to_x list object returned by get_mappings
#' @param freqs vector object returned by get_freqs
#' @return list
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
#'     occup$multipier[occup$year == "2008"]
#'   )
#' )
#' head(data.frame(I(mappings$to_old), I(mapp_p)))
#' mapp_p <- cat_apply_freq(
#'   mappings$to_new,
#'   get_freqs(
#'     occup$code[occup$year == "2010"],
#'     occup$multipier[occup$year == "2010"]
#'   )
#' )
#' head(data.frame(I(mappings$to_new), I(mapp_p)))
#' @export
cat_apply_freq <- function(to_x, freqs) {
  stopifnot(ncol(freqs) == 2)
  res <- lapply(
    to_x,
    function(x) {
      alls <- freqs[, 2][match(x, freqs[, 1])]
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

#' getting frequencies for a vector with an optional multipier argument
#' @description getting frequencies for a vector with an optional multipier argument
#' @param x vector
#' @param multipier vector how many times to repeat certain value
#' @return data.frame with two columns `input` `Freq`
#' @examples
#' data(occup)
#'
#' get_freqs(occup$code[occup$year == "2008"])
#' get_freqs(occup$code[occup$year == "2010"])
#'
#' get_freqs(occup$code[occup$year == "2008"], occup$multipier[occup$year == "2008"])
#' get_freqs(occup$code[occup$year == "2010"], occup$multipier[occup$year == "2010"])
#' @export

get_freqs <- function(x, multipier = NULL) {
  input <- if (!is.null(multipier)) {
    rep(x, times = multipier)
  } else {
    x
  }
  res <- as.data.frame(table(input), stringsAsFactors = F)
  res
}

#' Automatic transforming of a categorical variable according to a new encoding
#' @description Automatic transforming of a categorical variable according to a new encoding
#' @param data list with 4, 5 or 6 named fileds `old` `new` `cat_var` `time_var` and optional `multipier_var`,`freq_df`
#' @param mappings list with 2 named fileds `trans` `direction`
#' @param ml list with 3 named fields `method` `features` `args``
#' @details
#' data args
#' \itemize{
#'  \item{"old"}{ data.frame}
#'  \item{"new"} { data.frame}
#'  \item{"cat_var"}{ name of the caterogical variable}
#'  \item{"time_var"}{ name of the time varaiable}
#'  \item{"multipier_var"}{ name of the multipier variable - number of replication needed to reproduce the population}
#'  \item{"freqs_df"}{ data.frame with 2 columns where first one is category name and second counts which will be used to assess the probabilities.}
#' }
#' mappings args
#' \itemize{
#'  \item{"trans"}{ data.frame with 2 columns - transition table}
#'  \item{"direction"}{ direction - "backward or "forward"}
#' }
#' ml args
#' \itemize{
#'  \item{"method"}{ character "knn"}
#'  \item{"features"}{ vector of features names}
#'  \item{"args"}{ k number of k nearest n}
#' }
#' @return list of 2 data.frames
#' @importFrom progress progress_bar
#' @importFrom caret knn3
#' @importFrom Hmisc impute
#' @examples
#' data(occup)
#' data(trans)
#'
#' occup_old <- occup[occup$year == 2008, ]
#' occup_new <- occup[occup$year == 2010, ]
#'
#' cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "forward")
#' )
#'
#' cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "forward"),
#'   ml = list(
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#'   )
#' )
#' @export

cat2cat <-
  function(data = list(
             old = NULL,
             new = NULL,
             cat_var = NULL,
             time_var = NULL,
             multipier_var = NULL,
             freqs_df = NULL
           ),
           mappings = list(trans = NULL, direction = NULL),
           ml = list(
             method = NULL,
             features = NULL,
             args = NULL
           )) {
    stopifnot(
      is.list(data) &&
        length(data) %in% c(4, 5, 6) &&
        inherits(data$old, "data.frame") &&
        inherits(data$new, "data.frame") &&
        all(c("old", "new", "cat_var", "time_var") %in% names(data)) &&
        all(c(data$cat_var, data$time_var) %in% colnames(data$old)) &&
        all(c(data$cat_var, data$time_var) %in% colnames(data$new))
    )

    stopifnot(
      is.null(data$multipier_var) ||
        (
          data$multipier_var %in% colnames(data$new) &&
            data$multipier_var %in% colnames(data$old)
        )
    )

    stopifnot(is.null(data$freqs_df) || (ncol(data$freqs_df) == 2))

    d_old <- length(unique(data$old[[data$time_var]]))
    d_new <- length(unique(data$new[[data$time_var]]))

    stopifnot((d_old == 1) && (d_new == 1))

    stopifnot(
      is.list(mappings) &&
        length(mappings) == 2 &&
        all(c("trans", "direction") %in% names(mappings)) &&
        mappings$direction %in% c("forward", "backward") &&
        all(vapply(mappings, Negate(is.null), logical(1))) &&
        ncol(mappings$trans) == 2
    )

    mapps <- get_mappings(mappings$trans)

    if (mappings$direction == "forward") {
      cat_base_year <- data$old
      cat_final_year <- data$new
      mapp <- mapps$to_old
      res_ord <- c(1, 2)
    } else if (mappings$direction == "backward") {
      cat_base_year <- data$new
      cat_final_year <- data$old
      mapp <- mapps$to_new
      res_ord <- c(2, 1)
    }

    cats_base <- cat_base_year[[data$cat_var]]
    cats_final <- cat_final_year[[data$cat_var]]

    multi_base <- if (!is.null(data$multipier_var)) {
      cat_base_year[[data$multipier_var]]
    } else {
      NULL
    }

    stopifnot(is.null(data$freq_df) || all(data$freqs_df[, 1] %in% cats_base))

    fre <- if (!is.null(data$freqs_df)) {
      data$freqs_df
    } else {
      get_freqs(cats_base, multi_base)
    }

    freqs_2 <- cat_apply_freq(mapp, fre)

    g_vec <- mapp[cats_final]
    rep_vec <- lengths(g_vec)
    wei_vec <- unlist(freqs_2[cats_final])
    cat_final_year$index_c2c <- 1:nrow(cat_final_year)
    cat_final_rep <-
      cat_final_year[rep(1:nrow(cat_final_year), times = rep_vec), ]
    cat_final_rep$g_new_c2c <- unlist(g_vec)
    cat_final_rep$wei_freq_c2c <- wei_vec
    cat_final_rep$rep_c2c <- rep(rep_vec, times = rep_vec)
    cat_final_rep$wei_naive_c2c <- 1 / cat_final_rep$rep_c2c

    cat_base_year$index_c2c <- 1:nrow(cat_base_year)
    cat_base_year$g_new_c2c <- cat_base_year[[data$cat_var]]
    cat_base_year$wei_freq_c2c <- 1
    cat_base_year$rep_c2c <- 1
    cat_base_year$wei_naive_c2c <- 1

    if (sum(vapply(ml, Negate(is.null), logical(1))) >= 2) {
      stopifnot(all(c("method", "features") %in% names(ml)))
      stopifnot(all(ml$features %in% colnames(cat_final_rep)))
      stopifnot(ml$method %in% c("knn"))

      uu <- unique(cat_final_rep[[data$cat_var]])

      features <- ml$features

      cat_base_year_g <- split(cat_base_year, cat_base_year[[data$cat_var]])

      cat_final_rep_cat_c2c <- split(cat_final_rep, cat_final_rep[[data$cat_var]])

      pb <- progress_bar$new(total = length(uu))

      for (i in unique(uu)) {
        pb$tick()

        tryCatch(
          {
            base <- cat_final_rep_cat_c2c[[i]]

            dis <- do.call(rbind, cat_base_year_g[unique(base$g_new_c2c)])

            if (ml$method == "knn") {
              if (length(unique(base$g_new_c2c)) > 1 &&
                length(unique(dis$code)) >= 1 &&
                nrow(base) > 0 &&
                any(base$g_new_c2c %in% names(cat_base_year_g))) {
                kkk <- caret::knn3(
                  x = dis[, features],
                  y = factor(dis$code),
                  k = min(ml$args$k, nrow(dis))
                )

                pp <- as.data.frame(predict(kkk, unique(base[, features]), type = "prob"))

                ll <- setdiff(unique(base$g_new_c2c), colnames(pp))

                if (ncol(pp) == 1 &&
                  length(unique(dis$code)) == 1) {
                  colnames(pp) <- unique(dis$code)
                }

                if (length(ll)) {
                  pp[ll] <- 0
                }

                pp <- pp[, unique(base$g_new_c2c)]

                cat_final_rep_cat_c2c[[i]]$wei_ml_c2c <- Hmisc::impute(as.vector(t(pp)), 0)
              } else {
                cat_final_rep_cat_c2c[[i]]$wei_ml_c2c <- 1 / cat_final_rep_cat_c2c[[i]]$rep_c2c
              }
            }
          },
          error = function(e) {
            cat_final_rep_cat_c2c[[i]]$wei_ml_c2c <- 1 / cat_final_rep_cat_c2c[[i]]$rep_c2c
          }
        )
      }

      pb$terminate()
      cat_final_rep <- do.call(rbind, cat_final_rep_cat_c2c)
      cat_final_rep <- cat_final_rep[order(cat_final_rep$id), ]
      cat_base_year$wei_ml_c2c <- 1
    }

    res <- list(cat_base_year, cat_final_rep)[res_ord]
    names(res) <- c("old", "new")
    res
  }

#' A set of prune methods which will be usefull after transition process
#'
#' @description user could specify one from four methods to prune replications
#'
#' @param df data.frame
#' @param index character default wei_freq_c2c
#' @param column character default index_c2c
#' @param method character one of four available methods: default "nonzero", "highest", "highest1", "morethan"
#' @param percent integer from 0 to 99
#' @return data.frame
#' @details
#' method - specify method to reduce number of replications
#' \itemize{
#'  \item{"nonzero"}{ remove nonzero probabilities}
#'  \item{"highest"} { leave only highest probabilities for each subject- accepting ties}
#'  \item{"highest1"} { leave only highest probabilities for each subject- not accepting ties so always one is returned}
#'  \item{"morethan"}{ leave rows where a probability is hgher than value specify by percent argument }
#' }
#' @examples
#' data(occup)
#' data(trans)
#'
#' occup_old <- occup[occup$year == 2008, ]
#' occup_new <- occup[occup$year == 2010, ]
#'
#'
#' occup_2 <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "backward"),
#'   ml = list(
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#'   )
#' )
#'
#' prune_cat2cat(occup_2$old, method = "nonzero")
#' prune_cat2cat(occup_2$old, method = "highest")
#' prune_cat2cat(occup_2$old, method = "highest1")
#' prune_cat2cat(occup_2$old, method = "morethan", percent = 90)
#'
#' prune_cat2cat(occup_2$old, column = "wei_ml_c2c", method = "nonzero")
#' @export
prune_cat2cat <- function(df, index = "index_c2c", column = "wei_freq_c2c", method = "nonzero", percent = 50) {
  stopifnot(inherits(df, "data.frame") &&
    all(c(index, column) %in% colnames(df)) &&
    method %in% c("nonzero", "highest", "highest1", "morethan") &&
    length(percent) == 1 &&
    percent >= 0 &&
    percent < 100)

  df <- df[order(df[[column]]), ]

  switch(method,
    nonzero = df[df[[column]] > 0, ],
    highest1 = df[unlist(tapply(df[[column]], df[[index]], function(x) seq_along(x) == which.max(x))), ],
    highest = df[unlist(tapply(df[[column]], df[[index]], function(x) x == max(x))), ],
    morethan = df[df[[column]] > percent / 100, ]
  )
}

#' a funtion to make a combination of weights from different methods
#'
#' @description adding additional column which is a mix of weights columns
#'
#' @param df data.frame
#' @param cols character vector default c("wei_freq_c2c", "wei_naive_c2c", "wei_ml_c2c")
#' @param weis numeric vector c(1/3,1/3,1/3)
#' @return data.frame with an additional column
#' @export
cross_cat2cat <- function(df, cols = c("wei_freq_c2c", "wei_naive_c2c", "wei_ml_c2c"), weis = c(1 / 3, 1 / 3, 1 / 3)) {
  stopifnot(inherits(df, "data.frame"))

  df[["wei_cross_c2c"]] <- as.vector(as.matrix(df[, cols]) %*% weis)

  df
}
