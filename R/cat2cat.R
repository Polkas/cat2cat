# aggregated data sets

# d.f/d.t
#######################################
### Aggregat



#' @export

get_mappings <- function(x = data.frame()) {
  stopifnot(ncol(x) == 2)

  x <- as.matrix(x)

  x <- x[x[, 1] != "" &&
           !is.na(x[, 1]) && x[, 2] != "" && !is.na(x[, 2]), ]

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



#' @export
cat_apply_freq <- function(to_x, freqs) {
  stopifnot(ncol(freqs) == 2)
  res <- lapply(to_x,
                function(x) {
                  alls <- freqs[, 2][match(x, freqs[, 1])]
                  ff <- alls / sum(alls, na.rm = T)
                  # NA to 0
                  ifelse(is.na(ff) | is.nan(ff), 0, ff)
                })
  # all equal to zero so proportional probability
  res_out <- lapply(res,
                    function(x) {
                      ifelse(rep(all(x == 0), length(x)), 1 / length(x), x)
                    })
  names(res_out) <- names(to_x)
  res_out
}


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

#' @importFrom progress progress_bar
#' @importFrom caret knn3
#' @importFrom Hmisc impute
#' @importFrom hash hash
#' @export

cat2cat <-
  function(data = list(
    old = NULL,
    new = NULL,
    cat_var = NULL,
    time_var = NULL,
    multipier_var = NULL
  ),
  mappings = list(trans = NULL, direction = NULL),
  ml = list(method = NULL,
            features = NULL,
            args = NULL)) {
    stopifnot(
      is.list(data) &&
        length(data) %in% c(4, 5) &&
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

    d_to <- length(unique(data$old[[data$time_var]]))
    d_from <- length(unique(data$new[[data$time_var]]))

    stopifnot((d_to == 1) && (d_from == 1))

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
      res_ord  <- c(1, 2)
    } else if (mappings$direction == "backward") {
      cat_base_year <- data$new
      cat_final_year <- data$old
      mapp <- mapps$to_new
      res_ord  <- c(2, 1)
    }

    cats_base <- cat_base_year[[data$cat_var]]
    cats_final <- cat_final_year[[data$cat_var]]
    multi_base <-
      if (!is.null(data$multipier_var)) {
        cat_final_year[[data$multipier_var]]
      } else {
        data$multipier_var
      }
    freqs_2 <-
      cat_apply_freq(mapp, get_freqs(cats_base, multi_base))

    g_vec <- mapp[cats_final]
    rep_vec <- lengths(g_vec)
    wei_vec <- unlist(freqs_2[cats_final])
    cat_final_year$index_c2c <- 1:nrow(cat_final_year)
    cat_final_rep <-
      cat_final_year[rep(1:nrow(cat_final_year), times = rep_vec), ]
    cat_final_rep$g_new_c2c <- unlist(g_vec)
    cat_final_rep$wei_c2c <- wei_vec
    cat_final_rep$rep_c2c <- rep(rep_vec, times = rep_vec)
    cat_final_rep$wei_naive_c2c = 1 / cat_final_rep$rep_c2c

    cat_base_year$index_c2c <- 1:nrow(cat_base_year)
    cat_base_year$g_new_c2c <- cat_base_year[[data$cat_var]]
    cat_base_year$wei_c2c <- 1
    cat_base_year$rep_c2c <- 1

    if (sum(vapply(ml, Negate(is.null), logical(1))) >= 2) {
      stopifnot(all(c("method", "features") %in% names(ml)))
      stopifnot(all(ml$features %in% colnames(cat_final_rep)))
      stopifnot(ml$method %in% c("knn"))

      uu = unique(cat_final_rep[[data$cat_var]])

      features  = ml$features

      cat_base_year_g = split(cat_base_year, cat_base_year[[data$cat_var]])

      cat_final_rep_cat_c2c = split(cat_final_rep, cat_final_rep[[data$cat_var]])

      pb <- progress_bar$new(total = length(uu))

      for (i in unique(uu)) {
        pb$tick()

        tryCatch({
          base = cat_final_rep_cat_c2c[[i]]

          dis = do.call(rbind, cat_base_year_g[unique(base$g_new_c2c)])

          if (ml$method == "knn") {
            if (length(unique(base$g_new_c2c)) > 1 &&
                length(unique(dis$code)) >= 1 &&
                nrow(base) > 0 &&
                any(base$g_new_c2c %in% names(cat_base_year_g))) {

              kkk = caret::knn3(
                x = dis[, features],
                y = factor(dis$code),
                k = min(ml$args$k, nrow(dis))
              )

              pp  =  as.data.frame(predict(kkk, unique(base[, features]), type = "prob"))

              ll = setdiff(unique(base$g_new_c2c), colnames(pp))

              if (ncol(pp) == 1 &&
                  length(unique(dis$code)) == 1)
                colnames(pp) <- unique(dis$code)

              if (length(ll))
                pp[ll] <- 0

              pp = pp[, unique(base$g_new_c2c)]

              cat_final_rep_cat_c2c[[i]]$wei_ml_c2c = Hmisc::impute(as.vector(t(pp)), 0)

            } else {
              cat_final_rep_cat_c2c[[i]]$wei_ml_c2c <- NA
            }
          }

        }, error = function(e) {
          cat_final_rep_cat_c2c[[i]]$wei_ml_c2c <- NA
        })
      }

      pb$terminate()
      cat_final_rep = do.call(rbind, cat_final_rep_cat_c2c)
      cat_final_rep = cat_final_rep[order(cat_final_rep$id), ]

    }

    res <- list(cat_base_year, cat_final_rep)[res_ord]
    names(res) <- c("old", "new")
    res
  }
