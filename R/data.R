#' Occupational dataset
#'
#' @details occup dataset is an example of unbalance panel dataset.
#' This is a simulated data although there are applied a real world
#' characteristics from national statistical office survey.
#' The original survey is anonymous and take place every two years.
#' It is presenting a characteristics from randomly selected company and then
#' using k step procedure employees are chosen.
#'
#' @usage occup
#'
#' @format A data frame with around 70000 observations and 12 variables.
#' \describe{
#' \item{id}{	integer	id}
#' \item{age}{ numeric age of a subject}
#' \item{sex}{ numeric sex of a subject}
#' \item{edu}{
#'  integer edu level of education of a subject where lower means
#'  higher - 1 for at least master degree
#' }
#' \item{exp}{ numeric exp number of experience years for a subject}
#' \item{district}{ integer district}
#' \item{parttime}{
#'   numeric contract type regards time where 1 mean full-time
#'   (work a whole week)
#' }
#' \item{salary}{ numeric salary per year}
#' \item{code}{ character code - occupational code}
#' \item{multiplier}{
#'   numeric multiplier for the subject to reproduce a population - how many
#'   of such subjects in population
#' }
#' \item{year}{integer year}
#' \item{code4}{ character code - occupational code - first 4 digits}
#' }
#' @details occupational dataset
#'
"occup"

#' Occupational dataset - small one
#'
#' @details occup dataset is an example of unbalance panel dataset.
#' This is a simulated data although there are applied a real world
#' characteristics from national statistical office survey.
#' The original survey is anonymous and take place every two years.
#' It is presenting a characteristics from randomly selected company and
#' then using k step procedure employees are chosen.
#'
#' @usage occup_small
#'
#' @format A data frame with around 8000 observations and 12 variables.
#' \describe{
#' \item{id}{	integer	id}
#' \item{age}{ numeric age of a subject}
#' \item{sex}{ numeric sex of a subject}
#' \item{edu}{
#'   integer edu level of education of a subject where lower means
#'   higher - 1 for at least master degree
#' }
#' \item{exp}{ numeric exp number of experience years for a subject}
#' \item{district}{ integer district}
#' \item{parttime}{
#'   numeric contract type regards time where 1 mean full-time
#'   (work a whole week)
#' }
#' \item{salary}{ numeric salary per year}
#' \item{code}{ character code - occupational code}
#' \item{multiplier}{
#'   numeric multiplier for the subject to reproduce a population -
#'   how many of such subjects in population
#' }
#' \item{year}{integer year}
#' \item{code4}{ character code - occupational code - first 4 digits}
#' }
#' @details occupational dataset
#' @examples
#' set.seed(1234)
#' data("occup", package = "cat2cat")
#' occup_small <- occup[sort(sample(nrow(occup), 8000)), ]
"occup_small"

#' trans dataset containing mappings (transitions) between
#' old (2008) and new (2010) occupational codes.
#' This table could be used to map encodings in both directions.
#'
#' @usage trans
#'
#' @format A data frame with 2693 observations and 2 variables.
#' \describe{
#' \item{old}{	character an old encoding of a certain occupation}
#' \item{new}{	character a new encoding of a certain occupation}
#' }
#' @details mapping (transition) table for occupations where first column
#' contains old encodings and second one a new encoding
#'
"trans"

#' verticals dataset
#'
#' @usage verticals
#'
#' @format A data frame with 21 observations and 4 variables.
#' \describe{
#' \item{vertical}{	character an certain sales vertical}
#' \item{sales}{	numeric a size of sale}
#' \item{counts}{	integer counts size}
#' \item{v_date}{	character Date}
#' }
#' @details random data - aggregate sales across e-commerce verticals
#' @examples
#' set.seed(1234)
#' agg_old <- data.frame(
#'   vertical = c(
#'     "Electronics", "Kids1", "Kids2", "Automotive", "Books",
#'     "Clothes", "Home", "Fashion", "Health", "Sport"
#'   ),
#'   sales = rnorm(10, 100, 10),
#'   counts = rgeom(10, 0.0001),
#'   v_date = rep("2020-04-01", 10), stringsAsFactors = FALSE
#' )
#'
#' agg_new <- data.frame(
#'   vertical = c(
#'     "Electronics", "Supermarket", "Kids", "Automotive1",
#'     "Automotive2", "Books", "Clothes", "Home", "Fashion", "Health", "Sport"
#'   ),
#'   sales = rnorm(11, 100, 10),
#'   counts = rgeom(11, 0.0001),
#'   v_date = rep("2020-05-01", 11), stringsAsFactors = FALSE
#' )
#' verticals <- rbind(agg_old, agg_new)
"verticals"

#' verticals2 dataset
#'
#' @usage verticals2
#'
#' @format A data frame with 202 observations and 4 variables.
#' \describe{
#' \item{ean}{ product ean}
#' \item{vertical}{	character an certain sales vertical}
#' \item{sales}{	numeric a size of sale}
#' \item{v_date}{	character Date}
#' }
#' @details random data - single products sales across e-commerce verticals
#' @examples
#' set.seed(1234)
#' vert_old <- data.frame(
#'   ean = 90000001:90000020,
#'   vertical = sample(c(
#'     "Electronics", "Kids1", "Kids2", "Automotive", "Books",
#'     "Clothes", "Home", "Fashion", "Health", "Sport"
#'   ), 20, replace = TRUE),
#'   sales = rnorm(20, 100, 10),
#'   v_date = rep("2020-04-01", 20), stringsAsFactors = FALSE
#' )
#'
#' vert_old2 <- data.frame(
#'   ean = 90000021:90000100,
#'   vertical = sample(c(
#'     "Electronics", "Kids1", "Kids2", "Automotive", "Books",
#'     "Clothes", "Home", "Fashion", "Health", "Sport"
#'   ), 80, replace = TRUE),
#'   sales = rnorm(80, 100, 10),
#'   v_date = rep("2020-04-01", 80), stringsAsFactors = FALSE
#' )
#'
#' vert_new <- vert_old2
#' vert_new$sales <- rnorm(nrow(vert_new), 80, 10)
#' vert_new$v_date <- "2020-05-01"
#' vert_new$vertical[vert_new$vertical %in% c("Kids1", "Kids2")] <- "Kids"
#' vert_new$vertical[vert_new$vertical %in% c("Automotive")] <-
#'   sample(
#'     c("Automotive1", "Automotive2"),
#'     sum(vert_new$vertical %in% c("Automotive")),
#'     replace = TRUE
#'   )
#' vert_new$vertical[vert_new$vertical %in% c("Home")] <-
#'   sample(
#'     c("Home", "Supermarket"),
#'     sum(vert_new$vertical %in% c("Home")),
#'     replace = TRUE
#'   )
#'
#' vert_new2 <- data.frame(
#'   ean = 90000101:90000120,
#'   vertical = sample(
#'     c(
#'       "Electronics", "Supermarket", "Kids", "Automotive1",
#'       "Automotive2", "Books", "Clothes", "Home",
#'       "Fashion", "Health", "Sport"
#'     ), 20,
#'     replace = TRUE
#'   ),
#'   sales = rnorm(20, 100, 10),
#'   v_date = rep("2020-05-01", 20), stringsAsFactors = FALSE
#' )
#'
#' verticals2 <- rbind(
#'   rbind(vert_old, vert_old2),
#'   rbind(vert_new, vert_new2)
#' )
#' verticals2$vertical <- as.character(verticals2$vertical)
"verticals2"
