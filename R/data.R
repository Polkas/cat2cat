#' Occupational dataset
#'
#' @details occup dataset is an example of unbalance panel dataset.
#' This is a simulated data althought there are applied a real world characteristics from national statictics office survey.
#' The orginal survey is anonymous and take place every two years.
#' It is presenting a characteristics from randomly selected company and then using k step procedure employees are chosen.
#'
#' @usage occup
#'
#' @format A data frame with around 70000 observations and 12 variables.
#' \describe{
#' \item{id}{	integer	id}
#' \item{age}{ numeric age of a subject}
#' \item{sex}{ numeric sex of a subject}
#' \item{edu}{ integer edu level of education of a subject where lower means higher - 1 for at least master degree}
#' \item{exp}{ numeric exp number of experience years for a subject}
#' \item{district}{ integer district}
#' \item{sector}{ character sector}
#' \item{parttime}{ numeric contract type regards time where 1 mean full-time (work a whole week)}
#' \item{salary}{ numeric salary per year}
#' \item{code}{ character code - occupational code}
#' \item{code4}{ character code - occupational code - first 4 digits}
#' \item{multipier}{ numeric multipier for the subject to reproduce a population - how many of such subjects in population}
#' \item{year}{integer year}
#' }
#' @details occupational dataset
#'
"occup"

#' trans dataset containing transitions between old (2008) and new (2010) occupational codes.
#' this cold be used to translate encodings in both directions.
#'
#' @usage trans
#'
#' @format A data frame with 2693 observations and 2 variables.
#' \describe{
#' \item{old}{	character an old encoding of a certain occupation}
#' \item{new}{	character a new encoding of a certain occupation}
#' }
#' @details transition table for occupations where first column contains old encodings and second one a new encoding
#'
"trans"
