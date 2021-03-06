% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat2cat.R
\name{cat_apply_freq}
\alias{cat_apply_freq}
\title{Applying frequencies to object returned by get_mappings}
\usage{
cat_apply_freq(to_x, freqs)
}
\arguments{
\item{to_x}{list object returned by get_mappings}

\item{freqs}{vector object returned by get_freqs}
}
\value{
list
}
\description{
applying frequencies to object returned by get_mappings
}
\examples{
data(trans)
data(occup)

mappings <- get_mappings(trans)

mappings$to_old[1:4]
mappings$to_new[1:4]

mapp_p <- cat_apply_freq(
  mappings$to_old,
  get_freqs(
    occup$code[occup$year == "2008"],
    occup$multiplier[occup$year == "2008"]
  )
)
head(data.frame(I(mappings$to_old), I(mapp_p)))
mapp_p <- cat_apply_freq(
  mappings$to_new,
  get_freqs(
    occup$code[occup$year == "2010"],
    occup$multiplier[occup$year == "2010"]
  )
)
head(data.frame(I(mappings$to_new), I(mapp_p)))
}
