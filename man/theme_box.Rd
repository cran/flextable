% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes.R
\name{theme_box}
\alias{theme_box}
\title{Apply box theme}
\usage{
theme_box(x)
}
\arguments{
\item{x}{a flextable object}
}
\description{
Apply theme box to a flextable
}
\section{Illustrations}{


\if{html}{\figure{fig_theme_box_1.png}{options: width=60\%}}
}

\examples{
ft <- flextable(head(airquality))
ft <- theme_box(ft)
ft
}
\seealso{
Other functions related to themes: 
\code{\link{get_flextable_defaults}()},
\code{\link{set_flextable_defaults}()},
\code{\link{theme_alafoli}()},
\code{\link{theme_booktabs}()},
\code{\link{theme_tron_legacy}()},
\code{\link{theme_tron}()},
\code{\link{theme_vader}()},
\code{\link{theme_vanilla}()},
\code{\link{theme_zebra}()}
}
\concept{functions related to themes}
