% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_content.R
\name{as_i}
\alias{as_i}
\title{italic chunk}
\usage{
as_i(x)
}
\arguments{
\item{x}{value, if a chunk, the chunk will be updated}
}
\description{
The function is producing a chunk with
italic font.
}
\note{
This is a sugar function that ease the composition of complex
labels made of different formattings. It should be used inside a
call to \code{\link{as_paragraph}}.
}
\examples{
ft <- flextable( head(iris), col_keys = c("dummy") )

ft <- compose(ft, j = "dummy",
    value = as_paragraph(
      as_i(Sepal.Length)
    ) )

autofit(ft)
}
\seealso{
Other chunk elements for paragraph: 
\code{\link{as_bracket}()},
\code{\link{as_b}()},
\code{\link{as_chunk}()},
\code{\link{as_image}()},
\code{\link{as_sub}()},
\code{\link{as_sup}()},
\code{\link{hyperlink_text}()},
\code{\link{linerange}()},
\code{\link{lollipop}()},
\code{\link{minibar}()}
}
\concept{chunk elements for paragraph}