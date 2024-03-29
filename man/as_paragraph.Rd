% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_content.R
\name{as_paragraph}
\alias{as_paragraph}
\title{Concatenate chunks in a flextable}
\usage{
as_paragraph(..., list_values = NULL)
}
\arguments{
\item{...}{chunk elements that are defining paragraph. If a character is used,
it is transformed to a chunk object with function \code{\link[=as_chunk]{as_chunk()}}.}

\item{list_values}{a list of chunk elements that are defining paragraph. If
specified argument \code{...} is unused.}
}
\description{
The function is concatenating text and images within paragraphs of
a flextable object, this function is to be used with functions such as \code{\link[=compose]{compose()}},
\code{\link[=add_header_lines]{add_header_lines()}}, \code{\link[=add_footer_lines]{add_footer_lines()}}.

This allows the concatenation of formatted pieces of text (chunks) that
represent the content of a paragraph.

The cells of a flextable contain each a single paragraph. This paragraph
is made of chunks that can be text, images or plots, equations and links.
}
\examples{
library(flextable)
ft <- flextable(airquality[sample.int(150, size = 10), ])
ft <- compose(ft,
  j = "Wind",
  value = as_paragraph(
    as_chunk(Wind, props = fp_text_default(color = "orange")),
    " ",
    minibar(value = Wind, max = max(airquality$Wind), barcol = "orange", bg = "black", height = .15)
  ),
  part = "body"
)
ft <- autofit(ft)
ft
}
\seealso{
\code{\link[=as_chunk]{as_chunk()}}, \code{\link[=minibar]{minibar()}},
\code{\link[=as_image]{as_image()}}, \code{\link[=hyperlink_text]{hyperlink_text()}}

Other functions for mixed content paragraphs: 
\code{\link{append_chunks}()},
\code{\link{compose}()},
\code{\link{prepend_chunks}()}
}
\concept{functions for mixed content paragraphs}
