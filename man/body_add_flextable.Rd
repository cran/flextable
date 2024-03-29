% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/body_add_flextable.R
\name{body_add_flextable}
\alias{body_add_flextable}
\title{Add flextable into a Word document}
\usage{
body_add_flextable(
  x,
  value,
  align = NULL,
  pos = "after",
  split = NULL,
  topcaption = TRUE,
  keepnext = NULL
)
}
\arguments{
\item{x}{an rdocx object}

\item{value}{\code{flextable} object}

\item{align}{left, center (default) or right.
The \code{align} parameter is still supported for the time being, but
we recommend using \code{set_flextable_defaults(table_align = "center")} instead
that will set this default alignment for all flextables during
the R session, or to define alignement for each table with
\code{set_table_properties(align = "center")}.}

\item{pos}{where to add the flextable relative to the cursor,
one of "after", "before", "on" (end of line).}

\item{split}{set to TRUE if you want to activate Word
option 'Allow row to break across pages'.
This argument is still supported for the time being, but
we recommend using \code{set_flextable_defaults(split = TRUE)} instead
that will set this as default setting for all flextables during
the R session, or to define alignement for each table with
\code{\link[=set_table_properties]{set_table_properties()}} with argument \code{opts_word=list(split = TRUE)}
instead.}

\item{topcaption}{if TRUE caption is added before the table, if FALSE,
caption is added after the table.}

\item{keepnext}{Defunct in favor of \code{\link[=paginate]{paginate()}}. The default value
used for \code{keep_with_next} is set with
\code{set_flextable_defaults(keep_with_next = TRUE)}.}
}
\description{
Add a flextable into a Word document
created with 'officer'.
}
\details{
Use the \code{\link[=paginate]{paginate()}} function to define whether the table should
be displayed on one or more pages, and whether the header should be
displayed with the first lines of the table body on the same page.

Use the \code{\link[=set_caption]{set_caption()}} function to define formatted captions
(with \code{\link[=as_paragraph]{as_paragraph()}}) or simple captions (with a string).
\code{topcaption} can be used to insert the caption before the table
(default) or after the table (use \code{FALSE}).
}
\examples{
\dontshow{
init_flextable_defaults()
}
library(officer)

# define global settings
set_flextable_defaults(
  split = TRUE,
  table_align = "center",
  table.layout = "autofit"
)

# produce 3 flextable
ft_1 <- flextable(head(airquality, n = 20))
ft_1 <- color(ft_1, i = ~ Temp > 70, color = "red", j = "Temp")
ft_1 <- highlight(ft_1, i = ~ Wind < 8, color = "yellow", j = "Wind")
ft_1 <- set_caption(
  x = ft_1,
  autonum = run_autonum(seq_id = "tab"),
  caption = "Daily air quality measurements"
)
ft_1 <- paginate(ft_1, init = TRUE, hdr_ftr = TRUE)

ft_2 <- proc_freq(mtcars, "vs", "gear")
ft_2 <- set_caption(
  x = ft_2,
  autonum = run_autonum(seq_id = "tab", bkm = "mtcars"),
  caption = as_paragraph(
    as_b("mtcars"), " ",
    colorize("table", color = "orange")
  ),
  fp_p = fp_par(keep_with_next = TRUE)
)
ft_2 <- paginate(ft_2, init = TRUE, hdr_ftr = TRUE)

ft_3 <- summarizor(iris, by = "Species")
ft_3 <- as_flextable(ft_3, spread_first_col = TRUE)
ft_3 <- set_caption(
  x = ft_3,
  autonum = run_autonum(seq_id = "tab"),
  caption = "iris summary"
)
ft_3 <- paginate(ft_3, init = TRUE, hdr_ftr = TRUE)

# add the 3 flextable in a new Word document
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft_1)
doc <- body_add_par(doc, value = "")
doc <- body_add_flextable(doc, value = ft_2)
doc <- body_add_par(doc, value = "")
doc <- body_add_flextable(doc, value = ft_3)

fileout <- tempfile(fileext = ".docx")
print(doc, target = fileout)
\dontshow{
init_flextable_defaults()
}
}
\seealso{
\code{\link[=knit_print.flextable]{knit_print.flextable()}}, \code{\link[=save_as_docx]{save_as_docx()}}
}
