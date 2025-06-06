% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_grobs.R
\name{gen_grob}
\alias{gen_grob}
\title{Convert a flextable to a grid grob object}
\usage{
gen_grob(
  x,
  ...,
  fit = c("auto", "width", "fixed"),
  scaling = c("min", "full", "fixed"),
  wrapping = TRUE,
  autowidths = TRUE,
  just = NULL
)
}
\arguments{
\item{x}{A flextable object}

\item{...}{Reserved for extra arguments}

\item{fit}{Determines the fitting/scaling of the grob on its parent viewport.
One of \code{auto}, \code{width}, \code{fixed}, \code{TRUE}, \code{FALSE}:
\itemize{
\item \code{auto} or \code{TRUE} (default): The grob is resized to fit in the parent viewport.
The table row heights and column widths are resized proportionally.
\item \code{width}: The grob is resized horizontally to fit the width
of the parent viewport. The column widths are resized proportionally.
The row heights are unaffected and the table height may be smaller or larger
than the height of the parent viewport.
\item \code{fixed} or \code{FALSE}: The grob will have fixed dimensions,
as determined by the column widths and the row heights.
}}

\item{scaling}{Determines the scaling of the table contents.
One of \code{min}, \code{full}, \code{fixed}, \code{TRUE}, \code{FALSE}:
\itemize{
\item \code{min} or \code{TRUE} (default):
When the parent viewport is smaller than the necessary,
the various content sizes (text font size, line width and image dimensions)
will decrease accordingly so that the content can still fit.
When the parent viewport is larger than the necessary,
the content sizes will remain the same, they will not increase.
\item \code{full}: Same as \code{min}, except that the content sizes are scaled fully,
they will increase or decrease, according to the size of the drawing surface.
\item \code{fixed} or \code{FALSE}: The content sizes will not be scaled.
}}

\item{wrapping}{Determines the soft wrapping (line breaking) method
for the table cell contents. One of \code{TRUE}, \code{FALSE}:
\itemize{
\item \code{TRUE}: Text content may wrap into separate lines at normal word break points
(such as a space or tab character between two words) or at newline characters
anywhere in the text content. If a word does not fit in the available cell width,
then the text content may wrap at any character.
Non-text content (such as images) is also wrapped into new lines,
according to the available cell width.
\item \code{FALSE}: Text content may wrap only with a newline character.
Non-text content is not wrapped.
}

Superscript and subscript chunks do not wrap.
Newline and tab characters are removed from these chunk types.}

\item{autowidths}{If \code{TRUE} (default) the column widths are adjusted
in order to fit the contents of the cells (taking into account the \code{wrapping} setting).}

\item{just}{Justification of viewport layout,
same as \code{just} argument in \code{\link[grid:grid.layout]{grid::grid.layout()}}.
When set to \code{NULL} (default), it is determined according to the \code{fit} argument.}
}
\value{
a grob (gTree) object made with package \code{grid}
}
\description{
It uses Grid Graphics (package \code{grid}) to Convert a flextable
into a grob object with scaling and text wrapping capabilities.

This method can be used to insert a flextable inside a \code{ggplot2} plot,
it can also be used with package 'patchwork' or 'cowplot' to combine
ggplots and flextables into the same graphic.

User can vary the size of the elements according to the size of the graphic window. The text
behavior is controllable, user can decide to make the paragraphs (texts and images)
distribute themselves correctly in the available space of the cell. It is possible
to define resizing options, for example by using only the width, or by distributing
the content so that it occupies the whole graphic space. It is also possible to
freeze or not the size of the columns.

It is not recommended to use this function for
large tables because the calculations can be long.

Limitations: equations (see \code{\link[=as_equation]{as_equation()}}) and hyperlinks (see \code{\link[officer:hyperlink_ftext]{officer::hyperlink_ftext()}})
will not be displayed.

'ragg' or 'svglite' or 'ggiraph' graphical device drivers
should be used to ensure a correct rendering.
}
\section{size}{


The size of the flextable can be known by using the method
\link[=dim.flextableGrob]{dim} on the grob.
}

\section{caption}{

It's important to note that captions are not part of the table itself.
This means when exporting a table to PNG or SVG formats (image formats),
the caption won't be included. Captions are intended for document outputs
like Word, HTML, or PDF, where tables are embedded within the document
itself.
}

\examples{
\dontshow{
data.table::setDTthreads(1)
}
library(ragg)
library(gdtools)
register_liberationsans()

set_flextable_defaults(font.family = "Liberation Sans")

ft <- flextable(head(mtcars))

gr <- gen_grob(ft)

png_f_1 <- tempfile(fileext = ".png")
ragg::agg_png(
  filename = png_f_1, width = 4, height = 2,
  units = "in", res = 150)
plot(gr)
dev.off()

png_f_2 <- tempfile(fileext = ".png")
# get the size
dims <- dim(gr)
dims
ragg::agg_png(
  filename = png_f_2, width = dims$width + .1,
  height = dims$height + .1, units = "in", res = 150
)
plot(gr)
dev.off()


if (require("ggplot2")) {
  png_f_3 <- tempfile(fileext = ".png")
  z <- summarizor(iris, by = "Species")
  ft <- as_flextable(z, spread_first_col = TRUE)
  ft <- color(ft, color = "gray", part = "all")
  gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Width)) +
    annotation_custom(
      gen_grob(ft, scaling = "full"),
      xmin  = 4.5, xmax = 7.5, ymin = 0.25, ymax = 2.25) +
    geom_point() +
    theme_minimal()
  ragg::agg_png(
    filename = png_f_3, width = 7,
    height = 7, units = "in", res = 150
  )
  print(gg)
  dev.off()
}


}
\seealso{
Other flextable print function: 
\code{\link{df_printer}()},
\code{\link{flextable_to_rmd}()},
\code{\link{htmltools_value}()},
\code{\link{knit_print.flextable}()},
\code{\link{plot.flextable}()},
\code{\link{print.flextable}()},
\code{\link{save_as_docx}()},
\code{\link{save_as_html}()},
\code{\link{save_as_image}()},
\code{\link{save_as_pptx}()},
\code{\link{save_as_rtf}()},
\code{\link{to_html.flextable}()}
}
\concept{flextable print function}
