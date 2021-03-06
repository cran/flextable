% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_content.R
\name{chunk_dataframe}
\alias{chunk_dataframe}
\title{create a chunk representation suitable for flextable}
\usage{
chunk_dataframe(...)
}
\arguments{
\item{...}{values to set.}
}
\value{
a data.frame with an additional class "chunk" that makes it
suitable for beeing used in \code{\link[=as_paragraph]{as_paragraph()}}
}
\description{
This function is to be used by external packages
that want to provide an object that can be inserted as a chunk
object in paragraphs of a flextable object.
}
\section{text pattern with default values}{
\preformatted{chunk_dataframe(txt = c("any text", "other text"))
}
}

\section{text pattern with bold set to TRUE}{
\preformatted{chunk_dataframe(
  txt = c("any text", "other text"),
  bold = c(TRUE, TRUE))
}
}

\section{text pattern with control over all formatting properties}{
\preformatted{chunk_dataframe(
  txt = c("any text", "other text"),
  font.size = c(12, 10),
  italic = c(FALSE, TRUE),
  bold = c(FALSE, TRUE),
  underlined = c(FALSE, TRUE),
  color = c("black", "red"),
  shading.color = c("transparent", "yellow"),
  font.family = c("Arial", "Arial"),
  hansi.family = c("Arial", "Arial"),
  eastasia.family = c("Arial", "Arial"),
  cs.family = c("Arial", "Arial"),
  vertical.align = c("top", "bottom") )
}
}

\section{text with url pattern}{
\preformatted{chunk_dataframe(
  txt = c("any text", "other text"),
  url = rep("https://www.google.fr", 2),
  font.size = c(12, 10),
  italic = c(FALSE, TRUE),
  bold = c(FALSE, TRUE),
  underlined = c(FALSE, TRUE),
  color = c("black", "red"),
  shading.color = c("transparent", "yellow"),
  font.family = c("Arial", "Arial"),
  hansi.family = c("Arial", "Arial"),
  eastasia.family = c("Arial", "Arial"),
  cs.family = c("Arial", "Arial"),
  vertical.align = c("top", "bottom") )
}
}

\section{images pattern}{
\preformatted{chunk_dataframe(width = width, height = height, img_data = files )
}
}

\keyword{internal}
