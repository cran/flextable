% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment_rows.R
\name{delete_columns}
\alias{delete_columns}
\title{Delete flextable columns}
\usage{
delete_columns(x, j = NULL)
}
\arguments{
\item{x}{a \code{flextable} object}

\item{j}{columns selection}
}
\description{
The function removes one or more columns
from a 'flextable'.
}
\details{
Deleting one or more columns will result in the deletion
of any span parameters that may have been set previously.
They will have to be redone after this operation or
performed only after this deletion.
}
\examples{
ft <- flextable(head(iris))
ft <- delete_columns(ft, j = "Species")
ft
}
\seealso{
Other functions for row and column operations in a flextable: 
\code{\link{add_body}()},
\code{\link{add_body_row}()},
\code{\link{add_footer}()},
\code{\link{add_footer_lines}()},
\code{\link{add_footer_row}()},
\code{\link{add_header}()},
\code{\link{add_header_row}()},
\code{\link{delete_part}()},
\code{\link{delete_rows}()},
\code{\link{separate_header}()},
\code{\link{set_header_footer_df}},
\code{\link{set_header_labels}()}
}
\concept{functions for row and column operations in a flextable}
