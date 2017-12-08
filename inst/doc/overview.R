## ---- echo = FALSE, message=FALSE, warning=FALSE-------------------------
dir.create("assets/docx", recursive = TRUE, showWarnings = FALSE)
dir.create("assets/pptx", recursive = TRUE, showWarnings = FALSE)
office_doc_link <- function(url){
  stopifnot(requireNamespace("htmltools", quietly = TRUE))
  htmltools::tags$p(  htmltools::tags$span("Download file "),
    htmltools::tags$a(basename(url), href = url), 
    htmltools::tags$span(" - view with"),
    htmltools::tags$a("office web viewer", target="_blank", 
      href = paste0("https://view.officeapps.live.com/op/view.aspx?src=", url)
      ), 
    style="text-align:center;font-style:italic;color:gray;"
    )
}

## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
data <- iris[c(1:3, 51:53, 101:104),]

## ----warning=FALSE, echo=FALSE-------------------------------------------
library(flextable)
library(officer)

typology <- data.frame(
  col_keys = c( "Sepal.Length", "Sepal.Width", "Petal.Length",
                "Petal.Width", "Species" ),
  what = c("Sepal", "Sepal", "Petal", "Petal", "Species"),
  measure = c("Length", "Width", "Length", "Width", "Species"),
  stringsAsFactors = FALSE )

ft <- regulartable(
  data, 
  col_keys = c("Species", "sep_1", "Sepal.Length", "Sepal.Width", 
               "sep_2",  "Petal.Length", "Petal.Width" ) )

ft <- set_header_df(ft, mapping = typology, key = "col_keys" )
ft <- merge_h(ft, part = "header")
ft <- merge_v(ft, j = "Species", part = "body")
ft <- merge_v(ft, j = "Species", part = "header")
ft <- theme_vanilla(ft)
ft <- empty_blanks(ft)
autofit(ft) 

## ----warning=FALSE, message=FALSE----------------------------------------
library(flextable)
library(officer)

myft <- regulartable(
  head(mtcars), 
  col_keys = c("am", "carb", "gear", "mpg", "drat" ))
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- theme_vanilla(myft)
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- merge_v(myft, j = c("am", "carb") )
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- set_header_labels( myft, carb = "# carb." )
myft <- width(myft, width = .75) # set width of all columns to .75 in
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- autofit(myft)
myft

## ------------------------------------------------------------------------
myft <- italic(myft, j = 1)
myft <- bg(myft, bg = "#C90000", part = "header")
myft <- color(myft, color = "white", part = "header")
myft <- border(myft, border = fp_border(color = "orange"), part = "all")
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- color(myft, ~ drat > 3.5, ~ drat, color = "red")
myft <- bold(myft, ~ drat > 3.5, ~ drat, bold = TRUE)
myft <- autofit(myft)

myft

## ------------------------------------------------------------------------
library(officer)

## ----results='hide'------------------------------------------------------
ft <- regulartable(head(mtcars))
ft <- theme_booktabs(ft)
ft <- autofit(ft)

ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with_flextable(ppt, value = ft, type = "body") 

print(ppt, target = "assets/pptx/example.pptx")

## ----echo=FALSE----------------------------------------------------------
office_doc_link( url = paste0( "https://davidgohel.github.io/flextable/articles/", "assets/pptx/example.pptx" ) )

## ----results='hide'------------------------------------------------------
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "assets/docx/example.docx")

## ----echo=FALSE----------------------------------------------------------
office_doc_link( url = paste0( "https://davidgohel.github.io/flextable/articles/", "assets/docx/example.docx" ) )

