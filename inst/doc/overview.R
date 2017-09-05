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

ft <- regulartable(data, 
          col_keys = c("Species", "sep_1", "Sepal.Length", "Sepal.Width", "sep_2",  "Petal.Length", "Petal.Width" ) ) %>% 
  set_header_df(mapping = typology, key = "col_keys" ) %>% 
  merge_h(part = "header") %>% 
  merge_v(j = "Species", part = "body") %>% 
  merge_v(j = "Species", part = "header") %>% 
  theme_vanilla() %>% empty_blanks() %>% autofit() 

tabwid(ft)

## ----warning=FALSE, message=FALSE----------------------------------------
library(flextable)
library(officer)

myft <- regulartable(head(mtcars), 
                  col_keys = c("am", "carb", "gear", "mpg", "drat" ))
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% theme_vanilla()
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>%
  merge_v(j = c("am", "carb") )
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>%
  set_header_labels( carb = "# carb." ) %>% 
  width(width = .75) # set width of all columns to .75 in
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% italic(j = 1) %>% 
  bg(bg = "#C90000", part = "header") %>% 
  color(color = "white", part = "header") %>% 
  border(border = fp_border(color = "orange"), part = "all")
  
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% 
  color(~ drat > 3.5, ~ drat, color = "red") %>% 
  bold(~ drat > 3.5, ~ drat, bold = TRUE) %>% 
  autofit()

tabwid(myft)

## ------------------------------------------------------------------------
library(officer)

## ------------------------------------------------------------------------
ft <- regulartable(head(mtcars)) %>% 
  theme_booktabs() %>% 
  autofit()

ppt <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_flextable(value = ft, type = "body") 

print(ppt, target = "assets/pptx/example.pptx") %>% invisible()

## ----echo=FALSE----------------------------------------------------------
office_doc_link( url = paste0( "https://davidgohel.github.io/flextable/articles/", "assets/pptx/example.pptx" ) )

## ------------------------------------------------------------------------
doc <- read_docx() %>% 
  body_add_flextable(value = ft)
print(doc, target = "assets/docx/example.docx") %>% invisible()

## ----echo=FALSE----------------------------------------------------------
office_doc_link( url = paste0( "https://davidgohel.github.io/flextable/articles/", "assets/docx/example.docx" ) )

