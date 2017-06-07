## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, echo=FALSE-------------------------------------------
library(flextable)
library(officer)
library(dplyr)

typology <- data.frame(
  col_keys = c( "Sepal.Length", "Sepal.Width", "Petal.Length",
                "Petal.Width", "Species" ),
  what = c("Sepal", "Sepal", "Petal", "Petal", "Species"),
  measure = c("Length", "Width", "Length", "Width", "Species"),
  stringsAsFactors = FALSE )


data <- iris %>% 
  group_by(Species) %>% 
  do( head(., n = 3) )

ft <- flextable(data, 
          col_keys = c("Species", "sep_1", "Sepal.Length", "Sepal.Width", "sep_2",  "Petal.Length", "Petal.Width" ) ) %>% 
  set_header_df(mapping = typology, key = "col_keys" ) %>% 
  merge_h(part = "header") %>% 
  merge_v(j = "Species", part = "body") %>% 
  merge_v(j = "Species", part = "header") %>% 
  theme_vanilla() %>% empty_blanks() %>% autofit() 

tabwid(ft)

## ------------------------------------------------------------------------
data
typology

## ----warning=FALSE, message=FALSE----------------------------------------
library(flextable)
library(officer)
library(dplyr)

myft <- flextable(head(mtcars), 
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

## ------------------------------------------------------------------------
myft <- myft %>% italic(j = 1) %>% 
  bg(bg = "#C90000", part = "header") %>% 
  color(color = "white", part = "header") %>% 
  border(border = fp_border(color = "orange"), part = "all")
  
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% 
  color(~ drat > 3.5, ~ drat, color = "red") %>% 
  bold(~ drat > 3.5, ~ drat, bold = TRUE) 

tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
library(officer)
ft <- flextable(head(mtcars)) %>% 
  theme_zebra() %>% 
  autofit()

ppt <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_flextable(value = ft, type = "body") 
if( interactive() ) print(ppt, target = "test.pptx")

doc <- read_docx() %>% 
  body_add_flextable(value = ft)
if( interactive() ) print(doc, target = "test.docx")

