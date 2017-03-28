## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE----------------------------------------
library(flextable)
library(officer)
library(magrittr)

## ------------------------------------------------------------------------
library(dplyr)
iris %>% 
  group_by(Species) %>% 
  do( head(., n = 3) ) %>% 
  flextable(col_keys = c("Species", "Sepal.Length", "Petal.Length") ) %>%
  theme_booktabs() %>% 
  tabwid()


## ------------------------------------------------------------------------
iris %>% group_by(Species) %>% do( head(., n = 3) ) %>%
  flextable(col_keys = c("Species", "col_1", "Sepal.Length", "Petal.Length") ) %>%
  theme_vanilla() %>% autofit() %>% 
  border(j=2, border = fp_border(width=0), part = "all") %>% 
  tabwid()


## ----warning=FALSE, message=FALSE----------------------------------------
ft <- flextable( head( iris ) ) %>% 
  set_header_labels(Sepal.Length = "Sepal", 
    Sepal.Width = "Sepal", Petal.Length = "Petal",
    Petal.Width = "Petal", Species = "Species" )
  
ft %>% theme_vanilla() %>% autofit() %>% tabwid()

## ----warning=FALSE, message=FALSE----------------------------------------
ft <- ft %>% 
  add_header(Sepal.Length = "length",
    Sepal.Width = "width", Petal.Length = "length",
    Petal.Width = "width", Species = "Species", top = FALSE ) 
ft %>% theme_vanilla() %>% autofit() %>% tabwid()
ft <- ft %>% 
  add_header(Sepal.Length = "Inches",
    Sepal.Width = "Inches", Petal.Length = "Inches",
    Petal.Width = "Inches", Species = "Species", top = TRUE )
ft %>% theme_vanilla() %>% autofit() %>% tabwid()

## ----warning=FALSE, message=FALSE----------------------------------------
typology <- data.frame(
  col_keys = c( "Sepal.Length", "Sepal.Width", "Petal.Length",
                "Petal.Width", "Species" ),
  type = c("double", "double", "double", "double", "factor"),
  what = c("Sepal", "Sepal", "Petal", "Petal", "Species"),
  measure = c("Length", "Width", "Length", "Width", "Species"),
  stringsAsFactors = FALSE )
typology %>% flextable() %>% theme_vanilla() %>% autofit() %>% tabwid()

## ----warning=FALSE, message=FALSE----------------------------------------
flextable( head( iris ) ) %>% 
  set_header_df( mapping = typology, key = "col_keys" ) %>% 
  theme_vanilla() %>% autofit() %>% tabwid()

## ----warning=FALSE, message=FALSE----------------------------------------
select_columns <- c("Species", "Petal.Length", "Petal.Width")
myft <- flextable(iris[46:55,], col_keys = select_columns) %>% 
  flextable::merge_v(~ Species + Petal.Width )

tabwid(myft) 

## ----warning=FALSE, message=FALSE----------------------------------------
select_columns <- c("Species", "Petal.Length", "Petal.Width")
myft <- flextable(head(mtcars, n = 10 ) ) %>% 
  flextable::merge_h( ) %>% # merge
  border(border = fp_border(), part = "all") # and add borders

tabwid(myft)

## ------------------------------------------------------------------------
tabwid(myft %>% merge_none())

## ------------------------------------------------------------------------
ft_base <- flextable(head(iris)) %>% 
  theme_tron_legacy()
tabwid(ft_base)
dim(ft_base)

## ------------------------------------------------------------------------
dim_pretty(ft_base)

## ------------------------------------------------------------------------
ft <- ft_base %>% 
  autofit(add_w = 0, add_h = 0)

dim(ft)
tabwid(ft)

## ------------------------------------------------------------------------
ft <- ft_base %>% autofit() %>% 
  width(j = ~ Species, width = 2) %>% 
  height(i = 4:5, height = .5)
tabwid(ft)

