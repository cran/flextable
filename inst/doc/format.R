## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, echo=FALSE, message=FALSE----------------------------
library(flextable)
library(dplyr)

## ------------------------------------------------------------------------
myft <- flextable(head(iris))
tabwid(myft)

## ------------------------------------------------------------------------
myft <- flextable(head(iris)) %>% 
  # bold header
  bold(part = "header") 
tabwid(myft)

## ------------------------------------------------------------------------
myft <- flextable(head(iris)) %>% 
  # change font size header
  fontsize(part = "header", size = 12) 
tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% color(color = "#E4C994")
tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% 
  italic(i = ~ Sepal.Length > 5, 
         j = ~ Sepal.Length + Sepal.Width, italic = TRUE)
tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% 
  # light gray as background color for header
  bg(bg = "#E4C994", part = "header") %>% 
  # dark gray as background color for body
  bg(bg = "#333333", part = "body")

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% align( align = "center", part = "all" )
tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% padding( padding = 3, part = "all" )
tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>% 
  border( border = fp_border(color="white"), part = "all" )
  
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% 
  color(i = ~ Sepal.Length < 5 & Petal.Length > 1.3, 
        j = ~ Petal.Width + Species, 
        color="red") 
tabwid(myft)

## ----warning=FALSE, message=FALSE----------------------------------------
row_id <- with(head(iris), Sepal.Length < 5 & Petal.Length > 1.3 )
col_id <- c("Petal.Width", "Species")

myft <- color(myft, i = row_id, j = col_id, color="red") 

tabwid(myft)

## ------------------------------------------------------------------------
library(officer)
def_cell <- fp_cell(border = fp_border(color="#00C9C9"))
def_par <- fp_par(text.align = "center")
def_text <- fp_text(color="#999999", italic = TRUE)
def_text_header <- update(color="black", def_text, bold = TRUE)

ft <- flextable(head(mtcars, n = 10 )) %>% 
  style( pr_c = def_cell, pr_p = def_par, pr_t = def_text, part = "body")  
tabwid(ft)

ft <- ft %>% 
  style( pr_c = def_cell, pr_p = def_par, pr_t = def_text_header, part = "header")  
tabwid(ft)

## ------------------------------------------------------------------------
ft <- flextable(head(iris)) %>% 
  style(pr_c = fp_cell(text.direction = "btlr"), 
        part = "header") %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  height(height = 1, part = "header")

tabwid(ft)

## ------------------------------------------------------------------------
myft <- flextable(head(mtcars), 
                  col_keys = c("am", "separator", "gear", "mpg", "drat" )) %>% 
  bold(part = "header") %>% 
  border(border = fp_border( width = 0), 
         border.top = fp_border(), 
         border.bottom = fp_border(), part = "all") %>% 
  align(align = "right", part = "all" ) %>%
  border(j = ~ separator, border = fp_border(width=0), part = "all") %>% 
  width(j = ~ separator, width = .1)

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display( 
    mpg = fpar(formatC(mpg, format = "f", digits = 3 ) ) 
  )

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display(i = ~ drat > 3.6, 
    gear = fpar( ftext( gear, prop = fp_text(bold = TRUE, color="red") ) )
  )

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display(
    mpg = fpar(mpg, " and ", ftext( carb, prop = fp_text(bold = TRUE)), " carb(s)." ) 
  ) %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display( i = ~ gear < 4,
    drat = fpar(minibar( value = drat, max = max(.$drat), 
                         barcol = "#C90000", width = 1, height = .15) )
  ) %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
myft <- flextable(head(iris)) %>% 
    theme_vanilla()

if( require(ionicons) ){
  happy = as_png(name = "happy", fill = "green")
  sad = as_png(name = "sad", fill = "orange")
  happy
  sad
  myft <- myft  %>% 
    display(i = ~ Sepal.Length < 5, 
            Sepal.Length = fpar(external_img(sad, width = .2, height = .2)) ) %>% 
    display(i = ~ Sepal.Length >= 5, 
            Sepal.Length = fpar(external_img(happy, width = .2, height = .2)) )
  tabwid(myft)
}

