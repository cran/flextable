## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>"
)

dir.create("assets/format", recursive = TRUE, showWarnings = FALSE)
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
myft <- myft %>% fontsize(part = "header", size = 12) 
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

## ------------------------------------------------------------------------
ft <- flextable(head(iris)) %>% 
  rotate(rotation = "tbrl", align = "top", part = "header") %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  # as autofit do not handle rotation, you will have
  # to change manually header cells'height.
  height(height = 1, part = "header")

## ------------------------------------------------------------------------
library(officer)
read_docx() %>% 
  body_add_flextable(ft) %>% 
  print(target = "assets/format/rotate.docx") %>% 
  invisible()

## ----echo=FALSE----------------------------------------------------------
office_doc_link( url = paste0( "https://davidgohel.github.io/flextable/articles/", "assets/format/rotate.docx" ) )

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- myft %>% 
  color(i = ~ Sepal.Length < 5 & Petal.Length > 1.3, 
        j = ~ Petal.Width + Species, 
        color="red") %>% 
  bg(j = 1, bg = "#D3C994", part = "header") %>% 
  italic(i = ~ Sepal.Length > 5) %>% 
  bold( i = 4, j = "Sepal.Length")
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
  style( pr_c = def_cell, pr_p = def_par, pr_t = def_text, part = "all")  
tabwid(ft)

ft <- ft %>% 
  style( pr_t = def_text_header, part = "header")  
tabwid(ft)

## ------------------------------------------------------------------------
myft <- flextable( head(mtcars), 
  col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" )) %>% 
  bold(part = "header") %>% 
  border(border = fp_border( width = 0), 
         border.top = fp_border(), border.bottom = fp_border(), 
         part = "all") %>% 
  align(align = "right", part = "all" ) %>%
  border(j = ~ separator, border = fp_border(width=0), part = "all") %>% 
  width(j = ~ separator, width = .1)

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display( col_key = "mpg", pattern = "{{mpg}}", 
           formatters = list(mpg ~ sprintf("%.01f", mpg) ), 
              fprops = list(mpg = fp_text(color = "red", italic = TRUE) )
  )

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display( i = ~ drat > 3.6, 
           col_key = "mpg", pattern = "{{mpg}} with {{carb}}", 
           formatters = list(mpg ~ sprintf("%.01f", mpg), 
                             carb ~ sprintf("# %.0f carb.", carb) ), 
              fprops = list(mpg = fp_text(color = "#CC55CC", bold = TRUE) )
  ) %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
myft <- myft %>%
  display( col_key = "mpg", pattern = "{{mpg}} {{my_message}}", part = "header",
           formatters = list(mpg ~ "Miles/(US) gallon", 
                             my_message ~ sprintf("* with num of carb.") ), 
              fprops = list(my_message = fp_text(color = "gray", vertical.align = "superscript")
                            )
  ) %>% autofit()

tabwid(myft)

## ------------------------------------------------------------------------
img.file <- file.path( Sys.getenv("R_HOME"), "doc", "html", "logo.jpg" )

myft <- myft %>%
  display( i = ~ qsec > 18, col_key = "qsec", 
           pattern = "blah blah {{r_logo}} {{qsec}}",
           formatters = list(
             r_logo ~ as_image(qsec, src = img.file, width = .20, height = .15), 
             qsec ~ sprintf("qsec: %.1f", qsec) ), 
           fprops = list(qsec = fp_text(color = "orange", vertical.align = "superscript"))
           ) %>% 
  autofit()

tabwid(myft)

