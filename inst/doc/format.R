## ----echo = FALSE--------------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>", 
  eval = !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to"))
)

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

## ----warning=FALSE, echo=FALSE, message=FALSE----------------------------
library(officer)
library(flextable)

## ------------------------------------------------------------------------
myft <- flextable(head(iris))
myft

## ------------------------------------------------------------------------
myft <- flextable(head(iris)) 
myft <- bold(myft, part = "header") # bold header
myft

## ------------------------------------------------------------------------
myft <- fontsize(myft, part = "header", size = 12) 
myft

## ------------------------------------------------------------------------
myft <- color(myft, color = "#E4C994")
myft

## ------------------------------------------------------------------------
myft <- italic(myft, i = ~ Sepal.Length > 5, 
         j = ~ Sepal.Length + Sepal.Width, italic = TRUE)
myft

## ------------------------------------------------------------------------
# light gray as background color for header
myft <-  bg(myft, bg = "#E4C994", part = "header")
# dark gray as background color for body
myft <-  bg(myft, bg = "#333333", part = "body")
myft

## ------------------------------------------------------------------------
myft <- align( myft, align = "center", part = "all" )
myft

## ------------------------------------------------------------------------
myft <- padding( myft, padding = 3, part = "all" )
myft

## ------------------------------------------------------------------------
myft <- font(myft, j = "Species", fontname = "Times")
myft <- fontsize(myft, j = "Species", size = 14)
myft

## ------------------------------------------------------------------------
ft <- flextable(head(iris))
ft <- rotate(ft, rotation = "tbrl", align = "top", part = "header")
ft <- theme_vanilla(ft)
ft <- autofit(ft)

# as autofit do not handle rotation, you will have
# to change manually header cells'height.
ft <- height(ft, height = 1, part = "header")
ft

## ------------------------------------------------------------------------
# remove all defined borders
myft <- border_remove( myft )

big_b <- fp_border(color="gray70", width = 2)
std_b <- fp_border(color="white")

myft <- vline( myft, border = std_b, part = "all" )
myft <- vline_left( myft, border = big_b, part = "all" )
myft <- vline_right( myft, border = big_b, part = "all" )
myft <- hline( myft, border = std_b )
myft <- hline_bottom( myft, border = big_b )
myft <- hline_top( myft, border = big_b, part = "all" )
myft

## ------------------------------------------------------------------------
std_b2 <- fp_border(color="white", style = "dashed")

# remove all defined borders
myft <- border_remove( myft )

myft <- border_outer( myft, border = big_b, part = "all" )
myft <- border_inner_h( myft, border = std_b, part = "all" )
myft <- border_inner_v( myft, border = std_b2, part = "all" )
myft

## ----warning=FALSE, message=FALSE----------------------------------------
myft <- color(myft, i = ~ Sepal.Length < 5 & Petal.Length > 1.3, 
        j = ~ Petal.Width + Species, 
        color="red")
myft <- bg(myft, j = 1, bg = "#D3C994", part = "header")
myft <- italic(myft, i = ~ Sepal.Length > 5)
myft <- bold(myft, i = 4, j = "Sepal.Length")
myft

## ----warning=FALSE, message=FALSE----------------------------------------
row_id <- with(head(iris), Sepal.Length < 5 & Petal.Length > 1.3 )
col_id <- c("Petal.Width", "Species")

myft <- color(myft, i = row_id, j = col_id, color="red") 

myft

## ------------------------------------------------------------------------
library(officer)
def_cell <- fp_cell(border = fp_border(color="#00C9C9"))
def_par <- fp_par(text.align = "center")
def_text <- fp_text(color="#999999", italic = TRUE)
def_text_header <- update(color="black", def_text, bold = TRUE)

ft <- flextable(head(mtcars, n = 10 ))
ft <- style( ft, pr_c = def_cell, pr_p = def_par, pr_t = def_text, part = "all")  
ft

ft <- style( ft, pr_t = def_text_header, part = "header")  
ft

## ------------------------------------------------------------------------
dat <- head(mtcars, n = 10)
dat[3:7, 1] <- NA
dat[, 2] <- dat[, 6] * 1000000

ft <- flextable(dat)
num_keys <- c("mpg", "disp", "drat", "wt", "qsec")
int_keys <- c("cyl", "hp", "vs", "am", "gear", "carb")

ft <- colformat_num(x = ft, col_keys = num_keys, big.mark = ",", digits = 2, na_str = "missing")
ft <- colformat_int(x = ft, col_keys = int_keys, big.mark = ",")
autofit(ft)

## ------------------------------------------------------------------------
ft <- flextable(head(mtcars, n = 10 ), 
                   col_keys = c("gear", "mpg", "qsec"))
ft <- set_formatter(ft, 
    mpg = function(x) sprintf("%.04f", x),
    gear = function(x) sprintf("%.0f gears", x)
  )
ft <- theme_booktabs(ft)
ft <- autofit(ft)
ft

## ------------------------------------------------------------------------
myft <- flextable( head(mtcars), 
  col_keys = c("am", "separator", "gear", "mpg", "drat", "qsec" ))
myft <- bold(myft, part = "header")
myft <- border(myft, border = fp_border( width = 0), 
  border.top = fp_border(), border.bottom = fp_border(), 
  part = "all")
myft <- align(myft, align = "right", part = "all" )
myft <- border(myft, j = ~ separator, border = fp_border(width=0), part = "all")
myft <- width(myft, j = ~ separator, width = .1)
myft

## ------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", 
  value = as_paragraph(
    "mpg value is ", 
    as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ) )
  )
myft <- autofit(myft)
myft

## ------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", 
  value = as_paragraph(
    "mpg value is ", 
    as_chunk(sprintf("%.01f", mpg), props = fp_text(color = "red", bold = TRUE) ), 
    " with ",
    as_chunk(sprintf("# %.0f", carb), props = fp_text(color = "gray", italic = TRUE) )
    )
  )

myft <- autofit(myft)
myft

## ------------------------------------------------------------------------
myft <- compose( 
  myft, j = "mpg", part = "header",
  value = as_paragraph(
    "Miles/(US) gallon ", 
    as_chunk("* with num of carb.", props = fp_text(color = "gray", vertical.align = "superscript") )
    )
  )

myft <- autofit(myft)
myft

## ------------------------------------------------------------------------
img.file <- file.path( R.home("doc"), "html", "logo.jpg" )

myft <- compose( myft, i = ~ qsec > 18, j = "qsec", 
  value = as_paragraph(as_image( src = img.file, width = .20, height = .15))
)
myft <- autofit(myft)
myft

## ------------------------------------------------------------------------
myft <- flextable( head(iris, n = 10 ))

myft <- compose( myft, j = 1,
  value = as_paragraph(
    minibar(value = Sepal.Length, max = max(Sepal.Length))
  ),
  part = "body")

autofit(myft)

