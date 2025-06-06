#' @title Set column formatter functions
#' @description Apply formatter functions to column keys.
#'
#' Functions should have a single argument (the vector) and should
#' return the formatted values as a character vector.
#' @param x a flextable object
#' @param ... Name-value pairs of functions, names should be existing col_key values
#' @param values format functions, If values is supplied argument `...` is ignored.
#' - It can be a list of name-value pairs of functions, names
#' should be existing col_key values.
#' - If `values` is a single function, it will be applied to each column.
#' @param part part of the table (one of 'body' or 'header' or 'footer')
#' where to apply the formatter functions.
#' @examples
#' ft <- flextable(head(iris))
#' ft <- set_formatter(
#'   x = ft,
#'   Sepal.Length = function(x) sprintf("%.02f", x),
#'   Sepal.Width = function(x) sprintf("%.04f", x)
#' )
#' ft <- theme_vanilla(ft)
#' ft
#' @export
#' @family cells formatters
set_formatter <- function(x, ..., values = NULL, part = "body") {
  if (!inherits(x, "flextable")) {
    stop(sprintf("Function `%s` supports only flextable objects.", "set_formatter()"))
  }

  part <- match.arg(part, c("body", "header", "footer"), several.ok = FALSE)

  if (is.null(values)) {
    values <- list(...)
  } else if (is.function(values)) {
    values <- lapply(x$col_keys, function(...) {
      values
    })
    names(values) <- x$col_keys
  }
  col_keys <- intersect(names(values), x[[part]]$col_keys)

  for (key in col_keys) {
    dat <- x[[part]]$dataset[, key]
    chk <- as_chunk(values[[key]](dat))
    x <- compose(x, j = key, value = as_paragraph(chk), part = part)
  }

  x
}

#' @export
#' @title Format character cells
#' @description Format character cells in a flextable.
#' @param x a flextable object
#' @param i rows selection
#' @param j columns selection.
#' @param na_str,nan_str string to be used for NA and NaN values
#' @param prefix,suffix string to be used as prefix or suffix
#' @family cells formatters
#' @examples
#' dat <- iris
#' z <- flextable(head(dat))
#' ft <- colformat_char(
#'   x = z, j = "Species", suffix = "!"
#' )
#' z <- autofit(z)
#' z
colformat_char <- function(
    x, i = NULL, j = NULL,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  quo_fun <- quo(format_fun.character(
    x,
    na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(x = , na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix),
    get_expr(quo_fun)
  )

  col_keys <- filter_col_keys(x, j, function(x) is.character(x) || is.factor(x))
  docall_display(col_keys, fun_, x, i = i)
}

#' @export
#' @title Format numeric cells
#' @description Format numeric cells in a flextable.
#' @inheritParams colformat_char
#' @param big.mark,digits,decimal.mark see [formatC()]
#' @family cells formatters
#' @examples
#' dat <- mtcars
#' ft <- flextable(head(dat))
#' ft <- colformat_double(
#'   x = ft,
#'   big.mark = ",", digits = 2, na_str = "N/A"
#' )
#' autofit(ft)
#' @importFrom rlang new_function quo get_expr pairlist2
colformat_double <- function(
    x, i = NULL, j = NULL,
    big.mark = get_flextable_defaults()$big.mark,
    decimal.mark = get_flextable_defaults()$decimal.mark,
    digits = get_flextable_defaults()$digits,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, function(x) is.double(x) && !inherits(x, "POSIXt") && !inherits(x, "Date"))

  quo_fun <- quo(format_fun.double(
    x,
    big.mark = big.mark, decimal.mark = decimal.mark,
    digits = digits, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(
      x = , big.mark = big.mark, decimal.mark = decimal.mark,
      digits = digits, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
    ),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}
#' @export
#' @title Format numeric cells
#' @description Format numeric cells in a flextable.
#'
#' The function is different from [colformat_double()] on numeric type
#' columns. The function uses the [format()] function of R on numeric
#' type columns. So this is normally what you see on the R console
#' most of the time (but scientific mode is disabled and NA are replaced).
#'
#' @section format call:
#'
#' Function [format()] is called with the following values:
#'
#' * `trim` is set to TRUE,
#' * `scientific` is set to FALSE,
#' * `big.mark` is set to the value of `big.mark` argument,
#' * `decimal.mark` is set to the value of `decimal.mark` argument,
#' * other arguments are passed 'as is' to the format function.
#'
#' argument `digits` is ignored as it is not the same `digits` that users
#' want, this one will be used by [format()] and not [formatC()].
#' To change the digit argument use `options(digits=4)` instead.
#'
#' This argument will not be changed because `colformat_num()`
#' is supposed to format things roughly as what you see on the R console.
#'
#' If these functions does not fit your needs, use [set_formatter()]
#' that lets you use any format function.
#'
#' @inheritParams colformat_char
#' @param big.mark,decimal.mark see [format()]
#' @param ... additional argument for function [format()], `scientific`
#' and `digits` can not be used.
#' @family cells formatters
#' @examples
#' dat <- mtcars
#' dat[2, 1] <- NA
#' ft <- flextable(head(dat))
#' ft <- colformat_num(
#'   x = ft,
#'   big.mark = " ", decimal.mark = ",",
#'   na_str = "N/A"
#' )
#' ft <- autofit(ft)
#' ft
colformat_num <- function(
    x, i = NULL, j = NULL,
    big.mark = get_flextable_defaults()$big.mark,
    decimal.mark = get_flextable_defaults()$decimal.mark,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "", ...) {
  stopifnot(inherits(x, "flextable"))
  col_keys <- filter_col_keys(x, j, is.numeric)

  quo_fun <- quo(format_fun.default(
    x,
    big.mark = big.mark, decimal.mark = decimal.mark,
    na_str = na_str, nan_str = nan_str,
    prefix = prefix, suffix = suffix,
    ...
  ))
  fun_ <- new_function(
    pairlist2(
      x = , big.mark = big.mark, decimal.mark = decimal.mark,
      na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix,
      ...
    ),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}

#' @title Format date cells
#' @description Format date cells in a flextable.
#' @inheritParams colformat_char
#' @param fmt_date see [strptime()]
#' @family cells formatters
#' @export
#' @examples
#' dat <- data.frame(
#'   z = Sys.Date() + 1:3,
#'   w = Sys.Date() - 1:3
#' )
#' ft <- flextable(dat)
#' ft <- colformat_date(x = ft)
#' ft <- autofit(ft)
#' ft
colformat_date <- function(
    x, i = NULL, j = NULL,
    fmt_date = get_flextable_defaults()$fmt_date,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, function(x) inherits(x, "Date"))

  quo_fun <- quo(format_fun.Date(
    x,
    fmt_date = fmt_date, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(x = , fmt_date = fmt_date, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}

#' @title Format datetime cells
#' @description Format datetime cells in a flextable.
#' @inheritParams colformat_char
#' @param fmt_datetime see [strptime()]
#' @family cells formatters
#' @export
#' @examples
#' dat <- data.frame(
#'   z = Sys.time() + (1:3) * 24,
#'   w = Sys.Date() - (1:3) * 24
#' )
#' ft <- flextable(dat)
#' ft <- colformat_datetime(x = ft)
#' ft <- autofit(ft)
#' ft
colformat_datetime <- function(
    x, i = NULL, j = NULL,
    fmt_datetime = get_flextable_defaults()$fmt_datetime,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, function(x) inherits(x, "POSIXt"))

  quo_fun <- quo(format_fun.POSIXt(
    x,
    fmt_datetime = fmt_datetime, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(x = , fmt_datetime = fmt_datetime, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}

#' @title Format integer cells
#' @description Format integer cells in a flextable.
#' @inheritParams colformat_char
#' @param big.mark see [format()]
#' @family cells formatters
#' @export
#' @examples
#' z <- flextable(head(mtcars))
#' j <- c("vs", "am", "gear", "carb")
#' z <- colformat_int(x = z, j = j, prefix = "# ")
#' z
colformat_int <- function(
    x, i = NULL, j = NULL,
    big.mark = get_flextable_defaults()$big.mark,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, is.integer)

  quo_fun <- quo(format_fun.integer(
    x,
    big.mark = big.mark, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(x = , big.mark = big.mark, na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}

#' @title Format logical cells
#' @description Format logical cells in a flextable.
#' @inheritParams colformat_char
#' @param false,true string to be used for logical
#' @family cells formatters
#' @export
#' @examples
#' dat <- data.frame(a = c(TRUE, FALSE), b = c(FALSE, TRUE))
#'
#' z <- flextable(dat)
#' z <- colformat_lgl(x = z, j = c("a", "b"))
#' autofit(z)
colformat_lgl <- function(
    x, i = NULL, j = NULL,
    true = "true", false = "false",
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, is.logical)

  quo_fun <- quo(format_fun.logical(
    x,
    true = true, false = false,
    na_str = na_str, nan_str = nan_str, prefix = prefix, suffix = suffix
  ))
  fun_ <- new_function(
    pairlist2(
      x = , true = true, false = false,
      na_str = na_str, nan_str = nan_str,
      prefix = prefix, suffix = suffix
    ),
    get_expr(quo_fun)
  )

  docall_display(col_keys, fun_, x, i = i)
}

#' @title Format cells as images
#' @description Format image paths as images in a flextable.
#' @inheritParams colformat_char
#' @param width,height size of the png file in inches
#' @family cells formatters
#' @export
#' @examples
#' img.file <- file.path(R.home("doc"), "html", "logo.jpg")
#'
#' dat <- head(iris)
#' dat$Species <- as.character(dat$Species)
#' dat[c(1, 3, 5), "Species"] <- img.file
#'
#' myft <- flextable(dat)
#' myft <- colformat_image(
#'   myft,
#'   i = c(1, 3, 5),
#'   j = "Species", width = .20, height = .15
#' )
#' ft <- autofit(myft)
#' ft
colformat_image <- function(
    x, i = NULL, j = NULL,
    width, height,
    na_str = get_flextable_defaults()$na_str,
    nan_str = get_flextable_defaults()$nan_str,
    prefix = "", suffix = "") {
  stopifnot(inherits(x, "flextable"))

  col_keys <- filter_col_keys(x, j, is.character)

  check_formula_i_and_part(i, "body")
  for (varname in col_keys) {
    x <- compose(
      x = x, j = varname, i = i, value =
        as_paragraph(
          prefix,
          as_image(get(varname), width = width, height = height),
          suffix
        ),
      part = "body"
    )
  }
  x
}


filter_col_keys <- function(x, j, fun) {
  j <- get_columns_id(x[["body"]], j)
  col_keys <- x$col_keys[j]
  col_keys[vapply(x[["body"]]$dataset[col_keys], fun, FUN.VALUE = NA)]
}

docall_display <- function(col_keys, fun, x, i = NULL, part = "body") {
  check_formula_i_and_part(i, part)
  for (varname in col_keys) {
    x <- mk_par(
      x = x,
      j = varname,
      i = i,
      value = as_paragraph(as_chunk(fun(get(varname)))),
      part = part
    )
  }
  x
}
