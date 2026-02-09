#' @title Summary of contingency tables as HTML table
#' @name tabc
#'
#' @description Shows contingency tables as HTML file in browser or viewer pane, or saves them as file.
#'
#' @param data Optional data frame for use with the pipe operator.
#' @param var.row Variable that should be displayed in the table rows.
#' @param var.col Cariable that should be displayed in the table columns.
#' @param var.labels Character vector with variable names, which will be used
#'          to label variables in the output.
#' @param string.total Character label for the total column / row header
#' @param cell.p Logical, if \code{TRUE}, cell percentage values are shown
#' @param row.p Logical, if \code{TRUE}, row percentage values are shown
#' @param col.p Logical, if \code{TRUE}, column percentage values are shown
#' @param show.obs Logical, if \code{TRUE}, observed values are shown
#' @param show.exp Logical, if \code{TRUE}, expected values are also shown
#' @param show.summary Logical, if \code{TRUE}, a summary row with
#'          chi-squared statistics, degrees of freedom and Cramer's V or Phi
#'          coefficient and p-value for the chi-squared statistics.
#' @param tdcol.n Color for highlighting count (observed) values in table cells. Default is black.
#' @param tdcol.expected Color for highlighting expected values in table cells. Default is cyan.
#' @param tdcol.cell Color for highlighting cell percentage values in table cells. Default is red.
#' @param tdcol.row Color for highlighting row percentage values in table cells. Default is blue.
#' @param tdcol.col Color for highlighting column percentage values in table cells. Default is green.
#' @param emph.total Logical, if \code{TRUE}, the total column and row will be emphasized with a
#'          different background color. See \code{emph.color}.
#' @param emph.color Logical, if \code{emph.total = TRUE}, this color value will be used
#'          for painting the background of the total column and row. Default is a light grey.
#' @param prc.sign The percentage sign that is printed in the table cells, in HTML-format.
#'          Default is \code{"&nbsp;\%"}, hence the percentage sign has a non-breaking-space after
#'          the percentage value.
#' @param hundret Default value that indicates the 100-percent column-sums (since rounding values
#'          may lead to non-exact results). Default is \code{"100.0"}.
#' @param statistics Name of measure of association that should be computed. May
#'          be one of \code{"auto"}, \code{"cramer"}, \code{"phi"}, \code{"spearman"},
#'          \code{"kendall"}, \code{"pearson"} or \code{"fisher"}. See
#'          \code{\link[sjstats]{xtab_statistics}}.
#' @param ... Other arguments, currently passed down to the test statistics functions
#'        \code{chisq.test()} or \code{fisher.test()}.
#' @param encoding String, indicating the charset encoding used for variable and
#'          value labels. Default is \code{NULL}, so encoding will be auto-detected
#'          depending on your platform (e.g., \code{"UTF-8"} for Unix and \code{"Windows-1252"} for
#'          Windows OS). Change encoding if specific chars are not properly displayed (e.g. German umlauts).
#' @param remove.spaces Logical, if \code{TRUE}, leading spaces are removed from all lines in the final string
#'          that contains the html-data. Use this, if you want to remove parantheses for html-tags. The html-source
#'          may look less pretty, but it may help when exporting html-tables to office tools.
#' @param value.labels Character vector (or \code{list} of character vectors)
#'          with value labels of the supplied variables, which will be used
#'          to label variable values in the output.
#'
#' @inheritParams tab_model
#' @inheritParams plot_grpfrq
#'
#' @return Invisibly returns
#'          \itemize{
#'            \item the web page style sheet (\code{page.style}),
#'            \item the web page content (\code{page.content}),
#'            \item the complete html-output (\code{page.complete}) and
#'            \item the html-table with inline-css for use with knitr (\code{knitr})
#'            }
#'            for further use.
#'
#' @examples
#' # prepare sample data set
#' data(efc)
#'
#' # print simple cross table with labels
#' \dontrun{
#' if (interactive()) {
#'   tabc(efc$e16sex, efc$e42dep)
#'
#'   # print cross table with manually set
#'   # labels and expected values
#'   tabc(
#'     efc$e16sex,
#'     efc$e42dep,
#'     var.labels = c("Elder's gender", "Elder's dependency"),
#'     show.exp = TRUE
#'   )
#'
#'   # print minimal cross table with labels, total col/row highlighted
#'   tabc(efc$e16sex, efc$e42dep, cell.p = FALSE, emph.total = TRUE)
#'
#'   # User defined style sheet
#'   tabc(efc$e16sex, efc$e42dep,
#'            CSS = list(css.table = "border: 2px solid;",
#'                       css.tdata = "border: 1px solid;",
#'                       css.horline = "border-bottom: double blue;"))
#'
#'   # ordinal data, use Kendall's tau
#'   tabc(efc$e42dep, efc$quol_5, statistics = "kendall")
#'
#'   # calculate Spearman's rho, with continuity correction
#'   tabc(
#'     efc$e42dep,
#'     efc$quol_5,
#'     statistics = "spearman",
#'     exact = FALSE,
#'     continuity = TRUE
#'   )
#' }
#' }
#' @importFrom stats ftable
#' @importFrom sjstats crosstable_statistics table_values
#' @export
tabc <- function(data,
                     var.row,
                     var.col,
                     weight.by = NULL,
                     title = NULL,
                     var.labels = NULL,
                     value.labels = NULL,
                     wrap.labels = 20,
                     show.obs = FALSE,
                     cell.p = FALSE,
                     row.p = FALSE,
                     col.p = TRUE,
                     show.exp = FALSE,
                     show.legend = FALSE,
                     show.na = TRUE,
                     show.summary = TRUE,
                     drop.empty = TRUE,
                     statistics = c("auto", "cramer", "phi", "spearman", "kendall", "pearson", "fisher"),
                     string.total = "Total",
                     digits = 1,
                     tdcol.n = "black",
                     tdcol.expected = "#339999",
                     tdcol.cell = "#993333",
                     tdcol.row = "#333399",
                     tdcol.col = "#339933",
                     emph.total = FALSE,
                     emph.color = "#f8f8f8",
                     prc.sign = "&nbsp;&#37;",
                     hundret = "100.0",
                     CSS = NULL,
                     encoding = NULL,
                     file = NULL,
                     use.viewer = TRUE,
                     remove.spaces = TRUE,
                     ...) {
  if (missing(var.col) && !missing(var.row)) {
    var.col <- var.row
    var.row <- data
    data <- NULL
  }

  if (is.data.frame(data)) {
    var.name.row <- deparse(substitute(var.row))
    var.name.col <- deparse(substitute(var.col))
    var.row <- data[[var.name.row]]
    var.col <- data[[var.name.col]]

    if (!missing(weight.by)) {
      weight.by.name <- deparse(substitute(weight.by))
      if (weight.by.name %in% names(data)) {
        weight.by <- data[[weight.by.name]]
      }
    }
  } else {
    var.name.row <- NULL
    var.name.col <- NULL
  }

  tab_core(
    data = NULL,
    var.row = var.row,
    var.col = var.col,
    weight.by = weight.by,
    title = title,
    var.labels = var.labels,
    value.labels = value.labels,
    wrap.labels = wrap.labels,
    show.obs = show.obs,
    cell.p = cell.p,
    row.p = row.p,
    col.p = col.p,
    show.exp = show.exp,
    show.legend = show.legend,
    show.na = show.na,
    show.summary = show.summary,
    drop.empty = drop.empty,
    statistics = statistics,
    string.total = string.total,
    digits = digits,
    tdcol.n = tdcol.n,
    tdcol.expected = tdcol.expected,
    tdcol.cell = tdcol.cell,
    tdcol.row = tdcol.row,
    tdcol.col = tdcol.col,
    emph.total = emph.total,
    emph.color = emph.color,
    prc.sign = prc.sign,
    hundret = hundret,
    CSS = CSS,
    encoding = encoding,
    file = file,
    use.viewer = use.viewer,
    remove.spaces = remove.spaces,
    var.name.row = var.name.row,
    var.name.col = var.name.col,
    ...
  )
}


#' @rdname tabc
#' @export
tabcc <- tabc
