# Internal helper to proxy to sjPlot::tab_xtab with ldtool defaults.
# Not exported.
tab_xtab_wrapper <- function(data,
                             var.row,
                             var.col,
                             weight.by = NULL,
                             title = NULL,
                             var.labels = NULL,
                             value.labels = NULL,
                             wrap.labels = 20,
                             show.obs = TRUE,
                             cell.p = FALSE,
                             row.p = FALSE,
                             col.p = FALSE,
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

  row_levels <- unique(stats::na.omit(var.row))
  col_levels <- unique(stats::na.omit(var.col))
  if (length(row_levels) <= 1 || length(col_levels) <= 1) {
    return(tab_core(
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
    ))
  }

  xtab <- create.xtab.df(
    var.row,
    var.col,
    round.prz = digits,
    na.rm = !show.na,
    weight.by = weight.by
  )
  if (ncol(xtab$mydat) <= 2) {
    return(tab_core(
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
    ))
  }

  sjPlot::tab_xtab(
    var.row = var.row,
    var.col = var.col,
    weight.by = weight.by,
    title = title,
    var.labels = var.labels,
    value.labels = value.labels,
    wrap.labels = wrap.labels,
    show.obs = show.obs,
    show.cell.prc = cell.p,
    show.row.prc = row.p,
    show.col.prc = col.p,
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
    ...
  )
}
