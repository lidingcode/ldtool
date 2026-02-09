
##################################################
# 在数据分析中常用的一些包和函数
##################################################
# stata命令可能是目前统计软件中最简洁的
# R功能相当强大，但输出简单，频数表和列联表等常见输出实现起来代码量非常大
# 这非常不友好
# 统计了自己最近撰写的几千行stata命令，发现下面的主命令使用最频繁：
# tab tabout reg
# replace rename label clonevar recode gen drop destring
# foreach local ci
# 其中gen keep drop replace merge等用data.table或dplyr就好
# reg也很简单
# 但recode、label在R中不算很友好
# 常见recode函数比较
# car::recode
# fct_recode
# expss::recode 类似spss，功能复杂 ~  %thru% 按不够简洁
# sjmisc::rec  沿袭spss风格，比较简洁，最好，可以数字变因子
# sjlabelled 包中关于变量标签和取值标签的函数
#* set_label   # var_labels
#* get_labels
#* set_labels  # val_labels

# 最常用tab，在R中要实现同样的输出，代码量大，最不好用
# 在stata中 tab意味着 tab1 tab2 tabstat tabout都是很分类汇总相关的命令
# 是频次表、列联表、分类汇总的核心
# tab y;tab y x ,(row,col,cell);table a b,c(mean c);weight;by

# 比较了几个常见的R包中频次表、连列表、分类分总函数，试图找到最简洁的
# 初步发现expss包做频次表和列联表最方便，分类汇总表格可使用tidyverse来做
# 未来可以尝试开发与stata的tab命令媲美的R包，整合这几个函数
# 使用最少参数做出最常用表格，提升R代码制作常见表格的效率

# -------------------------------------------
#   分类汇总描述性统计包
#   程序包    / 简洁度 /效率与结果  / 稳健性 /   加权  /
# ------------/--------/------------/----------------- / --------/
# base        / 简洁   /高 + 基础   /  很好  /   无     / table xtabs ftable
# tidyverse   / 适中   /高 + 基础   /   很好 /    wt   /count summarise
# crosstable  / 适中   /高 + 表好   /  一般  /    无   /crosstable 1-2-3
# gtsummary   / 复杂   /高 + 表好   /  很好  /         /tbl_summary  tbl_cross
# janitor     / 简洁   /不知 + 好   /  很好  /    无   / tabyl 1-2-3
#**expss      / 简洁   /高 + 表好   /  一般  / weight  /fre cro_cases cro_cpct
#* sjPlot     / 简洁   /高 + 图好   /  较好  / weights /frq(1) sjtab(2)
#summarytools / 适中   /较好        /  较好  / weights /freq ctable descr
##################################################


# 加载包
library(tidyverse)  # dplyr 管道函数
library(magrittr)   # for %>%  %$%
library(data.table)
library(janitor)    # 清理变量名 clean_names 得到方便R使用的变量名
library(sjmisc)     # for rec 因子变量重新编码，可以将数字变量编码为分类变量
library(sjlabelled)
library(sjstats)
library(sjPlot)
library(MASS)
library(DT)          # 用于在Viewer里面展示表格 暂时不用
library(expss)       # https://github.com/gdemin/expss
library(summarytools)# 描述性分析包
library(ggplot2)
library(dplyr)
library(gt)
library(rlang)
library(htmltools)  # 用于组合多个 HTML 元素


get_var_label <- function(data, var) {
  lab <- attr(data[[var]], "label")
  if (is.null(lab) || is.na(lab) || lab == "") {
    return("")
  }
  as.character(lab)
}

# tab1函数
#' @title 自定义频次表.
#' @name tab1
#' @description 自己定义的频次表.
#' @export
tab1 <- function(data, var, title = NULL) {
  var <- enquo(var)
  var_name <- rlang::as_name(var)
  var_label <- get_var_label(data, var_name)

  df <- data %>%
    dplyr::filter(!is.na(!!var)) %>%
    count(!!var, name = "Frequency") %>%
    mutate(
      Percent = Frequency / sum(Frequency) * 100,
      CumPercent = cumsum(Percent),
    )
  
  N <- sum(df$Frequency)

  header_title <- title %||%
    paste0(var_name,
           if (var_label != "") paste0("：", var_label) else "")
  df %>%
    gt() %>%
    tab_header(
      title = header_title) %>%
    tab_source_note(source_note = paste0("N = ", N)) %>% # 统计量输出在表格底部
    fmt_number(
      columns = c(Percent, CumPercent),
      decimals = 1
    ) %>%
    cols_label(
      !!colnames(df)[1] := "Value",
      Frequency = "Freq.",
      Percent = "Percent",
      CumPercent = "Cum."
    )
}

# 自定义函数名，少写一些字
#' @title 批量做频次分析.
#' @name tab_1
#' @description 对多个单变量进行频次分析。
#' @export
tab_1 <- function(data,..., title_prefix = NULL) {
  # vars 可以是列名向量或者 tidyselect 语法
  vars <- tidyselect::eval_select(expr(c(...)), data)
  var_names <- names(vars)
  
 # vars_selected <- tidyselect::eval_select(enquo(vars), data)
 #  var_names <- names(vars_selected)

  # 循环生成每个 gt 表
  gt_list <- lapply(var_names, function(v) {
    title <- if (!is.null(title_prefix)) paste0(title_prefix, v) else NULL
    tab1(data, !!sym(v), title = title)
  })

  # 直接组合成 HTML 可渲染对象
  res <- htmltools::tagList(gt_list)
  return(htmltools::browsable(res))
}

# 自定义函数名，少写一些字
#' @title 多选题分析函数.
#' @name mrtab1
#' @description 多选题分析自定义函数。
#' @export
mrtab <- function(df, ..., select_value = 1, title_prefix = "多选题分析") {
  # 使用 tidyselect 语法选择变量
  vars <- tidyselect::eval_select(expr(c(...)), df)
  var_names <- names(vars)
  # 提取数据块
  block <- df[var_names]
  # 有效样本：至少一列非缺失
  #valid <- apply(block, 1, function(x) any(!is.na(x)))
  valid <- apply(block, 1, function(x) {
  sum(x == select_value, na.rm = TRUE) > 0
})
  base_n <- sum(valid)
  # 获取标签
  labels <- sapply(block, var_label)
  # 计算频数（修正：确保取向量，labelled 转数值）
  freq <- sapply(var_names, function(v) {
    vec <- df[[v]][valid]
    if (inherits(vec, "labelled")) vec <- as_numeric(vec)
    sum(vec == select_value, na.rm = TRUE)
  })
  
  # 百分比（占有效样本）
  pct_case <- freq / base_n * 100
  
  # 响应百分比（占总回应次数 = 所有选中次数之和）
  total_responses <- sum(freq)
  pct_resp <- freq / total_responses * 100
  
  # 构建表格数据框
  df_tab <- tibble(
    Value = labels,
    Freq. = freq,
    Pct_resp = round(pct_resp, 1),
    Pct_case = round(pct_case, 1)
  )
  
  # 标题自动显示变量名范围 + 可选前缀
  header_title <- paste0(
    if (!is.null(title_prefix)) paste0(title_prefix, " ") else "",
    var_names[1], " - ", var_names[length(var_names)]
  )
  
  # 输出 gt 表
  df_tab %>%
    gt() %>%
    tab_header(
      title = header_title,
      subtitle = paste0("N = ", base_n,"  Rs = ", total_responses)
    ) %>%
    fmt_number(
      columns = c("Pct_resp","Pct_case"),
      decimals = 1
    )
}


# 自定义函数名，少写一些字
#' @title recode variable using rec in sjmisc.
#' @name rec
#' @description 用来进行变量的重编码.映射规则比较简单。
#' @export
rec  <-  sjmisc::rec

# 自定义tab 函数
#tabc / tabr / tabt are thin semantic wrappers around sjPlot::sjtab.
#They do not modify evaluation rules and only set default display options


#' @title 默认的列联表.
#' @name tab
#' @description 多选题分析自定义函数。
#' @export
tab  <-  sjPlot::sjtab

#' @title 默认的列百分比列联表.
#' @name tabc
#' @description 基于sjPlot函数默认输出列百分比。
#' @export
tabc <- function(data, ..., show.col.prc = TRUE) {
  # 1. 捕获当前 tabc 的完整调用命令
  m <- match.call(expand.dots = TRUE)
  
  # 2. 将函数名从 tabc 替换为 sjPlot::sjtab
  # 这样进入 sjtab 内部时，match.call() 就能拿到正确的变量符号
  m[[1]] <- quote(sjPlot::sjtab)
  
  # 3. 显式注入默认参数 show.col.prc = TRUE
  # 如果用户在调用 tabc 时已经写了 show.col.prc，则不覆盖用户的选择
  if (!"show.col.prc" %in% names(m)) {
    m[["show.col.prc"]] <- show.col.prc
  }

  # 4. 在父环境中执行修改后的调用
  eval(m, parent.frame())
}

#' @title 默认的行百分比列联表.
#' @name tabr
#' @description 基于sjPlot函数默认输出行百分比.
#' @export
tabr <- function(data, ..., show.row.prc = TRUE) {
  # 1. 捕获当前 tabc 的完整调用命令
  m <- match.call(expand.dots = TRUE)
  
  # 2. 将函数名从 tabc 替换为 sjPlot::sjtab
  # 这样进入 sjtab 内部时，match.call() 就能拿到正确的变量符号
  m[[1]] <- quote(sjPlot::sjtab)
  
  # 3. 显式注入默认参数 show.col.prc = TRUE
  # 如果用户在调用 tabc 时已经写了 show.col.prc，则不覆盖用户的选择
  if (!"show.row.prc" %in% names(m)) {
    m[["show.row.prc"]] <- show.row.prc
  }
  
  # 4. 在父环境中执行修改后的调用
  eval(m, parent.frame())
}

#' @title 默认的单元格百分比列联表.
#' @name tabt
#' @description 基于sjPlot函数默认输出单元格百分比.
#' @export
tabt <- function(data, ..., show.cell.prc = TRUE) {
  # 1. 捕获当前 tabc 的完整调用命令
  m <- match.call(expand.dots = TRUE)
  
  # 2. 将函数名从 tabc 替换为 sjPlot::sjtab
  # 这样进入 sjtab 内部时，match.call() 就能拿到正确的变量符号
  m[[1]] <- quote(sjPlot::sjtab)
  
  # 3. 显式注入默认参数 show.col.prc = TRUE
  # 如果用户在调用 tabc 时已经写了 show.col.prc，则不覆盖用户的选择
  if (!"show.cell.prc" %in% names(m)) {
    m[["show.cell.prc"]] <- show.cell.prc
  }
  
  # 4. 在父环境中执行修改后的调用
  eval(m, parent.frame())
}

#' @title 输出带有边缘频数的列联表.
#' @name tabn
#' @description 支持管道操作和 group_by 分组，自动拼接多个表格到 Viewer。
#' @export
tabn <- function(data, row_var, col_var, type = "col") {
  
  # 1. 立即捕获变量名，防止在 group_map 闭包中丢失环境
  row_v <- rlang::enquo(row_var)
  col_v <- rlang::enquo(col_var)
  
  if (rlang::quo_is_missing(row_v) || rlang::quo_is_missing(col_v)) {
    stop("错误：必须指定 row_var 和 col_var")
  }

  row_name <- rlang::as_label(row_v)
  col_name <- rlang::as_label(col_v)

  # 2. 核心处理函数：生成单个 gt 对象
  # 将你原始的详细计算逻辑完整放入此处
  make_tab <- function(df, group_label = NULL) {
    
    # --- 基础频数提取 ---
    t_raw <- janitor::tabyl(df, !!row_v, !!col_v, show_na = FALSE, show_missing_levels = FALSE)
    row_labels <- as.character(t_raw[[1]])
    mat <- as.matrix(t_raw[,-1])
    
    # --- 统计量计算 ---
    chi_test <- stats::chisq.test(mat)
    # 计算 Cramer's V (手动实现，避免依赖外部包)
    n_sum <- sum(mat)
    phi2 <- chi_test$statistic / n_sum
    r_idx <- nrow(mat)
    k_idx <- ncol(mat)
    cramer_v <- as.numeric(sqrt(phi2 / min(r_idx - 1, k_idx - 1)))
    # 构造统计量字符串
    # 按照你的要求：χ2, df, Cramer's V, p
    # 构造统计量字符串（准备放在表格下方）
    chi_stats <- sprintf("Pearson χ²(%d) = %.4f, Cramer's V = %.3f, Pr = %.3f", 
                         chi_test$parameter, chi_test$statistic, cramer_v, chi_test$p.value)
    # --- 核心百分比逻辑（保留你原始定义的完整格式化） ---
    if (type == "row") {
      # 行百分比逻辑
      row_ns <- rowSums(mat)
      total_n <- sum(mat)
      pct_mat <- mat / row_ns * 100
      total_row_pct <- colSums(mat) / total_n * 100
      full_pct_mat <- rbind(pct_mat, total_row_pct)
      row_totals <- rowSums(full_pct_mat)
      
      res_mat <- cbind(full_pct_mat, Total = row_totals)
      res_mat <- round(res_mat, 1)
      res_mat <- format(res_mat, nsmall = 1)
      res_mat <- cbind(res_mat, `N` = as.character(c(row_ns, total_n)))
      
      res_df <- data.frame(Label = c(row_labels, "Total"), res_mat, 
                           check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      # 列百分比逻辑
      col_ns <- colSums(mat)
      total_n <- sum(mat)
      pct_mat <- sweep(mat, 2, col_ns, "/") * 100
      total_col_pct <- rowSums(mat) / total_n * 100
      full_pct_mat <- cbind(pct_mat, Total = total_col_pct)
      col_totals <- colSums(full_pct_mat)
      
      res_mat <- round(full_pct_mat, 1)
      res_mat <- format(res_mat, nsmall = 1)
      final_mat <- rbind(res_mat, 
                         Total_Pct = format(col_totals, nsmall = 1),
                         `Sample Size (N)` = as.character(c(col_ns, total_n)))
      
      res_df <- data.frame(Label = c(row_labels, "Total_Pct", "N"), 
                           final_mat, check.names = FALSE, stringsAsFactors = FALSE)
    }

    # --- 变量标签与标题处理 ---
    # 使用原始 data 获取 label 属性（分组后的 df 属性可能丢失）
    r_lab <- attr(data[[row_name]], "label") %||% row_name
    c_lab <- attr(data[[col_name]], "label") %||% col_name
    
    header_title <- paste0(r_lab, " × ", c_lab, " 交互分析")
    if (!is.null(group_label)) {
      header_title <- paste0("[分组: ", group_label, "] ", header_title)
    }

    # --- 返回 gt 对象 ---
    res_df %>% 
      gt::gt() %>%
      gt::tab_header(title = header_title) %>% # 这里没有 subtitle
      gt::tab_source_note(source_note = chi_stats) %>% # 统计量输出在表格底部
      gt::cols_label(Label = row_name)
  }

  # 3. 分组执行逻辑
  if (dplyr::is_grouped_df(data)) {
    # 使用 group_map 遍历每个分组
    # .keep = TRUE 确保 row_var 和 col_var 在子数据框中可见
    out_list <- data %>% 
      dplyr::group_map(~{
        # 提取当前分组的变量值作为标签
        grp_vals <- paste(names(.y), .y[1, ], sep = "=", collapse = ", ")
        make_tab(.x, group_label = grp_vals)
      }, .keep = TRUE)
    
    # 关键：使用 htmltools 将列表中的多个 gt 对象转换为原始 HTML 并拼接
    # 添加 htmltools::browsable 确保结果直接推送到 Viewer 而不是 Console
    res <- htmltools::tagList(lapply(out_list, gt::as_raw_html))
    return(htmltools::browsable(res))
    
  } else {
    # 非分组数据直接返回单表预览
    return(make_tab(data))
  }
}

# 辅助操作符
`%||%` <- function(a, b) if (!is.null(a) && a != "") a else b

#' @title make frequency table using sjmisc library.
#' @name freq
#' @description 用来制作频数表，使用sjmisc::frq函数
#' @export
freq  <-  sjmisc::frq

#' @title make frequency table using expss library
#' @name fre
#' @description 使用expss::fre制作频数表
#' @export
fre  <- expss::fre

st_options(ctable.prop = "n")
#tabn <- sjPlot::tab_xtab# expss::cro_cases # tab of cases n
#' @title make  count table using ctable function in summarytools library.
#' @name tabf
#' @description 使用summarytools::ctable制作简单的频数表
#' @export
tabf <- summarytools::ctable

#tabf <- expss::cro_cases

#' @export
group_by <- dplyr::group_by

#' @title make frequency graph using plot_frq in sjPlot.
#' @name gtab
#' @description 使用sjPlot::plot_frq制作频数条形图
#' @export
gtab <- sjPlot::plot_frq # 单变量条形图

# 设定默认参数后其他参数没法设定了

## 自定义一个查看数据基本信息（变量名、标签、类型）的函数

#' @title Describe the data and display the varible name,label,type and position information in dataview.
#' @name des
#' @description 创建一个新dataframe描述数据集的变量名、变量标签及格式信息。
#' @param dfile a dataframe.
#' @export
#'
des <- function (dfile) {
  lbl = sapply(dfile, attr, 'label')
  fmt = sapply(dfile, class)
  if (is.list(lbl)) {
    lbl[sapply(lbl, is.null)] = ''
    lbl[sapply(lbl, length) > 1] = ''
    lbl = unlist(lbl)
  }
  Encoding(lbl) = 'UTF-8'

  if (is.list(fmt)) {
    fmt[sapply(fmt, is.null)] = ''
    fmt[sapply(fmt, length) > 1] = 'double' # 加标签的都默认为数字型
    fmt = unlist(fmt)
  }

  dfile_var = data.frame(id=seq_len(ncol(dfile)),var =names(dfile), lbl = lbl,fmt=fmt)
  rownames(dfile_var) <- NULL
  View(dfile_var)
}

# 多选题 https://stackoverflow.com/questions/9265003/analysis-of-multiple-response
# 目前没法加权,交互分析也不行，如果能够复制stata的mrtab比较好
#' @title Frequence tabll of Multiple response variable .
#' @name mrtab1
#' @description 进行多选题分析，类似stata中的mrtab
#' @param data A dataframe.
#' @param question.prefix  The common strings of that series of multiple response vaiables
#' @export
#'
mrtab1= function(data, question.prefix) {
  z = length(question.prefix)
  temp = vector("list", z)

  for (i in 1:z) {
    a = grep(question.prefix[i], names(data))
    b = sum(data[, a] != 0)
    d = colSums(data[, a] != 0)
    e = sum(rowSums(data[,a]) !=0)
    f = as.numeric(c(d, b))
    temp[[i]] = data.frame(question = c(sub(question.prefix[i],
                                            "", names(d)), "Total"),
                           freq = f,
                           percent = (f/b)*100,
                           percentofcases = (f/e)*100 )
    names(temp)[i] = question.prefix[i]
  }
  temp
}

#注意r当中的round函数与stata中的round对于0.5的处理是有区别的
#R中将它归为最近的偶数（even）一侧。
#https://statisticsglobe.com/r-round-ceiling-floor-trunc-signif-function-example
#https://stackoverflow.com/questions/12688717/round-up-from-5
#https://datacornering.com/round-roundup-rounddown-trunc-in-r/
#四舍五入，向上取整，与stata保持一致

#' @title Round 0.5 up.
#' @name  round2
#' @description 常见的四舍五入而不是根据舍位的奇数偶数决定
#' @param x A vector
#' @param n Number of decimals you want to keep.
#' @export
#'
round2 = function(x, n) {  # Function to always round 0.5 up
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# 设置expss列联表的输出窗口，更短的函数
#' @title table output in viewer.
#' @name  frev
#' @description  设定fre函数结果在viewer输出
#'
library(expss)
#' @export
frev <-function ()
{
  options(expss.output = "viewer")
}
#' @title table output in txt.
#' @name  fret
#' @description  设定fre函数结果在viewer输出
#' @export

fret<- function ()
{
  options(expss.output = "default")
}

