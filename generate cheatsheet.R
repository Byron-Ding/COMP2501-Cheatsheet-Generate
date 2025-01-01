# install.packages("rvest")
# library(rvest)
# 定义一个函数来检测并安装包
check_and_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 使用这个函数来检测并安装所需的包
check_and_install("xml2")


# help_html_path_base <- "/usr/lib/R/library/base/html/00Index.html"
# help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/ggplot2/html/00Index.html"
# help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/dplyr/html/00Index.html"
# help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/data.table/html/00Index.html"
# help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/janitor/html/00Index.html"
# help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/lubridate/html/00Index.html"
help_html_path_base <- "/home/byron/R/x86_64-pc-linux-gnu-library/4.4/tidyr/html/00Index.html"
# library("janitor")

help_html_base <- xml2::read_html(help_html_path_base)
# 提取每一行
func_pairs_tab <- xml2::xml_find_all(help_html_base, xpath = "//tr/td")
func_pairs_text <- xml2::xml_text(func_pairs_tab)

# nrows 不适用于 vector
# 要用length
func_pairs_nrow <- length(func_pairs_text)

odd_row_index_list <- base::seq(1, func_pairs_nrow, 2)
even_row_index_list <- base::seq(2, func_pairs_nrow, 2)

# 配对
odd_row_text <- func_pairs_text[odd_row_index_list]
even_row_text <- func_pairs_text[even_row_index_list]

# 创建合并 tibble 
# 默认按照列合并
base_func_with_description <- tibble::tibble(func_name = odd_row_text,
                                             description = even_row_text)
base_func_with_description <- base_func_with_description[-1, ]

# 清除特殊字符以及特殊表达式
base_func_with_description <-
  dplyr::filter(base_func_with_description,
                !stringr::str_detect(func_name,
                                    r"--(^[^A-Za-z]+.*$)--")
                )

# :const
control_flow_statements <- list("if" = "if (condition) {}",
                                "for" = "for (variable in vector) {}",
                                "while" = "while (condition) {}",
                                "repeat" = "repeat {if(){break}}",
                                "next" = "next == continue",
                                "break" = "break == break",
                                "function" = "fn <- function(){}",
                                "return" = "return()"
                                )

# 转closure 为列表， 获取所有参数
get_parameter_and_type <- function(func_name){
  # 先看看函数是否存在
  if (! exists(func_name, mode = "function")){
    return("")
  }else if(func_name %in% names(control_flow_statements)){
    # ↑ names 获取表头
    return(control_flow_statements[func_name])
  }
  
  whether_special <- stringr::str_detect(func_name, r"----(^[^A-Za-z_]+$)----")
  
  if (whether_special){
    return("")
  }
  
  
  # get args |> formals
  # :closure
  args_closure <- args(func_name) |> formals()
  # str 的输出很好，直接拿来用
  args_closure_str_output <- capture.output(str(args_closure))
  # 移除第一行的"Dotted pair list of *"
  args_closure_str_output <- args_closure_str_output[-1]
  
  # 拼接到一起
  args_closure_str_output <- stringr::str_c(args_closure_str_output,
                                            collapse = ",")
  
  # 移除首尾空格+替换内部连续空格为单空格
  args_together <- stringr::str_squish(args_closure_str_output)
  
  return(args_together)
}


# 遍历加列
base_func_with_description_args <-
  dplyr::rowwise(base_func_with_description) |>
  dplyr::mutate(
  args = list(get_parameter_and_type(func_name))
  )

# 拼合+额外保存
base_func_with_description_args_char_vector <-
  dplyr::rowwise(base_func_with_description_args) |> 
  dplyr::transmute(functions = paste0(func_name,
                           "(",
                           args,
                           ")\t"
                           ),
                   description = description
                   )

# a-z 排序
base_func_with_description_args_char_vector <-
  dplyr::arrange(base_func_with_description_args_char_vector,
                 functions)

# 按照200行分
split_data <- function(data, n) {
  # rep 是生成 1 - 组数 的序列
  # each = n 扩展为 1111111....., 2........, n....... 
  # length.out 负责截断
  split(data,
        rep(1:ceiling(nrow(data) / n), 
            each = n, 
            length.out = nrow(data)
            )
        )
}

check_and_install("gt")

data_n_row_ed <- split_data(base_func_with_description_args_char_vector,
                            100)

save_gt_pic <- function(data, save_name){
  # 记得规范为字符串
  save_name <- as.character(save_name)
  
  gt_table <- gt(data)
  # 字体
  gt_table <- gt::tab_options(gt_table,
                              table.font.names = "得意黑",
                              table.font.size = "20pt",
                              data_row.padding = px(0), # 设置数据行内边距
                              row.striping.include_table_body = TRUE, # 去除表格行条纹
                              table.border.top.width = 0,
                              table.border.bottom.width = 0,
                              row_group.border.top.width = 0,
                              row_group.border.bottom.width = 0,
                              # row_group.as_column = TRUE,
                              table.width = 825,
                              row_group.padding = 0,
                              # ow_group.as_column = TRUE
                              )

  
  # 分组
  for (LETTER in rev(LETTERS)){
    # regex ignore case
    regex_ignore_case <- stringr::str_c("(?i:", LETTER,")")
    
    start_result <-
      stringr::str_starts(data$functions,
                          regex_ignore_case)
    
    gt_table <- tab_row_group(gt_table,
                              label = gt::md(paste0("**", LETTER, "**")),
                              rows = start_result,
                              )
  }
  # + 分组非普通字符
  # _
  start_result <-
    stringr::str_starts(data$functions,
                        "_")
  
  gt_table <- tab_row_group(gt_table,
                            label = gt::md("**_**"),
                            rows = start_result)
  # @#$%^&*() ......
  start_result <-
    stringr::str_starts(data$functions,
                        R"---([^A-Za-z_])---")
  
  gt_table <- tab_row_group(gt_table,
                            label = gt::md(r"---(**[^A-Za-z_]**)---"),
                            rows = start_result,
                            )
  
  
  
  # 显示 gt 表格
  gt_table
  
  # 检查目录是否存在
  if (!dir.exists("/home/byron/Desktop")) {
    stop("路径不存在")
  }
  
  # 将 gt 表格保存为图片
  gt::gtsave(gt_table,
             paste0("/home/byron/桌面/", save_name, ".png"),
             expand = 10
             ) 
  
  # 打印成功信息 
  cat(paste0("gt 表格已成功保存为图片 ", save_name, ".png\n"))
}

# 一键保存
Map(save_gt_pic, data_n_row_ed, save_name = 1:length(data_n_row_ed))

# save_gt_pic(base_func_with_description_args_char_vector, 100)


