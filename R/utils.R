#' Return a string in quotes
#' 
#' @param str the string to enquote
#' 
#' @return 'str'
in_quotes <- function(str) sprintf("'%s'", str)

#' Get function args as string
#' 
#' @param args named vector of function arguments
#' 
#' @return arg_name = arg_value,
parse_args <- function(args) paste(names(args), "=", args, collapse = ", ")

#' Get list as a string
#' 
#' @param lst the list to get as a string
#' 
#' @return lst as a a string representing a vector
paste_list_as_vector <- function(lst) {
  
  if (length(names(lst))) {
    names(lst) <- in_quotes(names(lst))
    list_t <- lapply(lst, function(v) ifelse(is.character(v), in_quotes(v), v))
    paste0("c(", paste(names(list_t), "=", list_t, collapse = ", "), ")")
  } else {
    paste0("c(", paste(in_quotes(lst), collapse = ", "), ")")
  }
}