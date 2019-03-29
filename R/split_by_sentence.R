#function adapted from original written by Nick Wallen
#https://github.com/nickwallen

split.by.sentence <- function (text) {
  
  # split based on periods, exclams or question marks
  result <- unlist (strsplit (text, split = "[\\.!?]+"))
  
  # do not return empty strings
  result <- str_trim (result, "both")
  result <- result [nchar (result) > 0]
  
  # ensure that something is always returned
  if (length (result) == 0)
    result <- ""
  
  return (result)
}
