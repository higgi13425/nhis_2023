# function
# to extract string portion from NN_StringValueLabel
parse_string <- function(string) {
  str_extract(string = string, 
              pattern = "(?<=\\d\\_).+")
}
