#create a function to indicate whether a word is in the dictionary
#this function has the option of accepting multiple dictionaries
#in the below implementation, it takes the Grady Augmented dictionary, plus a manually edited file that adds jargon words
#' Find words in one or more dictionaries
#'
#' @param keyword a character vector of words to be analyzed
#' @param dict1 a character vector of words to form a dictionary -- defaults to the Grady Augmented dictionary from package `qdapDictionaries`
#' @param dict2 an optional additional dictionary
#'
#' @return a vector of booleans
#' @export
is.word  <- function(keyword, dict1 = GradyAugmented, dict2 = NULL) {
  if(class(dict2) != "character" & (class(dict2) != "NULL")) stop("Additional dictionaries must be formatted as character vectors. Stopping.")
  a <- keyword %in% dict1 | keyword %in% dict2
  return(a)
} # create a vector of booleans based on this function

#######################################################################################
## findSimpleSplits = function to return a vector of strings that are not words from a vector of strings
## input = vector of words, alternative dictionary
#######################################################################################

#' Complete splitting process
#'
#' @param wordvec a character vector of words to split
#' @param alt_dict a character vector of user-added words
#'
#' @return a data frame containing the simplest way to split each strirng
#' @export
#'
findSimpleSplits <-  function(wordvec, alt_dict) {
  # input a check to be sure that this is a vector of strings
  
  worddf <- data.frame()
  worddf$words <- wordvec
  # check if the vector of strings is in GradyAugmented or alt_dict and assign Boolean value to second column called dict
  worddf$inDict <- is.word(worddf$words, dict2 = alt_dict)
  # take every observation that returns Boolean = F
  toSplit <- worddf[worddf$inDict==FALSE,]
  # append your alt_dict to GradyAugmented and retain only unique values
  dictionary <- unique(c(GradyAugmented, alt_dict))
  
  ########################################################################################################
    # run the function
    split <- as.matrix(sapply(toSplit$words, wordBreakD, dictionary))
    # convert to a dataframe # the output is weird from .cpp, and as such the rownames need to be added as a column vector; # switch the columns; # rename them
    df_split <-  as.data.frame(split); df_split$words <-  row.names(split); df_split <- df_split[,c(2,1)]; names(df_split) <- c("words", "potsplits")    
    
    # this will unnest the listed potential splits, group and aggregate multiple splits, then build a dataframe with each potential split having its own column
    df_split <- df_split %>% unnest(potsplits) %>% group_by(words) %>% summarise(potsplits = paste(potsplits, collapse = ", ")) %>% cSplit(splitCols='potsplits', sep=", ", direction = 'wide')
    
    # now want to choose which of the splits we want (I am using the shortest length based upon word count)
    simplesplits <- df_split %>%
      gather(key, value, -words) %>% # long format, every instance of a word and a potential split; if a collapsed string has multiple ways of splitting, that collapsed string will occupy multiple rows
      group_by(words) %>%
      mutate(n_words = str_count(value,'\\w+')) %>%  # count the words
      slice(which.min(n_words)) %>% # choose string split with the fewest words # grouping allows this slice to take the smallest for anygiven word
      select(words, value) %>% rename(orig = words, split = value) # renaming for my other functions, can name whatever
    
  return(simplesplits)
  
}