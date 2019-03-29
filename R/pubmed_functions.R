#example of compiling a characteristic corpus from pubmed abstracts

#install.packages("easyPubMed")
#requires XML

#############

#Note that there are three functions in this document. All take the same arguments are return information from pubmed abstracts. 
#They differ in the format in which the information is returned. 

#############

#terms should take the format: "term1[TYPE] (AND/OR) term2[TYPE]"
#note that the '[TI]' denotes searching in the article title
#a full list of searchable fields can be found at: https://www.ncbi.nlm.nih.gov/books/NBK3827/#_pubmedhelp_Search_Field_Descriptions_and_
#for example: "term1[TI] AND term2[TI]" searches for titles that contain both terms
#we recommend only returning enlish results for training the parser


#' Obtain a volatile corpus from PubMed abstracts
#'
#' @param terms a series of PubMed search terms
#' @param language defaults to English. Changing this is not recommended because of typos introduced in translating papers from other languages.
#'
#' @return this function returns a `tm` volatile corpus
#' @export
#'
#' @examples
#' `corpus <- pubmed.corpus("influenza[TI] AND adult[TI] AND type[TI]")`
#' `corpus <- pubmed.corpus("uglypaRse[TI] AND levin[LASTAU]")` #searches for titles containing uglyparse with authors having the last name 'Levin'
#' @seealso 
#' The below link contains a list of search fields that can be used to query PubMed.
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Search_Field_Descriptions_and}
pubmed.corpus <- function(terms, language = "English") {
  lan_arg <- paste(language, "[LA]", sep = "") #create the language argument
  pubmed_search <- paste(terms, "AND", lan_arg) # combine the search terms with the language argument
  message("Downloading PubMed data.")
  pubmed_out <- batch_pubmed_download(pubmed_query_string = pubmed_search, batch_size = 1000000) #retreive 
  clean_out <- table.articles.mod(pubmed_out) 
  if(count(clean_out) < 1) stop("Your query did not return any results. Stopping.")
  clean_out <- clean_out %>% #creating tibble with one obs per sentence
    select(abstract) %>%
    distinct() %>%
    filter(!is.na(abstract)) %>% 
    apply(1, split.by.sentence) %>% #using user-defined function to split by sentence
    unlist() %>%
    as.tibble() %>%
    mutate(value = replace_html(value), #cleaning html
           value = gsub('([[:punct:]])|\\s+',' ', value), #cleaning special characters
           value = gsub('[[:digit:]]+', ' ', value), #removing numbers, replacing with space
           value = tolower(value), #lowercase
           value = gsub('\\b\\w{1,2}\\b','', value), #removing 1-2 letter words
           value = str_squish(value)) %>% #removing extra spaces
    transmute(doc_id = 1:n(),
             text = value)
    if(all(validUTF8(clean_out$text)) != TRUE) stop("Text must be encoded using UTF8. Stopping.")
  #now creating the corpus
    clean_out <- DataframeSource(clean_out) 
    the_corpus <- VCorpus(clean_out, readerControl = list(language = "en"))
    return(the_corpus)
}  

#' Obtain a data frame of abstracts from PubMed
#'
#' @param terms a series of PubMed search terms
#' @param language defaults to English. Changing this is not recommended if the output will be used in NLP because of typos introduced in translating papers from other languages.
#'
#' @return this function returns a data frame with each observation representing a paper
#' @export
#'
#' @examples
#' `df <- pubmed.abstracts.df("influenza[TI] AND adult[TI] AND type[TI]")`
#' `df <- pubmed.abstracts.df("uglypaRse[TI] AND levin[LASTAU]")` #searches for titles containing uglyparse with authors having the last name 'Levin'
#' @seealso 
#' The below link contains a list of search fields that can be used to query PubMed.
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Search_Field_Descriptions_and}
pubmed.abstracts.df <- function(terms, language = "English") {
  lan_arg <- paste(language, "[LA]", sep = "") #create the language argument
  pubmed_search <- paste(terms, "AND", lan_arg) # combine the search terms with the language argument
  message("Downloading PubMed data.")
  pubmed_out <- batch_pubmed_download(pubmed_query_string = pubmed_search, batch_size = 1000000) #retreive 
  clean_out <- table.articles.mod(pubmed_out) 
  if(count(clean_out) < 1) stop("Your query did not return any results. Stopping.")
  clean_out <- clean_out %>% #creating tibble with one obs per sentence
    select(abstract, title) %>% #note that I added the title back here, even though we don't want it for later steps
    distinct() %>%
    filter(!is.na(abstract)) %>%
    mutate(abstract = replace_html(abstract), #cleaning html
           #abstract = gsub('([[:punct:]])|\\s+',' ', abstract), #cleaning special characters
           abstract = gsub('[[:digit:]]+', ' ', abstract), #removing numbers, replacing with space
           abstract = tolower(abstract), #lowercase
           abstract = gsub('\\b\\w{1,2}\\b','', abstract), #removing 1-2 letter words
           abstract = str_squish(abstract), #removing extra spaces
           doc_id = 1:n()
           )
  return(clean_out)
}  

#' Obtain a data frame of abstracts from PubMed, parsed in to sentences
#'
#' @param terms a series of PubMed search terms
#' @param language defaults to English. Changing this is not recommended if the output will be used in NLP because of typos introduced in translating papers from other languages.
#'
#' @return this function returns a data frame with each observation representing a sentence from an abstract.
#' @export
#'
#' @examples
#' `df <- pubmed.abstracts.df("influenza[TI] AND adult[TI] AND type[TI]")`
#' `df <- pubmed.abstracts.df("uglypaRse[TI] AND levin[LASTAU]")` #searches for titles containing uglyparse with authors having the last name 'Levin'
#' @seealso 
#' The below link contains a list of search fields that can be used to query PubMed.
#' \url{https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Search_Field_Descriptions_and}
pubmed.abstract.sentences.df <- function(terms, language = "English") {
  lan_arg <- paste(language, "[LA]", sep = "") #create the language argument
  pubmed_search <- paste(terms, "AND", lan_arg) # combine the search terms with the language argument
  message("Downloading PubMed data.")
  pubmed_out <- batch_pubmed_download(pubmed_query_string = pubmed_search, batch_size = 1000000) #retreive
  clean_out <- table.articles.mod(pubmed_out) 
  if(count(clean_out) < 1) stop("Your query did not return any results. Stopping.")
  clean_out <- clean_out %>% #creating tibble with one obs per sentence
    select(abstract) %>%
    distinct() %>%
    filter(!is.na(abstract)) %>% 
    apply(1, split.by.sentence) %>% #using user-defined function to split by sentence
    unlist() %>%
    as.tibble() %>%
    mutate(value = replace_html(value), #cleaning html
           value = gsub('([[:punct:]])|\\s+',' ', value), #cleaning special characters
           value = gsub('[[:digit:]]+', ' ', value), #removing numbers, replacing with space
           value = tolower(value), #lowercase
           value = gsub('\\b\\w{1,2}\\b','', value), #removing 1-2 letter words
           value = str_squish(value)) %>% #removing extra spaces
    transmute(doc_id = 1:n(),
              text = value)
  return(clean_out)
}  

#this returns relatively few results because of the three conditions, so is good for testing
#test_corpus <- pubmed.corpus("influenza[TI] AND adult[TI] AND type[TI]")

#test_abstracts <- pubmed.abstracts.df("influenza[TI] AND adult[TI] AND type[TI]")

#test_sentences <- pubmed.abstract.sentences.df("influenza[TI] AND adult[TI] AND type[TI]")

