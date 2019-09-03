# uglypaRse

"thisismywebsite"
"imnotsurehowIgothere"

uglypaRse was created to solve a data quality problem. Real-world text data often contain missing delimiters, such as the spaces missing in the above examples. Making inferences about the location of such missing delimiters can be computationally intensive. uglypaRse implements an efficient algorithm in c++ (using Rcpp) to predict the location of missing delimiters. It also contains functions to aid the user in building domain-specific training corpora, to account for jargon.

uglypaRse is still in development, but has been functional for our group. A refined version will be published to CRAN in the future. Please feel free to email if we can help you make use of it.
