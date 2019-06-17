# uglypaRse

"thisismywebsite"
"imnotsurehowIgothere"

uglypaRse was created to solve a data quality problem. Real-world text data often contains missing delimiters, such as the spaces missing in the above examples. Making inferences about the location of such missing delimiters can be computationally-intensive. uglypaRse implements an efficient algorithm in c++ (using Rcpp) to predict the location of missing delimiters. It also contains functions to aid the user in building domain-specific training corpora, to account for jargon.

uglypaRse is still in development, but has been functional for our group. Please feel free to reach out if it could be of use to you. A refined version will be published to CRAN in the future.
