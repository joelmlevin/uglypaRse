# uglypaRse

"thisismywebsite"
"imnotsurehowIgothere"

uglypaRse was created to solve a data quality problem. Real-world text data often contains missing delimiters, such as the spaces missing in the above examples. Making inferences about the most-likely placement of such missing delimiters can be very computationally-intensive. uglypaRse implements an efficient algorithm in c++ (using Rcpp) to predict the location of missing delimiters. It also contains functions to aid the user in building domain-specific training corpora, to account for jargon.

If you have issues with missing delimiters, we're happy to share the existing codebase and provide tips on how to use it. The package will be published to CRAN in time, but don't hold your breath. 
