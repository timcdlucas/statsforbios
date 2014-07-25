# A function to turn messy object names into nice readable text.

humanize <- function(x){
                return(strHumanize(deparse(substitute(x))))
                }

strHumanize <- function(x){
                y <- gsub("_id", "", x)
                y <- gsub("\\.", " ", y)
                y <- gsub("_", " ", y)
                p <- gregexpr("[[:lower:]][[:upper:]]", y)
                y <- paste(read.fwf(textConnection(y), c(p[[1]][1], diff(p[[1]]),nchar(y)), as.is = TRUE), collapse = " ")              
                y <- tolower(y)  
                y <- gsub("(^)([[:alpha:]])", "\\1\\U\\2", y, perl=TRUE)
                return(y)
}



