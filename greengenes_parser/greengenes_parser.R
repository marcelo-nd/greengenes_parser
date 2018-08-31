green_genes_parser <- function(string) {
    current_taxonomy = NULL
    string <- rev(strsplit(string, ";")[[1]])
    if (length(string) == 7 && length(strsplit(string[1], "__")[[1]]) == 2) {
        current_taxonomy <- sprintf("%s %s", strsplit(string[2], "__")[[1]][2], strsplit(string[1], "__")[[1]][2])
    }
    else {
        repeat {
            tryCatch({
                current_taxonomy <- sprintf("%s", strsplit(string[1], "__")[[1]][2])
                if ((current_taxonomy != "NA") && !is.na(current_taxonomy) && !is.null(current_taxonomy)) {
                    break
                }
                string <- string[-1]
            }
                   )
        }
    }
    return(current_taxonomy)

}

### TESTS ###
#current_string <- "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus;s__buchneri"
#green_genes_parser(current_string)

#current_string2 <- "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__Lactobacillus;s__"
#green_genes_parser(current_string2)

#current_string3 <- "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae;g__;s__"
#green_genes_parser(current_string3)

#current_string4 <- "k__Bacteria;p__Firmicutes;c__Bacilli;o__Lactobacillales;f__Lactobacillaceae"
#green_genes_parser(current_string4)