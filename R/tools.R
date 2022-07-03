#' Admixture Data Subsetting
#'
#' Subset function optimized for admixture data.
#' @param data Data frame containing the admixture data.
#' @param anc Vector of ancestry column names to use for pairwise subsetting with percentage vector. Must be of same length as the supplied percentage vector.
#' @param pct Vector of percentage values to use for pairwise subsetting with ancestry column name vector. Only ancestries with values above the percentage are kept.
#' @param comparison What comparison operator to use for the subsetting. Can either be "greater" or "less"; default is ">". Also accepts "gt", "lt", ">" and "<".
#' @param quiet Whether to print progress or not; default is "FALSE".
#' @param ... Variable number of additional vectors for subsetting. Looking at the column with argument name, keep only those observations with values which are elements of the argument vector.
#' @return A subset of the provided data frame.
#' @examples
#' # keep only observations with anc1 > 10 and anc2 > 1
#' subset1 <- admix_subset(data, anc = c("anc1", "anc2"), pct = c(10, 1))
#'
#' # keep only observations with values "GBR" or "FRA" in column "country"
#' subset2 <- admix_subset(data, country = c("GBR", "FRA"))
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
admix_subset <- function(data, anc = NULL, pct = NULL, comparison = "greater", quiet = FALSE, ...) {
    # ancestries <anc> and percentages <pct> can be vectors
    # they must be of the same length!
    # the first entries each form a pair, then the second ones...
    # /!\ IMPORTANT: columns -must- be called "country" and "species" for respective
    # subsetting to work!

    # Error handling - check for same length vectors
    if (length(anc) != length(pct)) stop("Ancestry and percentage vectors must be of same length!")

    asub <- data
    if (!quiet) {
        cat("observations:", nrow(asub), "\n")
    }

    args <- list(...) # get additional arguments
    arg_names <- names(args) # get argument names

    # use additional args for subsetting
    if (length(arg_names) > 0) {
        for (i in 1:length(arg_names)) {
            asub <- asub %>% filter(asub[[arg_names[i]]] %in% args[[i]])
            if (!quiet) {
                cat("keeping only specified values in col:", arg_names[i], "\n")
                cat("\tobservations left after this step:", nrow(asub), "\n")
            }
        }
    }

    # loop over all ancestry-percentage pairs,
    # only selecting those with percentage higher than cutoff
    # generating one subset
    if (hasArg(anc) && hasArg(pct)) {
        for (i in 1:length(anc)) {
            if (comparison == "gt" || comparison == "greater" || comparison == ">") {
                asub <- asub %>% filter(asub[[anc[i]]] > pct[i])
                compc <- ">"
            } else if (comparison == "lt" || comparison == "less" || comparison == "<") {
                asub <- asub %>% filter(asub[[anc[i]]] < pct[i])
                compc <- "<"
            } else {
                stop("comparison must be either 'gt' or 'lt'")
            }
            asub <- na.omit(asub)
            if (!quiet) {
                cat(i, ". subset, ", anc[i], " ", compc, " ", pct[i], "\n", sep = "")
                cat("\tobservations:", nrow(asub), "\n")
            }
        }
    }
    return(asub)
}
