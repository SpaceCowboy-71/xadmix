#' Simulated Admixture Data
#'
#' A dataset containing simulated admixture data of 600 observations. 
#' 
#' @format A data frame with 600 rows and 8 variables: 
#' \describe{
#'  \item{\code{acc}}{Accession identifier}
#'  \item{\code{country}}{Country where plant material was collected}
#'  \item{\code{species}}{Name of species}
#'  \item{\code{K1},\code{K2},\code{K3},\code{K4},\code{K5}}{Admixture coefficients; expresses the proportions of the respective ancestries. Sum up to 1.}
#' }
#' @source Data simulated for this package; for code see: \url{https://github.com/SpaceCowboy-71/xadmix/blob/main/data-raw/xadmixture.R}
#' @examples 
#' # load simulated admixture data
#' data("xadmixture")
#' 
#' # create a subset of the data
#' xadmixture_sub <- admix_subset(xadmixture, 
#'                        country = c("GBR", "FRA"),
#'                        anc = c("K1", "K2"), 
#'                        pct = c(0.02, 0.2))
#'                        
#' # generate a grouped & sorted stacked barplot
#' admix_barplot(xadmixture_sub, 
#'          K = 4:ncol(xadmixture),
#'          sortkey = "K1",
#'          grouping = "country", 
#'          palette = "turbo")   
"xadmixture"