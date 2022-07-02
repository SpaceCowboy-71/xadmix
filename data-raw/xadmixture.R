# Generate Dummy Dataset

library(usethis)
# set seed for reproducible results
set.seed(01640731)

# number of observations is 600
# generate semi-random accession names
key <- rep(
  c("AA", "BB", "ABC", "DC", "EA"),
  c(100, 130, 200, 10, 160)
)
val <- sample(1:200000, 600, replace = FALSE)
acc <- paste0(key, "_", val, "_")
xadmixture <- data.frame(acc)

# generate vector with six country codes and
# randomly assign country codes to dummy data observations
country_codes <- c("GBR", "FRA", "ESP", "ITA", "DEU", "NLD")
d_country <- rep(
  country_codes,
  c(100, 130, 160, 50, 100, 60)
)
xadmixture$country <- sample(d_country)

# generate vector with three species and
# randomly assign species to dummy data observations
species <- c("lorem", "ipsum", "dolor")
d_species <- rep(species, 200)
xadmixture$species <- sample(d_species)

# define number of ancestry columns (K),
# give different weights to each one and
# assign them to dummy data
n_anc <- 5
for (n in 1:n_anc) {
  anc <- paste0("K", n)
  pct <- runif(600, 0, +n / n_anc)
  xadmixture[anc] <- pct
}

# create a vector with the sum of all ancestry columns
# excluding first column (which is the accession)
# then normalize ancestry columns so they add up to 1
d_sums <- rowSums(xadmixture[, -1])
for (n in 1:n_anc) {
  anc <- paste0("anc", n)
  xadmixture[anc] <- xadmixture[anc] / d_sums
}

# save dummy data to data file
write.csv(xadmixture, "data-raw/xadmixture.csv", row.names = FALSE)
usethis::use_data(xadmixture, overwrite = TRUE)


#-------------------------------------------------------------------------------

# Test dummy dataset
admix_barplot(xadmixture, 
              K = 2:6, 
              grouping = "country", 
              names = FALSE, 
              sortkey = "anc1", 
              palette = "turbo")

admix_subset(xadmixture, 
             country = c("GBR", "FRA"), 
             anc = c("anc1", "anc5"), 
             pct = c(0.1, 0.1)) %>%
  admix_barplot(K = 2:6, 
                sortkey = "anc5", 
                palette = "viridis", 
                grouping = "country")

admix_subset(xadmixture, 
             species = c("lorem", "dolor"), 
             anc = c("anc2", "anc4"), 
             pct = c(0.1, 0.1)) %>%
  admix_barplot(K = 2:6, 
                sortkey = "anc2", 
                palette = "viridis", 
                grouping = "species")
