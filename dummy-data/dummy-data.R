# Generate Dummy Dataset

# set seed for reproducible results
set.seed(01640731)

# number of observations is 600
# generate accession names
d_key <- rep(
  c("AA", "BB", "ABC", "DC", "EA"),
  c(100, 130, 200, 10, 160)
)
d_val <- sample(1:200000, 600, replace = FALSE)
d_acc <- paste0(d_key, "_", d_val, "_")
dummy <- data.frame(d_acc)

# define number of ancestry columns
# and give different weights to each one
n_anc <- 5
for (n in 1:n_anc) {
  anc <- paste0("anc", n)
  pct <- runif(600, 0, +n / n_anc)
  dummy[anc] <- pct
}

# create a vector with the sum of all ancestry columns
# excluding first column (which is the accession)
d_sums <- rowSums(dummy[, -1])

# normalize ancestry columns so they add up to 1
for (n in 1:n_anc) {
  anc <- paste0("anc", n)
  dummy[anc] <- dummy[anc] / d_sums
}

# generate vector with six country codes
country_codes <- c("GBR", "FRA", "ESP", "ITA", "DEU", "NLD")
d_country <- rep(
  country_codes,
  c(100, 130, 160, 50, 100, 60)
)

# randomly assign country codes to dummy data observations
dummy$country <- sample(d_country)

#-------------------------------------------------------------------------------

# Test dummy dataset
admix_barplot(dummy, 
              K = 2:6, 
              grouping = "country", 
              names = FALSE, 
              sortkey = "anc1", 
              palette = "turbo")

admix_subset(dummy, 
             country = c("GBR", "FRA"), 
             anc = c("anc1", "anc5"), 
             pct = c(0.1, 0.1)) %>%
  admix_barplot(K = 2:6, 
                sortkey = "anc5", 
                palette = "viridis", 
                grouping = "country")