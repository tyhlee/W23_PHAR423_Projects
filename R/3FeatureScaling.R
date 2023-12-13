# Define the normalization function change to sd stnadardization
#normalize01 <- function(x) {
 # return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
#}

normalize <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  return((x - mean_x) / sd_x)
}


# normalized pre-AKIcreatVar
df$norm.preAKIcreatVar <- normalize(df$preAKIcreatVar)


# normalized proportional change
df$norm.proportionalChange <- normalize(df$proportionalChange)


# normalized absolute change
df$norm.absoluteChange <- normalize(df$absoluteChange)
