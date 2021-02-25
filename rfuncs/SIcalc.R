# Ezra's algorithm for SPI/SMRI ----
PQ_index_mutate <- function (sumq, agg) {
  library(SPEI)
  
  pq <- lapply(agg, function (x) {
    data.frame(index = as.numeric(SPEI::spi(sumq, x, distribution = "Gamma")$fitted, 3))} # the spi function requires a vector input
  )
  
  return(bind_rows(pq, .id = "agg") %>% as.tibble)
}

# SPEI algorithm----
PQspei_index_mutate <- function (q_pet, agg) {
  library(SPEI)
  
  pq <- lapply(agg, function (x) {
    data.frame(index = as.numeric(SPEI::spei(q_pet, x, # the spi function requires a vector input
                                             distribution = "log-Logistic")$fitted, 3))}
  )
  
  return(bind_rows(pq, .id = "agg") %>% as.tibble)
}