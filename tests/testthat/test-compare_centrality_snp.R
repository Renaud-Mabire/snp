library(testthat)
library(snp)

# simulated data
simulations_example <- list(
  simulations = list(
    list(Fit_sample = list(weiadj = matrix(0, 5, 5))),
    list(Fit_sample = list(weiadj = matrix(1, 5, 5)))
  )
)

# Testez que la fonction retourne un objet ggplot
test_that("compare_centrality_snp retourne un objet ggplot", {
  plot_output <- compare_centrality_snp(simulations_example)
  expect_is(plot_output, "ggplot")
})

# Ajoutez d'autres tests spécifiques pour la fonction compare_centrality_snp en fonction de vos besoins
