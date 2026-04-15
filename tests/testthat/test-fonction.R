test_that("stats_var calcule les statistiques correctement", {
  test_data <- data.frame(
    valeurs = c(10, 20, 30, 40, 50)
  )

  res <- stats_var(test_data, "valeurs")

  expect_s3_class(res, "data.frame")
  expect_equal(res$moyenne, 30)
  expect_equal(res$min, 10)
  expect_equal(res$max, 50)
  expect_equal(nrow(res), 1)
})




test_that("corr_vars renvoie les matrices de corrélation", {
  test_data <- data.frame(
    a = c(1, 2, 3, 4, 5),
    b = c(5, 4, 3, 2, 1)
  )

  res <- corr_vars(test_data, c("a", "b"))

  expect_type(res, "list")
  expect_true("pearson" %in% names(res))
  expect_true("spearman" %in% names(res))
  # La corrélation entre 'a' et 'b' ici est parfaitement négative (-1)
  expect_equal(as.numeric(res$pearson[1, 2]), -1)
})




test_that("anova_var effectue le calcul d'ANOVA", {
  test_data <- data.frame(
    score = c(10, 12, 11, 20, 22, 21),
    groupe = c("A", "A", "A", "B", "B", "B")
  )

  res <- anova_var(test_data, "score", "groupe")

  # L'objet retourné par summary(aov) est une liste de classe summary.aov
  expect_type(res, "list")
  expect_s3_class(res[[1]], "anova")
})




test_that("plot_quanti_quanti génère un graphique ggplot valide", {
  test_data <- data.frame(
    x = rnorm(10),
    y = rnorm(10)
  )

  res <- plot_quanti_quanti(test_data, "x", "y")

  expect_s3_class(res, "ggplot")
  expect_equal(res$labels$x, "x")
  expect_equal(res$labels$y, "y")
})
