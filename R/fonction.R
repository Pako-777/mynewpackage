#'Statistiques descriptives d'une variable quantitative
#'
#'
#'Calcule les principales statistiques descriptives (moyenne, médiane,écart-type, minimum, maximum) pour une variable numérique d'un data.frame.
#' @param data Un data.frame contenant les données.
#' @param variable Le nom ou numero(position de la colonne) de la variable à analyser
#'
#'
#' @returns Un data.frame contenant les statistiques descriptives.
#'
#'
#' @importFrom stats median sd
#'
#'
#' @export
#'
#'
#' @examples
#' stats_var(iris, "Sepal.Length")
#' stats_var(iris, 1)

stats_var <- function(data, variable){

  x <- as.numeric(data[[variable]])

  data.frame(
    variable = names(data)[variable],
    moyenne = mean(x, na.rm = TRUE),
    mediane = median(x, na.rm = TRUE),
    ecart_type = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}



#' Matrices de corrélation (Pearson et Spearman)
#'
#'
#'Calcule les matrices de corrélation de Pearson et Spearman pour un ensemble de variables quantitatives.
#'
#'
#' @param data Un data.frame contenant les données.
#' @param variables  Un vecteur de noms de colonnes à inclure dans les corrélations.
#'
#'
#' @returns Une liste contenant deux matrices : pearson et spearman.
#' @export
#'
#'
#' @importFrom stats cor
#'
#'
#' @examples
#' corr_vars(iris, c("Sepal.Length", "Sepal.Width", "Petal.Length"))
#' corr_vars(iris, c(1, 2, 3))

corr_vars <- function(data, variables){

  x <- data[, variables]

  list(
    pearson = cor(x, method = "pearson", use = "complete.obs"),
    spearman = cor(x, method = "spearman", use = "complete.obs")
  )
}



#'Analyse de variance (ANOVA) à un facteur
#'
#'
#'Réalise une ANOVA pour tester l'effet d'un facteur sur une variable quantitative.
#'
#'
#' @param data Un data.frame contenant les données.
#' @param variable Le nom de la variable quantitative.
#' @param group Le nom de la variable catégorielle (facteur).
#'
#'
#' @returns Le tableau de résultats de l'ANOVA.
#' @export
#'
#'
#' @importFrom stats as.formula aov
#'
#'
#' @examples
#' anova_var(iris, "Sepal.Length", "Species")

anova_var <- function(data, variable, group){

  formule <- as.formula(paste(variable, "~", group))

  modele <- aov(formule, data = data)

  summary(modele)
}



#' Nuage de points entre deux variables quantitatives
#'
#'
#'Produit un graphique représentant la relation entre deux variables quantitatives, accompagné d'une droite de régression linéaire.
#'
#' @param data Un data.frame contenant les données.
#' @param x Le nom de la variable en abscisse.
#' @param y Le nom de la variable en ordonnée.
#'
#'
#' @returns Un objet ggplot2 représentant le graphique.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs
#' @importFrom rlang .data
#'
#' @export
#' @examples
#' plot_quanti_quanti(iris, "Sepal.Length", "Petal.Length")

plot_quanti_quanti <- function(data, x, y){

  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color="red") +
    labs(
      x = x,
      y = y,
      title = paste("Relation entre", x, "et", y)
    )
}


