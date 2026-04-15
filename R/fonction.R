moyenne_espece <- function(iris, var){
  aggregate(data[[var]],
            by = list(Espece = iris$Species),
            mean)
}
