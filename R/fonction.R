moyenne_espece <- function(data, nom_espece) {
  data |>
    filter(Species == nom_espece) |>
    pull(Sepal.Length) |>
    mean()
}
