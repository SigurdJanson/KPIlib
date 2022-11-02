sapply(x$domain, \(x) strsplit(x, ",")) |>
  unlist() |>
  unname() |>
  trimws() |>
  tolower() |>
  na.omit() |> table() |> View()

