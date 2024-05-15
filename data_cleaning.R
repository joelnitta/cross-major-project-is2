library(tidyverse)

# use this for converting JA numbers to ASCII
ja_en_nums <- c(
  "１" = "1",
  "２" = "2",
  "３" = "3",
  "４" = "4",
  "５" = "5",
  "６" = "6",
  "７" = "7",
  "８" = "8",
  "９" = "9",
  "０" = "0"
)

read_csv(
  "data/屋上はらっぱ植生調査 - とりまとめ.csv",
  show_col_types = FALSE,
  col_names = c(
    "quadrat", "name_ja", "name_en",
    "cover", "abun", "pop")
  ) |>
  # remove rows where every column is empty
  filter(!if_all(everything(), is.na)) |>
  filter(!is.na(quadrat)) |>
  # remove rows that actually contain metadata about year of collection
  filter(!str_detect(quadrat, "2024")) |>
  # split quadrat into plot and quadrat
  separate_wider_delim(
    quadrat, delim = "-", names = c("plot", "quadrat")) |>
  # remove rows with no cover, abun, or pop data
  filter(!(is.na(cover) & is.na(abun) & is.na(pop))) |>
  # replace JA font numbers with ASCII, convert to numeric
  mutate(
    across(cover:pop, ~str_replace_all(.x, ja_en_nums) |> parse_number())) |>
  # write out
  write_csv("data/okujo_2024.csv")
