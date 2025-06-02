library(tidyverse)

okujo_2024 <- read_csv("data/okujo_2024.csv") |>
  mutate(year = 2024)
okujo_2025 <- read_csv("data/okujo_2025.csv") |>
  mutate(year = 2025)

okujo <- bind_rows(okujo_2024, okujo_2025) |>
  filter(!is.na(name_ja)) |>
  filter(!str_detect(name_ja, "？")) |>
  mutate(year = as.factor(year))

# abunとcoverはあまり変わらない
ggplot(okujo, aes(x = abun, y = cover)) + geom_point()

# 最も多い種類はどちらの年にしても、多い
okujo |>
  group_by(name_ja, year) |>
  summarize(total_abun = sum(abun)) |>
  ggplot(
    aes(
      x = total_abun,
      y = name_ja,
      fill = year
    )
  ) +
  geom_col(position = "dodge") +
  theme_gray(base_family = "HiraKakuPro-W3")

# B は最も年間の変化が少ない
ggplot(
  okujo,
  aes(
    x = abun,
    y = name_ja,
    fill = year
  )
) +
  geom_col(position = "dodge") +
  theme_gray(base_family = "HiraKakuPro-W3") +
  facet_wrap(vars(plot))

# 種数には明確なパターンがない
okujo |>
  group_by(plot, year) |>
  summarize(richness = n_distinct(name_ja)) |>
  ggplot(
    aes(x = plot, y = richness, fill = year)
  ) +
  geom_col(position = "dodge")

