library(tidyverse)
library(rinat)

nishi_chiba_obs <- get_inat_obs(
  place_id = "chiba-university-nishi-chiba-campus",
  maxresults = 10000
) |>
  as_tibble()

nishi_chiba_obs |>
  slice(1) |>
  glimpse()

# Find top observer for each taxon group

top_observers <- nishi_chiba_obs |>
  filter(!is.na(iconic_taxon_name)) |>
  count(iconic_taxon_name, user_login, sort = TRUE) |>
  group_by(iconic_taxon_name) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup()

top_observers

nishi_chiba_obs |>
  mutate(hour = lubridate::hour(datetime)) |>
  count(hour, iconic_taxon_name)

library(tidyverse)
library(lubridate)

nishi_chiba_obs |>
  mutate(
    datetime = with_tz(ymd_hms(datetime), tzone = "Asia/Tokyo"),
    date = as_date(datetime),
    hour = hour(datetime)
  ) |>
  filter(hour >= 6, hour < 20) |>
  count(date, hour, iconic_taxon_name) |>
  complete(
    date,
    hour = 6:19,
    iconic_taxon_name,
    fill = list(n = 0)
  ) |>
  group_by(hour, iconic_taxon_name) |>
  summarize(
    mean_obs = mean(n),
    .groups = "drop"
  ) |>
  group_by(iconic_taxon_name) |>
  mutate(
    percent = mean_obs / sum(mean_obs) * 100
  ) |>
  ggplot(aes(hour, percent, color = iconic_taxon_name)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 6:19) +
  labs(
    x = "Hour of day (Japan time)",
    y = "Percent of observations",
    color = "Iconic taxon"
  ) +
  theme_minimal()


obs_table <- nishi_chiba_obs |>
  mutate(
    datetime = with_tz(ymd_hms(datetime), tzone = "Asia/Tokyo"),
    hour = hour(datetime)
  ) |>
  filter(hour >= 6, hour < 20) |>
  count(iconic_taxon_name, hour) |>

  pivot_wider(
    names_from = hour,
    values_from = n,
    values_fill = 0
  )

mat <- obs_table |>
  column_to_rownames("iconic_taxon_name") |>
  as.matrix()

chisq.test(mat)
