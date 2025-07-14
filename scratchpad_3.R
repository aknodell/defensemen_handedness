readr::read_csv("../eh_pbp/eh_skater_bio_w_war.csv") |>
  dplyr::filter(Season >= "09-10") |>
  dplyr::group_by(
    id = `API ID`,
    Season,
    Age
  ) |>
  dplyr::summarise(
    toi = sum(TOI_All),
    war = sum(WAR),
    war_60 = war / (toi / 60)
  ) |>
  dplyr::group_by(Season) |>
  dplyr::filter(
    rank(-toi) <=
      dplyr::case_when(
        Season <= "17-18" ~ 30,
        Season <= "20-21" ~ 31,
        T ~ 32
      ) * 18
  ) |>
  dplyr::mutate(
    war_60_percentile = rank(war_60) / dplyr::n(),
    war_60_bin =
      war_60_percentile |>
      magrittr::multiply_by(10) |>
      floor() |>
      magrittr::divide_by(10)
  ) |>
  dplyr::group_by(
    age_bin = Age |> magrittr::divide_by(2) |> floor() |> magrittr::multiply_by(2),
    war_60_bin, Season
  ) |>
  dplyr::tally() |>
  dplyr::filter(age_bin < 35) |>
  dplyr::group_by(Season) |>
  dplyr::mutate(n_norm = n / max(n)) |>
  ggplot2::ggplot(ggplot2::aes(x = age_bin, y = war_60_bin, fill = n_norm)) +
  ggplot2::geom_raster() +
  ggplot2::geom_smooth(
    ggplot2::aes(weight = n_norm),
    color = "red",
    se = T
  ) +
  ggplot2::scale_x_continuous("Age") +
  ggplot2::scale_y_continuous(
    "WAR/60 Percentile",
    labels = scales::percent,
    breaks = c(0, .2, .4, .6, .8, 1)
  ) +
  ggplot2::scale_fill_viridis_c(option = "A") +
  ggplot2::facet_wrap(ggplot2::vars(Season), ncol = 5) +
  ggplot2::labs(
    title = "WAR/60 by Age Heatmap",
    subtitle = "Skaters 35 and Under, Frequency Normalized by Season, Top 18 Skaters Per Team by TOI All Situations",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.spacing.x = ggplot2::unit(2, "lines")
  )

readr::read_csv("../eh_pbp/eh_skater_bio_w_war.csv") |>
  dplyr::filter(Season >= "09-10") |>
  dplyr::group_by(
    id = `API ID`,
    Season,
    Age
  ) |>
  dplyr::summarise(
    toi = sum(TOI_All),
    war = sum(WAR),
    war_60 = war / (toi / 60),
    .groups = "drop"
  ) |>
  dplyr::group_by(Season) |>
  dplyr::filter(
    rank(-toi) <=
      dplyr::case_when(
        Season <= "17-18" ~ 30,
        Season <= "20-21" ~ 31,
        T ~ 32
      ) * 18
  ) |>
  dplyr::mutate(
    war_percentile = rank(war) / dplyr::n(),
    war_bin =
      war_percentile |>
      magrittr::multiply_by(10) |>
      floor() |>
      magrittr::divide_by(10)
  ) |>
  dplyr::group_by(
    age_bin = Age |> magrittr::divide_by(2) |> floor() |> magrittr::multiply_by(2),
    war_bin, Season
  ) |>
  dplyr::tally() |>
  dplyr::filter(age_bin < 35) |>
  dplyr::group_by(Season) |>
  dplyr::mutate(n_norm = n / max(n)) |>
  ggplot2::ggplot(ggplot2::aes(x = age_bin, y = war_bin, fill = n_norm)) +
  ggplot2::geom_raster() +
  ggplot2::geom_smooth(
    ggplot2::aes(weight = n_norm),
    color = "red",
    se = T
  ) +
  ggplot2::scale_x_continuous("Age") +
  ggplot2::scale_y_continuous(
    "WAR Percentile",
    labels = scales::percent,
    breaks = c(0, .2, .4, .6, .8, 1)
  ) +
  ggplot2::scale_fill_viridis_c(option = "A") +
  ggplot2::facet_wrap(ggplot2::vars(Season), ncol = 5) +
  ggplot2::labs(
    title = "WAR by Age Heatmap",
    subtitle = "Skaters 35 and Under, Frequency Normalized by Season, Top 18 Skaters Per Team by TOI All Situations",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.spacing.x = ggplot2::unit(2, "lines")
  )
