shot_locations_by_type <-
  readr::read_csv("../xs_v2/coord_adj_fenwick.csv") |>
  dplyr::filter(season_chr >= "19-20", event_zone == "Off") |>
  dplyr::mutate(event_type = ifelse(event_type == "POST", "MISS", event_type)) |>
  dplyr::group_by(season_chr, event_type, coords_x_adj, coords_y_adj) |>
  dplyr::tally() |>
  dplyr::group_by(season_chr) |>
  # dplyr::mutate(perc_overall = n / sum(n)) |>
  dplyr::group_by(season_chr, event_type) |>
  dplyr::mutate(
    perc_within_event_type = n / sum(n),
    perc_within_event_type_norm = (perc_within_event_type - min(perc_within_event_type)) / (max(perc_within_event_type) - min(perc_within_event_type))
  ) |>
  dplyr::ungroup()

# shot_location
# shot_locations_by_type |>
#   dplyr::mutate(
#     perc_within_event_type_norm = (perc_within_event_type - min(perc_within_event_type)) / (max(perc_within_event_type) - min(perc_within_event_type))
#   )
#   View()


tidyr::expand_grid(
  coords_x_adj = 0:64,
  coords_y_adj = -42:42,
  season_chr = c("19-20", "20-21", "21-22", "22-23", "23-24"),
  event_type = c("MISS", "SHOT", "GOAL")
) |>
  dplyr::left_join(
    shot_locations_by_type |>
      dplyr::group_by(season_chr, event_type) |>
      dplyr::mutate(
        perc_within_event_type_norm =
          (perc_within_event_type - min(perc_within_event_type)) /
          (max(perc_within_event_type) - min(perc_within_event_type))
      )
  ) |>
  dplyr::filter(!is.na(event_type), !is.na(season_chr)) |>
  dplyr::group_by(coords_x_adj, coords_y_adj, season_chr) |>
  dplyr::group_by(coords_x_adj, coords_y_adj, event_type) |>
  dplyr::arrange(coords_x_adj, coords_y_adj, event_type, season_chr) |>
  dplyr::mutate(
    change_within_event_type_norm =
      perc_within_event_type_norm -
      tidyr::replace_na(dplyr::lag(perc_within_event_type_norm), 0)
  ) |>
  dplyr::filter(season_chr != "19-20") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = coords_y_adj,
      y = coords_x_adj,
      fill = change_within_event_type_norm
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(event_type),
    cols = ggplot2::vars(season_chr)
  ) +
  ggplot2::scale_fill_viridis_c(option = "A") +
  ggplot2::labs(
    title = "Fenwick Heatmap 2019-24 by season",
    caption = "Data via Evolving-Hockey"
  ) +
  # # ggplot2::scale_x_continuous(limits = c(0, 64)) +
  # ggplot2::scale_y_continuous(limits = c(-42.5, 42.5)) +
  ggplot2::theme_minimal() +
  ggplot2::coord_fixed()




tidyr::expand_grid(
  coords_x_adj = 0:64,
  coords_y_adj = -42:42,
  season_chr = c("19-20", "20-21", "21-22", "22-23", "23-24")
) |>
  dplyr::left_join(
    shot_locations_by_type |>
      dplyr::group_by(coords_x_adj, coords_y_adj, season_chr, event_type) |>
      dplyr::summarise(n = sum(n, na.rm = T), .groups = "drop") |>
      dplyr::group_by(season_chr, event_type) |>
      dplyr::mutate(
        perc_overall = n / sum(n),
        perc_overall_norm = (perc_overall - min(perc_overall)) / (max(perc_overall) - min(perc_overall))
      ) |>
      dplyr::group_by(event_type) |>
      dplyr::mutate(n_norm = (n - min(n)) / (max(n) - min(n)))
  ) |>
  dplyr::filter(!is.na(event_type), !is.na(season_chr)) |>
  dplyr::arrange(coords_x_adj, coords_y_adj, event_type, season_chr) |>
  dplyr::group_by(coords_x_adj, coords_y_adj, event_type) |>
  dplyr::mutate(
    change =
      tidyr::replace_na(perc_overall_norm, 0) -
      tidyr::replace_na(dplyr::lag(perc_overall_norm), 0)
  ) |>
  # dplyr::filter(season_chr != "19-20") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = coords_y_adj,
      y = coords_x_adj,
      fill = n_norm
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(event_type),
    cols = ggplot2::vars(season_chr)
  ) +
  # ggplot2::scale_fill_gradient2(
  #   low = "darkblue",
  #   mid = "white",
  #   high = "darkred"
  # ) +
  ggplot2::scale_fill_viridis_c(option = "A", na.value = "white") +
  ggplot2::labs(
    title = "Fenwick Heatmap 2019-24 by Season",
    subtitle = "By Event Type",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::coord_fixed() +
  # # ggplot2::scale_x_continuous(limits = c(0, 64)) +
  # ggplot2::scale_y_continuous(limits = c(-42.5, 42.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )


tidyr::expand_grid(
  coords_x_adj = 0:64,
  coords_y_adj = -42:42,
  season_chr = c("19-20", "20-21", "21-22", "22-23", "23-24")
) |>
  dplyr::left_join(
    shot_locations_by_type |>
      dplyr::group_by(coords_x_adj, coords_y_adj, season_chr) |>
      dplyr::summarise(n = sum(n, na.rm = T), .groups = "drop") |>
      dplyr::group_by(season_chr) |>
      dplyr::mutate(
        perc_overall = n / sum(n),
        perc_overall_norm = (perc_overall - min(perc_overall)) / (max(perc_overall) - min(perc_overall))
      )
  ) |>
  dplyr::filter(!is.na(season_chr)) |>
  dplyr::arrange(coords_x_adj, coords_y_adj, season_chr) |>
  dplyr::group_by(coords_x_adj, coords_y_adj) |>
  dplyr::mutate(
    change =
      tidyr::replace_na(perc_overall_norm, 0) -
      tidyr::replace_na(dplyr::lag(perc_overall_norm), 0)
  ) |>
  # dplyr::filter(season_chr != "19-20") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = coords_y_adj,
      y = coords_x_adj,
      fill = perc_overall_norm
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::facet_grid(
    # rows = ggplot2::vars(event_type),
    cols = ggplot2::vars(season_chr)
  ) +
  # ggplot2::scale_fill_gradient2(
  #   low = "darkblue",
  #   mid = "white",
  #   high = "darkred"
  # ) +
  ggplot2::scale_fill_viridis_c(option = "A", na.value = "white") +
  ggplot2::labs(
    title = "Fenwick Heatmap 2019-24 by Season",
    subtitle = "All Fenwick Events",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::coord_fixed() +
  # # ggplot2::scale_x_continuous(limits = c(0, 64)) +
  # ggplot2::scale_y_continuous(limits = c(-42.5, 42.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )



tidyr::expand_grid(
  coords_x_adj = 0:64,
  coords_y_adj = -42:42,
  season_chr = c("19-20", "20-21", "21-22", "22-23", "23-24")
) |>
  dplyr::left_join(
    shot_locations_by_type |>
      dplyr::group_by(coords_x_adj, coords_y_adj, season_chr) |>
      dplyr::summarise(n = sum(n, na.rm = T), .groups = "drop") |>
      dplyr::group_by(season_chr) |>
      dplyr::mutate(
        perc_overall = n / sum(n),
        perc_overall_norm = (perc_overall - min(perc_overall)) / (max(perc_overall) - min(perc_overall))
      )
  ) |>
  dplyr::filter(!is.na(season_chr)) |>
  dplyr::arrange(coords_x_adj, coords_y_adj, season_chr) |>
  dplyr::group_by(coords_x_adj, coords_y_adj) |>
  dplyr::mutate(
    n_change =
      tidyr::replace_na(n, 0) -
      tidyr::replace_na(dplyr::lag(n), 0),
    change =
      tidyr::replace_na(perc_overall_norm, 0) -
      tidyr::replace_na(dplyr::lag(perc_overall_norm), 0)
  ) |>
  dplyr::filter(season_chr != "19-20") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = coords_y_adj,
      y = coords_x_adj,
      fill = n_change
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::facet_wrap(ggplot2::vars(season_chr)) +
  # ggplot2::facet_grid(
  #   # rows = ggplot2::vars(event_type),
  #   cols = ggplot2::vars(season_chr)
  # ) +
  ggplot2::scale_fill_gradient2(
    low = "darkblue",
    mid = "white",
    high = "darkred"
  ) +
  # ggplot2::scale_fill_viridis_c(option = "A", na.value = "white") +
  ggplot2::labs(
    title = "Fenwick Heatmap 2020-24 by Season",
    subtitle = "Change from Previous Season",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::coord_fixed() +
  # # ggplot2::scale_x_continuous(limits = c(0, 64)) +
  # ggplot2::scale_y_continuous(limits = c(-42.5, 42.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )

tidyr::expand_grid(
  coords_x_adj = 0:64,
  coords_y_adj = -42:42,
  season_chr = c("19-20", "20-21", "21-22", "22-23", "23-24")
) |>
  dplyr::left_join(
    shot_locations_by_type |>
      dplyr::group_by(coords_x_adj, coords_y_adj, season_chr, event_type) |>
      dplyr::summarise(n = sum(n, na.rm = T), .groups = "drop") |>
      dplyr::group_by(season_chr, event_type) |>
      dplyr::mutate(
        perc_overall = n / sum(n),
        perc_overall_norm = (perc_overall - min(perc_overall)) / (max(perc_overall) - min(perc_overall))
      )
  ) |>
  dplyr::filter(!is.na(event_type), !is.na(season_chr)) |>
  dplyr::arrange(coords_x_adj, coords_y_adj, event_type, season_chr) |>
  dplyr::group_by(coords_x_adj, coords_y_adj, event_type) |>
  dplyr::mutate(
    n_change =
      tidyr::replace_na(n, 0) -
      tidyr::replace_na(dplyr::lag(n), 0),
    perc_change_norm =
      tidyr::replace_na(perc_overall_norm, 0) -
      tidyr::replace_na(dplyr::lag(perc_overall_norm), 0)
  ) |>
  dplyr::filter(season_chr != "19-20") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = coords_y_adj,
      y = coords_x_adj,
      fill = n_change
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(event_type),
    cols = ggplot2::vars(season_chr)
  ) +
  ggplot2::scale_fill_gradient2(
    low = "darkblue",
    mid = "white",
    high = "darkred"
  ) +
  # ggplot2::scale_fill_viridis_c(option = "A", na.value = "white") +
  ggplot2::labs(
    title = "Fenwick Heatmap 2020-24 by Season",
    subtitle = "Raw Change from Previous Season By Event Type",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::coord_fixed() +
  # # ggplot2::scale_x_continuous(limits = c(0, 64)) +
  # ggplot2::scale_y_continuous(limits = c(-42.5, 42.5)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )




