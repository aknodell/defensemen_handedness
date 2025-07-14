cf_formula <-
  cf_per_60 ~
  score_diff +
  venue +
  pairing_evo_rf +
  teammate_f_evo_rf +
  opp_d_evd_ra +
  opp_f_evd_ra

cf_formula_w_handedness <-
  cf_per_60 ~
  balanced +
  score_diff +
  venue +
  pairing_evo_rf +
  teammate_f_evo_rf +
  opp_d_evd_ra +
  opp_f_evd_ra

ca_formula <-
  ca_per_60 ~
  score_diff +
  venue +
  pairing_evd_ra +
  teammate_f_evd_ra +
  opp_d_evo_rf +
  opp_f_evo_rf

ca_formula_w_handedness <-
  ca_per_60 ~
  balanced +
  score_diff +
  venue +
  pairing_evd_ra +
  teammate_f_evd_ra +
  opp_d_evo_rf +
  opp_f_evo_rf

xgf_formula <-
  xgf_per_60 ~
  score_diff +
  venue +
  cf_per_60 +
  pairing_evo_qf +
  teammate_f_evo_qf +
  opp_d_evd_qa +
  opp_f_evd_qa

xgf_formula_w_handedness <-
  xgf_per_60 ~
  balanced +
  score_diff +
  venue +
  cf_per_60 +
  pairing_evo_qf +
  teammate_f_evo_qf +
  opp_d_evd_qa +
  opp_f_evd_qa

xga_formula <-
  xga_per_60 ~
  score_diff +
  venue +
  ca_per_60 +
  pairing_evd_qa +
  teammate_f_evd_qa +
  opp_d_evo_qf +
  opp_f_evo_qf

xga_formula_w_handedness <-
  xga_per_60 ~
  balanced +
  score_diff +
  venue +
  ca_per_60 +
  pairing_evd_qa +
  teammate_f_evd_qa +
  opp_d_evo_qf +
  opp_f_evo_qf

all_pairing_gamelogs |>
  dplyr::filter(
    !(season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))
  ) |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf = sum(cf),
    league_toi = sum(toi / 3600),
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    cf_per_60 = cf / (toi / 3600),
    xgf_per_60 = xgf / (toi / 3600)
  ) |>
  lm(formula = cf_formla_w_handedness) |>
  summary()

all_pairing_gamelogs |>
  dplyr::filter(
    !(season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))
  ) |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf = sum(cf),
    league_toi = sum(toi / 3600),
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    ca_per_60 = ca / (toi / 3600),
    xga_per_60 = xga / (toi / 3600)
  ) |>
  lm(formula = ca_formla_w_handedness) |>
  summary()

all_pairing_gamelogs |>
  dplyr::filter(
    !(season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))
  ) |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf = sum(cf),
    league_toi = sum(toi / 3600),
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    cf_per_60 = cf / (toi / 3600),
    xgf_per_60 = xgf / (toi / 3600)
  ) |>
  lm(formula = xgf_formula_w_handedness) |>
  summary()

all_pairing_gamelogs |>
  dplyr::filter(
    !(season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))
  ) |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf = sum(cf),
    league_toi = sum(toi / 3600),
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    ca_per_60 = ca / (toi / 3600),
    xga_per_60 = xga / (toi / 3600)
  ) |>
  lm(formula = xga_formula_w_handedness) |>
  summary()
  purrr::pluck("sigma")
  as.data.frame() |>
  tibble::rownames_to_column("coef") |>
  tibble::as_tibble()
  # View()




all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(season) |>
  tidyr::nest() |>
  dplyr::mutate(
    coefs =
      purrr::map(
        data,
        function(d) {
          d <-
            d |>
            dplyr::mutate(
              league_cf = sum(cf),
              league_toi = sum(toi / 3600),
              league_cf_per_60 = sum(cf) / sum(toi / 3600),
              venue = as.integer(venue == "home"),
              balanced = as.integer(balanced)
            ) |>
            dplyr::filter(toi >= 200) |>
            dplyr::mutate(
              cf_per_60 = cf / (toi / 3600),
              ca_per_60 = ca / (toi / 3600),
              xgf_per_60 = xgf / (toi / 3600),
              xga_per_60 = xga / (toi / 3600)
            )

          d |>
            lm(formula = cf_formula_w_handedness) |>
            summary() |>
            purrr::pluck("coefficients") |>
            as.data.frame() |>
            tibble::rownames_to_column("coef") |>
            tibble::as_tibble() |>
            dplyr::filter(coef == "balanced") |>
            dplyr::mutate(model = "Corsi For") |>
            dplyr::bind_rows(
              d |>
                lm(formula = ca_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "Corsi Against")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xgf_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG For")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xga_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG Against")
            )
        }
      )
  ) |>
  tidyr::unnest(coefs) |>
  dplyr::mutate(
    model = model |> factor(levels = c("Corsi For", "Corsi Against", "xG For", "xG Against")),
    lower_ci = Estimate - (`Std. Error` * 1.96),
    upper_ci = Estimate + (`Std. Error` * 1.96)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = factor(season) |> as.integer(), y = Estimate)
  ) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2) +
  ggplot2::geom_ribbon(ggplot2::aes(ymax = upper_ci, ymin = lower_ci), alpha = 0.3) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_x_continuous(
    "",
    labels =
      c(
        "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16",
        "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"
      ),
    # c("16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"),
    breaks = c(1:15),
    minor_breaks = NULL
  ) +
  ggplot2::scale_y_continuous("Balanced Pairing Coefficient") +
  ggplot2::facet_wrap(ggplot2::vars(model), ncol = 2, scales = "free") +
  ggplot2::labs(
    title = "Impact of Balanced Handedness on Defensive Pairing Shot Rates",
    subtitle = "5-on-5, Minimum of 5 Shifts Together Per Game, Controling for Teammates, Competition, Score, and Venue",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.spacing.x = ggplot2::unit(2, "lines")
  )


all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(
    pre = season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")
  ) |>
  tidyr::nest() |>
  dplyr::mutate(
    coefs =
      purrr::map(
        data,
        function(d) {
          d <-
            d |>
            dplyr::mutate(
              league_cf = sum(cf),
              league_toi = sum(toi / 3600),
              league_cf_per_60 = sum(cf) / sum(toi / 3600),
              venue = as.integer(venue == "home"),
              balanced = as.integer(balanced)
            ) |>
            dplyr::filter(toi >= 200) |>
            dplyr::mutate(
              cf_per_60 = cf / (toi / 3600),
              ca_per_60 = ca / (toi / 3600),
              xgf_per_60 = xgf / (toi / 3600),
              xga_per_60 = xga / (toi / 3600)
            )

          d |>
            lm(formula = cf_formula_w_handedness) |>
            summary() |>
            purrr::pluck("coefficients") |>
            as.data.frame() |>
            tibble::rownames_to_column("coef") |>
            tibble::as_tibble() |>
            dplyr::filter(coef == "balanced") |>
            dplyr::mutate(model = "Corsi For") |>
            dplyr::bind_rows(
              d |>
                lm(formula = ca_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "Corsi Against")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xgf_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG For")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xga_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG Against")
            )
        }
      )
  ) |>
  tidyr::unnest(coefs)



all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(
    pre = season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")
  ) |>
  tidyr::nest() |>
  dplyr::mutate(
    coefs =
      purrr::map(
        data,
        function(d) {
          d <-
            d |>
            dplyr::mutate(
              league_cf = sum(cf),
              league_toi = sum(toi / 3600),
              league_cf_per_60 = sum(cf) / sum(toi / 3600),
              venue = as.integer(venue == "home"),
              balanced = as.integer(balanced)
            ) |>
            dplyr::filter(toi >= 200) |>
            dplyr::mutate(
              cf_per_60 = cf / (toi / 3600),
              ca_per_60 = ca / (toi / 3600),
              xgf_per_60 = xgf / (toi / 3600),
              xga_per_60 = xga / (toi / 3600)
            )

          tibble::tibble(
            formula =
              c(
                cf_formula_w_handedness,
                ca_formula_w_handedness,
                xgf_formula_w_handedness,
                xga_formula_w_handedness,
                cf_formula,
                ca_formula,
                xgf_formula,
                xga_formula
              ),
            name =
              c(
                "cf_formula_w_handedness",
                "ca_formula_w_handedness",
                "xgf_formula_w_handedness",
                "xga_formula_w_handedness",
                "cf_formula",
                "ca_formula",
                "xgf_formula",
                "xga_formula"
              )
          ) |>
            dplyr::mutate(
              data =
                purrr::map(
                  formula,
                  function(f) {
                    s <-
                      d |>
                      lm(formula = f) |>
                      summary()

                    tibble::tibble(
                      r_squared = s |> purrr::pluck("adj.r.squared"),
                      err = s |> purrr::pluck("sigma")
                    )
                  }
                )
            ) |>
            tidyr::unnest(data)
        }
      )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(coefs) |>
  View()






all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(
    pre = (season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")),
    balanced
  ) |>
  dplyr::summarise(
    cf_per_60 = sum(cf, na.rm = T) / sum(toi / 3600, na.rm = T),
    ca_per_60 = sum(ca, na.rm = T) / sum(toi / 3600, na.rm = T),
    xgf_per_60 = sum(xgf, na.rm = T) / sum(toi / 3600, na.rm = T),
    xga_per_60 = sum(xga, na.rm = T) / sum(toi / 3600, na.rm = T)
  ) |>
  tidyr::pivot_longer(cf_per_60:xga_per_60) |>
  dplyr::mutate(
    metric = ifelse(stringr::str_detect(name, "c[fa]"), "Corsi", "Expected Goals"),
    direction =
      ifelse(stringr::str_detect(name, "[cg]f"), "For", "Against") |>
      factor(levels = c("For", "Against")),
    seasons = ifelse(pre, "2009-16", "2016-24")
  ) |>
  dplyr::filter(metric == "Corsi") |>
  ggplot2::ggplot(ggplot2::aes(x = direction, y = value, fill = !balanced)) +
  ggplot2::geom_col(position = "dodge", width = 0.5) +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Per 60 Rate") +
  ggplot2::scale_fill_manual(
    name = "",
    labels = c(`FALSE` = "Balanced Handedness", `TRUE` = "Unbalanced Handedness"),
    values = c(`FALSE` = '#00BFC4', `TRUE` = '#F8766D')
  ) +
  # ggplot2::scale_fill_discrete(
  #   name = "",
  #   breaks = c(!TRUE, !FALSE),
  #   labels = c(`FALSE` = "Balanced Handedness", `TRUE` = "Unbalanced Handedness")
  # ) +
  ggplot2::facet_wrap(facets = ggplot2::vars(seasons), ncol = 2) +
  ggplot2::labs(
    title = "Corsi Rates By Defensive Pairing Handedness",
    subtitle = "5-on-5, 2009-24 Seasons, Minimum 5 Shifts Together Per Game",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "right",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(2, "lines")
  )


all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(
    pre = (season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")),
    balanced
  ) |>
  dplyr::summarise(
    cf_per_60 = sum(cf, na.rm = T) / sum(toi / 3600, na.rm = T),
    ca_per_60 = sum(ca, na.rm = T) / sum(toi / 3600, na.rm = T),
    xgf_per_60 = sum(xgf, na.rm = T) / sum(toi / 3600, na.rm = T),
    xga_per_60 = sum(xga, na.rm = T) / sum(toi / 3600, na.rm = T)
  ) |>
  tidyr::pivot_longer(cf_per_60:xga_per_60) |>
  dplyr::mutate(
    metric = ifelse(stringr::str_detect(name, "c[fa]"), "Corsi", "Expected Goals"),
    direction =
      ifelse(stringr::str_detect(name, "[cg]f"), "For", "Against") |>
      factor(levels = c("For", "Against")),
    seasons = ifelse(pre, "2009-16", "2016-24")
  ) |>
  dplyr::filter(metric == "Expected Goals") |>
  ggplot2::ggplot(ggplot2::aes(x = direction, y = value, fill = !balanced)) +
  ggplot2::geom_col(position = "dodge", width = 0.5) +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Per 60 Rate") +
  ggplot2::scale_fill_manual(
    name = "",
    labels = c(`FALSE` = "Balanced Handedness", `TRUE` = "Unbalanced Handedness"),
    values = c(`FALSE` = '#00BFC4', `TRUE` = '#F8766D')
  ) +
  ggplot2::facet_wrap(facets = ggplot2::vars(seasons), ncol = 2) +
  ggplot2::labs(
    title = "Expected Goals Rates By Defensive Pairing Handedness",
    subtitle = "5-on-5, 2009-24 Seasons, Minimum 5 Shifts Together Per Game",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "right",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(2, "lines")
  )



all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(season) |>
  dplyr::summarise(
    balanced_toi_perc =
      sum(toi * balanced, na.rm = T) /
      sum(toi, na.rm = T)
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(season) |> as.integer(),
    y = 1-balanced_toi_perc)
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(linewidth = 1) +
  # ggplot2::geom_smooth(method = "lm", linetype = 2, color = "red", linewith = 1, formula = "y ~ poly(x, 2)") +
  ggplot2::geom_smooth(
    ggplot2::aes(group = (as.character(season)) < "16-17"),
    method = "lm",
    linetype = 2,
    color = "red",
    linewidth = 1
    # formula = "y ~ poly(x, 2)"
  ) +
  ggplot2::scale_x_continuous(
    "",
    labels =
      c(
        "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16",
        "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"
      ),
    # c("16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"),
    breaks = c(1:15),
    minor_breaks = NULL
  ) +
  ggplot2::scale_y_continuous(
    "",
    limits = c(0, 0.6),
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = "Percent of Available Minutes Played by a Defenseman on His Off-Side by Season",
    subtitle = "5v5, Pairings With At Least 5 Shifts in a Game",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal()

all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::group_by(
    pre = (season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16"))
  ) |>
  tidyr::nest() |>
  dplyr::mutate(
    coefs =
      purrr::map(
        data,
        function(d) {
          d <-
            d |>
            dplyr::mutate(
              league_cf = sum(cf),
              league_toi = sum(toi / 3600),
              league_cf_per_60 = sum(cf) / sum(toi / 3600),
              venue = as.integer(venue == "home"),
              balanced = as.integer(balanced)
            ) |>
            dplyr::filter(toi >= 200) |>
            dplyr::mutate(
              cf_per_60 = cf / (toi / 3600),
              ca_per_60 = ca / (toi / 3600),
              xgf_per_60 = xgf / (toi / 3600),
              xga_per_60 = xga / (toi / 3600)
            )

          d |>
            lm(formula = cf_formula_w_handedness) |>
            summary() |>
            purrr::pluck("coefficients") |>
            as.data.frame() |>
            tibble::rownames_to_column("coef") |>
            tibble::as_tibble() |>
            dplyr::filter(coef == "balanced") |>
            dplyr::mutate(model = "Corsi For") |>
            dplyr::bind_rows(
              d |>
                lm(formula = ca_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "Corsi Against")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xgf_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG For")
            ) |>
            dplyr::bind_rows(
              d |>
                lm(formula = xga_formula_w_handedness) |>
                summary() |>
                purrr::pluck("coefficients") |>
                as.data.frame() |>
                tibble::rownames_to_column("coef") |>
                tibble::as_tibble() |>
                dplyr::filter(coef == "balanced") |>
                dplyr::mutate(model = "xG Against")
            )
        }
      )
  ) |>
  tidyr::unnest(coefs) |>
  dplyr::mutate(
    model = model |> factor(levels = c("Corsi For", "Corsi Against", "xG For", "xG Against")),
    Seasons = ifelse(pre, "2009-16", "2016-24"),
    lower_ci = Estimate - (`Std. Error` * 1.96),
    upper_ci = Estimate + (`Std. Error` * 1.96)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = Seasons, y = Estimate, color = !(lower_ci > 0 | upper_ci < 0))) +
  ggplot2::geom_hline(yintercept = 0, color = "red", linetype = 2) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_segment(
    ggplot2::aes(y = lower_ci, yend = upper_ci),
    linewidth = 1
  ) +
  ggplot2::facet_wrap(ggplot2::vars(model), ncol = 4, scales = "free_y") +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous("Balanced Pairing Coefficient") +
  ggplot2::scale_color_manual(
    "Statistially Significant at 95% confidence",
    labels =
      c(
        `FALSE` = "Statistically Significant",
        `TRUE` = "Not Statistically Significant"
      ),
    values =
      c(
        `FALSE` = "#00BFC4",
        `TRUE` = "black"
      )
  ) +
  ggplot2::labs(
    title = "Impact of Handedness on Defensive Pairing Shot Rates",
    subtitle = "5-on-5, Minimum of 5 Shifts Together Per Game, Controling for Teammates, Competition, Score, and Venue",
    caption = "Data via Evolving-Hockey"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(2, "lines")
  )


all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  lm(
    formula =
      cf ~
        toi *
        (score_diff +
        venue +
        pairing_evo_rf +
        teammate_f_evo_rf +
        opp_d_evd_ra +
        opp_f_evd_ra +
        balanced)
  ) |>
  summary()

all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  lm(
    formula =
      ca ~
      toi *
      (score_diff +
      venue +
      pairing_evd_ra +
      teammate_f_evd_ra +
      opp_d_evo_rf +
      opp_f_evo_rf)
  ) |>
  summary()

all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    cf_per_60 = cf / (toi / 3600),
    xgf_per_60 = xgf / (toi / 3600)
  ) |>
  lm(formula = xgf_formula) |>
  summary()







