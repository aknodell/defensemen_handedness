skaters <-
  readr::read_csv("../eh_pbp/eh_skater_bio_w_war.csv") |>
  # dplyr::filter(Season == "23-24") |>
  dplyr::transmute(
    on = EH_ID,
    season = Season,
    team = Team,
    shoots = Shoots,
    # team = Team,
    position = ifelse(stringr::str_detect(Position, "D"), "D", "F")
  ) |>
  dplyr::distinct()

xgar <-
  readr::read_csv("../eh_pbp/eh_skaters_xgaa_all.csv") |>
  dplyr::transmute(
    on = EH_ID,
    season = Season,
    team = Team,
    position = ifelse(stringr::str_detect(Position, "D"), "D", "F"),
    evo_rf_rate = EV_RF / (TOI_EV / 60),
    evd_ra_rate = EV_RA / (TOI_EV / 60),
    evo_qf_rate = EV_QF / (TOI_EV / 60),
    evd_qa_rate = EV_QA / (TOI_EV / 60)
  ) |>
  dplyr::distinct()

# test_shifts <-
#   readr::read_csv(
#     "../eh_pbp/eh_pbp_shifts_23-24.csv"
#   ) |>
#   dplyr::group_by(game_id) |>
#   dplyr::mutate(
#     shift_length = dplyr::lead(game_seconds) - game_seconds
#   ) |>
#   dplyr::filter(game_strength_state == "5v5", game_id == 2023020001)
#
# test_shots <-
#   readr::read_csv(
#     "../eh_pbp/eh_pbp_shots_23-24.csv"
#   ) |>
#   dplyr::group_by(game_id) |>
#   dplyr::filter(game_strength_state == "5v5", game_id == 2023020001)

all_pairing_gamelogs <-
  c(
    "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16",
    "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"
  ) |>
  purrr::map(
    function(s) {
      message(s)

      shifts <-
        readr::read_csv(
          "../eh_pbp/eh_pbp_shifts_{s}.csv" |> glue::glue()
        ) |>
        dplyr::group_by(game_id) |>
        dplyr::mutate(
          shift_length = dplyr::lead(game_seconds) - game_seconds
        ) |>
        dplyr::filter(game_strength_state == "5v5")

      shots <-
        readr::read_csv(
          "../eh_pbp/eh_pbp_shots_{s}.csv" |> glue::glue()
        ) |>
        dplyr::group_by(game_id) |>
        dplyr::filter(game_strength_state == "5v5")

      message("getting pairing shifts")
      pairing_shifts <-
        shifts |>
        dplyr::filter(game_strength_state == "5v5") |>
        tidyr::pivot_longer(
          c(home_on_1:home_on_7, away_on_1:away_on_7),
          names_to = "on_venue",
          values_to = "on"
        ) |>
        dplyr::filter(!is.na(on)) |>
        dplyr::transmute(
          game_id, game_seconds, shift_index, shift_length,
          venue = on_venue |> stringr::str_sub(end = 4),
          team = ifelse(venue == "home", home_team, away_team),
          on
        ) |>
        dplyr::left_join(skaters |> dplyr::filter(season == s), by = c("on", "team")) |>
        dplyr::filter(position == "D") |>
        dplyr::group_by(game_id, shift_index, team) |>
        dplyr::mutate(defensemen_on = dplyr::n()) |>
        dplyr::group_by(game_id, shift_index) |>
        dplyr::filter(all(defensemen_on == 2)) |>
        dplyr::left_join(
          xgar |> dplyr::filter(season == s)
        ) |>
        dplyr::group_by(game_id, game_seconds, shift_index, shift_length, team, venue) |>
        dplyr::summarise(
          pairing = on |> sort() |> stringr::str_c(collapse = "-"),
          balanced = shoots |> unique() |> length() |> magrittr::equals(2),
          pairing_evo_rf = sum(evo_rf_rate),
          pairing_evd_ra = sum(evd_ra_rate),
          pairing_evo_qf = sum(evo_qf_rate),
          pairing_evd_qa = sum(evd_qa_rate),
          .groups = "drop"
        )

      message("getting score diffs")
      score_diffs <-
        pairing_shifts |>
        dplyr::select(game_id, shift_index, venue) |>
        dplyr::left_join(
          shifts |>
            dplyr::transmute(
              game_id,
              shift_index,
              venue = "home",
              score_diff = home_score - away_score
            ) |>
            dplyr::bind_rows(
              shifts |>
                dplyr::transmute(
                  game_id,
                  shift_index,
                  venue = "away",
                  score_diff = away_score - home_score
                )
            )
        )

      message("getting on-ice")
      on_ice_shots <-
        pairing_shifts |>
        dplyr::select(game_id, shift_index, venue) |>
        dplyr::left_join(
          shots |>
            dplyr::group_by(game_id, shift_index, venue = "home") |>
            dplyr::summarise(
              cf = sum(event_team == home_team, na.rm = T),
              ca = sum(event_team == away_team, na.rm = T),
              xgf = sum((event_team == home_team) * pred_goal, na.rm = T),
              xga = sum((event_team == away_team) * pred_goal, na.rm = T),
              .groups = "drop"
            ) |>
            dplyr::bind_rows(
              shots |>
                dplyr::group_by(game_id, shift_index, venue = "away") |>
                dplyr::summarise(
                  cf = sum(event_team == away_team, na.rm = T),
                  ca = sum(event_team == home_team, na.rm = T),
                  xgf = sum((event_team == away_team) * pred_goal, na.rm = T),
                  xga = sum((event_team == home_team) * pred_goal, na.rm = T),
                  .groups = "drop"
                )
            )
        )

      message("getting qot")
      qot <-
        pairing_shifts |>
        dplyr::select(game_id, shift_index, venue) |>
        dplyr::left_join(
          shifts |>
            dplyr::select(
              game_id, team = home_team, shift_index, home_on_1:home_on_7
            ) |>
            dplyr::mutate(venue = "home") |>
            tidyr::pivot_longer(home_on_1:home_on_7, values_to = "on") |>
            dplyr::bind_rows(
              shifts |>
                dplyr::select(
                  game_id, team = away_team, shift_index, away_on_1:away_on_7
                ) |>
                dplyr::mutate(venue = "away") |>
                tidyr::pivot_longer(away_on_1:away_on_7, values_to = "on")
            ) |>
            dplyr::left_join(
              xgar |> dplyr::filter(season == s)
            ) |>
            dplyr::filter(position == "F") |>
            dplyr::group_by(game_id, venue, shift_index) |>
            dplyr::summarise(
              teammate_evo_rf = sum(evo_rf_rate),
              teammate_evd_ra = sum(evd_ra_rate),
              teammate_evo_qf = sum(evo_qf_rate),
              teammate_evd_qa = sum(evd_qa_rate),
              .groups = "drop"
            )
        )

      message("getting qoc")
      qoc <-
        pairing_shifts |>
        dplyr::select(game_id, shift_index, venue) |>
        dplyr::left_join(
          shifts |>
            dplyr::select(
              game_id, team = away_team, shift_index, away_on_1:away_on_7
            ) |>
            dplyr::mutate(venue = "home") |>
            tidyr::pivot_longer(away_on_1:away_on_7, values_to = "on") |>
            dplyr::bind_rows(
              shifts |>
                dplyr::select(
                  game_id, team = home_team, shift_index, home_on_1:home_on_7
                ) |>
                dplyr::mutate(venue = "away") |>
                tidyr::pivot_longer(home_on_1:home_on_7, values_to = "on")
            ) |>
            dplyr::left_join(
              xgar |> dplyr::filter(season == s)
            ) |>
            dplyr::filter(!is.na(position)) |>
            dplyr::group_by(game_id, venue, shift_index) |>
            dplyr::summarise(
              opp_f_evo_rf = sum(evo_rf_rate * (position == "F"), na.rm = T),
              opp_f_evd_ra = sum(evd_ra_rate * (position == "F"), na.rm = T),
              opp_f_evo_qf = sum(evo_qf_rate * (position == "F"), na.rm = T),
              opp_f_evd_qa = sum(evd_qa_rate * (position == "F"), na.rm = T),
              opp_d_evo_rf = sum(evo_rf_rate * (position == "D"), na.rm = T),
              opp_d_evd_ra = sum(evd_ra_rate * (position == "D"), na.rm = T),
              opp_d_evo_qf = sum(evo_qf_rate * (position == "D"), na.rm = T),
              opp_d_evd_qa = sum(evd_qa_rate * (position == "D"), na.rm = T),
              .groups = "drop"
            )
        )

      pairing_shifts |>
        dplyr::left_join(score_diffs) |>
        dplyr::left_join(on_ice_shots) |>
        dplyr::left_join(qot) |>
        dplyr::left_join(qoc) |>
        dplyr::group_by(
          season = s, game_id, team, venue, pairing, balanced
        ) |>
        dplyr::summarise(
          toi = sum(shift_length),
          score_diff = weighted.mean(score_diff, shift_length),
          cf = sum(cf, na.rm = T),
          ca = sum(ca, na.rm = T),
          xgf = sum(xgf, na.rm = T),
          xga = sum(xga, na.rm = T),
          pairing_evo_rf = mean(pairing_evo_rf),
          pairing_evd_ra = mean(pairing_evd_ra),
          pairing_evo_qf = mean(pairing_evo_qf),
          pairing_evd_qa = mean(pairing_evd_qa),
          teammate_f_evo_rf = weighted.mean(teammate_evo_rf, shift_length),
          teammate_f_evd_ra = weighted.mean(teammate_evd_ra, shift_length),
          teammate_f_evo_qf = weighted.mean(teammate_evo_qf, shift_length),
          teammate_f_evd_qa = weighted.mean(teammate_evd_qa, shift_length),
          opp_f_evo_rf = weighted.mean(opp_f_evo_rf, shift_length),
          opp_f_evd_ra = weighted.mean(opp_f_evd_ra, shift_length),
          opp_f_evo_qf = weighted.mean(opp_f_evo_qf, shift_length),
          opp_f_evd_qa = weighted.mean(opp_f_evd_qa, shift_length),
          opp_d_evo_rf = weighted.mean(opp_d_evo_rf, shift_length),
          opp_d_evd_ra = weighted.mean(opp_d_evd_ra, shift_length),
          opp_d_evo_qf = weighted.mean(opp_d_evo_qf, shift_length),
          opp_d_evd_qa = weighted.mean(opp_d_evd_qa, shift_length),
          .groups = "drop"
        )
    }
  ) |>
  dplyr::bind_rows()



all_pairing_gamelogs |>
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
  dplyr::group_by(season) |>
  dplyr::summarise(
    cf_per_60 = sum(cf) / sum(toi / 3600)
  )


all_pairing_gamelogs |>
  dplyr::filter(
    season %in% c("09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")
  ) |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf = sum(cf),
    league_toi = sum(toi / 3600),
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  # dplyr::filter(pairing |> stringr::str_detect("MIRO.HEISKANEN")) |>
  dplyr::filter(toi >= 200) |>
  # dplyr::group_by(season, pairing) |>
  # dplyr::summarise(
  #   league_cf_per_60 = unique((league_cf - sum(cf)) / (league_toi - sum(toi / 3600))),
  #   cf_per_60 = sum(cf) / sum(toi / 3600),
  #   balanced = unique(balanced),
  #   pairing_evo_rf = unique(pairing_evo_rf),
  #   score_diff = weighted.mean(score_diff, toi),
  #   venue = weighted.mean(venue, toi),
  #   teammate_f_evo_rf = weighted.mean(teammate_f_evo_rf, toi),
  #   opp_d_evd_ra = weighted.mean(opp_d_evd_ra, toi),
  #   opp_f_evd_ra = weighted.mean(opp_f_evd_ra, toi)
  # ) |>
  dplyr::mutate(cf_per_60 = cf / (toi / 3600)) |>
  lm(
    formula =
      cf_per_60 ~
      # balanced +
      # league_cf_per_60 +
      score_diff +
      venue +
      pairing_evo_rf +
      teammate_f_evo_rf +
      opp_d_evd_ra +
      opp_f_evd_ra
  ) |>
  summary()

all_pairing_gamelogs |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_cf_per_60 = sum(cf) / sum(toi / 3600),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  # dplyr::filter(pairing |> stringr::str_detect("MIRO.HEISKANEN")) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(ca_per_60 = ca / (toi / 3600)) |>
  lm(
    formula =
      ca_per_60 ~
      balanced +
      # league_cf_per_60 +
      score_diff +
      venue +
      pairing_evd_ra +
      teammate_f_evd_ra +
      opp_d_evo_rf +
      opp_f_evo_rf +
      0
  ) |>
  summary()

all_pairing_gamelogs |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    # league_cf_per_60 = sum(cf) / sum(toi / 3600),
    league_xgf_per_60 = sum(xgf) / sum(toi / 3600),
    xg_per_corsi = sum(xgf) / sum(cf),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  # dplyr::filter(pairing |> stringr::str_detect("MIRO.HEISKANEN")) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    cf_per_60 = cf / (toi / 3600),
    x_xgf_per_60 = cf_per_60 * xg_per_corsi,
    xgf_per_60 = xgf / (toi / 3600)
  ) |>
  lm(
    formula =
      xgf_per_60 ~
      balanced +
      league_xgf_per_60 +
      x_xgf_per_60 +
      # (cf_per_60 * xg_per_corsi)+
      score_diff +
      venue +
      pairing_evo_qf +
      teammate_f_evo_qf +
      opp_d_evd_qa +
      opp_f_evd_qa +
      0
  ) |>
  summary()

all_pairing_gamelogs |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    league_xgf_per_60 = sum(xgf) / sum(toi / 3600),
    xg_per_corsi = sum(xgf) / sum(cf),
    venue = as.integer(venue == "home"),
    balanced = as.integer(balanced)
  ) |>
  # dplyr::filter(pairing |> stringr::str_detect("MIRO.HEISKANEN")) |>
  dplyr::filter(toi >= 200) |>
  dplyr::mutate(
    ca_per_60 = ca / (toi / 3600),
    x_xga_per_60 = ca_per_60 * xg_per_corsi,
    xg_per_corsi = sum(xgf) / sum(cf),
    xga_per_60 = xga / (toi / 3600)
  ) |>
  lm(
    formula =
      xga_per_60 ~
      balanced +
      league_xgf_per_60 +
      x_xga_per_60 +
      # ca_per_60 +
      score_diff +
      venue +
      pairing_evd_qa +
      teammate_f_evd_qa +
      opp_d_evo_qf +
      opp_f_evo_qf +
      0
  ) |>
  summary()

{
  pairing_shifts <-
    test_shifts |>
    dplyr::filter(game_strength_state == "5v5") |>
    tidyr::pivot_longer(
      c(home_on_1:home_on_7, away_on_1:away_on_7),
      names_to = "on_venue",
      values_to = "on"
    ) |>
    dplyr::filter(!is.na(on)) |>
    dplyr::transmute(
      game_id, game_seconds, shift_index, shift_length,
      venue = on_venue |> stringr::str_sub(end = 4),
      team = ifelse(venue == "home", home_team, away_team),
      on
    ) |>
    dplyr::left_join(skaters |> dplyr::filter(season == "23-24"), by = c("on", "team")) |>
    dplyr::filter(position == "D") |>
    dplyr::group_by(game_id, shift_index, team) |>
    dplyr::mutate(defensemen_on = dplyr::n()) |>
    dplyr::group_by(game_id, shift_index) |>
    dplyr::filter(all(defensemen_on == 2)) |>
    dplyr::left_join(
      readr::read_csv("../eh_pbp/eh_skaters_xgaa_all.csv") |>
        dplyr::transmute(
          on = EH_ID,
          season = Season,
          team = Team,
          position = ifelse(stringr::str_detect(Position, "D"), "D", "F"),
          evo_rf_rate = EV_RF / (TOI_EV / 60),
          evd_ra_rate = EV_RA / (TOI_EV / 60),
          evo_qf_rate = EV_QF / (TOI_EV / 60),
          evd_qa_rate = EV_QA / (TOI_EV / 60)
        ) |>
        dplyr::distinct() |>
        dplyr::filter(season == "23-24")
    ) |>
    dplyr::group_by(game_id, game_seconds, shift_index, shift_length, team, venue) |>
    dplyr::summarise(
      pairing = on |> sort() |> stringr::str_c(collapse = "-"),
      balanced = shoots |> unique() |> length() |> magrittr::equals(2),
      pairing_evo_rf = sum(evo_rf_rate),
      pairing_evd_ra = sum(evd_ra_rate),
      pairing_evo_qf = sum(evo_qf_rate),
      pairing_evd_qa = sum(evd_qa_rate),
      .groups = "drop"
    )

  score_diffs <-
    pairing_shifts |>
    dplyr::select(game_id, shift_index, venue) |>
    dplyr::left_join(
      test_shifts |>
        dplyr::transmute(
          game_id,
          shift_index,
          venue = "home",
          score_diff = home_score - away_score
        ) |>
        dplyr::bind_rows(
          test_shifts |>
            dplyr::transmute(
              game_id,
              shift_index,
              venue = "away",
              score_diff = away_score - home_score
            )
        )
    )

  on_ice_shots <-
    pairing_shifts |>
    dplyr::select(game_id, shift_index, venue) |>
    dplyr::left_join(
      test_shots |>
        dplyr::group_by(game_id, shift_index, venue = "home") |>
        dplyr::summarise(
          cf = sum(event_team == home_team, na.rm = T),
          ca = sum(event_team == away_team, na.rm = T),
          xgf = sum((event_team == home_team) * pred_goal, na.rm = T),
          xga = sum((event_team == away_team) * pred_goal, na.rm = T),
          .groups = "drop"
        ) |>
        dplyr::bind_rows(
          test_shots |>
            dplyr::group_by(game_id, shift_index, venue = "away") |>
            dplyr::summarise(
              cf = sum(event_team == away_team, na.rm = T),
              ca = sum(event_team == home_team, na.rm = T),
              xgf = sum((event_team == away_team) * pred_goal, na.rm = T),
              xga = sum((event_team == home_team) * pred_goal, na.rm = T),
              .groups = "drop"
            )
        )
    )

  qot <-
    pairing_shifts |>
    dplyr::select(game_id, shift_index, venue) |>
    dplyr::left_join(
      test_shifts |>
        dplyr::select(
          game_id, team = home_team, shift_index, home_on_1:home_on_7
        ) |>
        dplyr::mutate(venue = "home") |>
        tidyr::pivot_longer(home_on_1:home_on_7, values_to = "on") |>
        dplyr::bind_rows(
          test_shifts |>
            dplyr::select(
              game_id, team = away_team, shift_index, away_on_1:away_on_7
            ) |>
            dplyr::mutate(venue = "away") |>
            tidyr::pivot_longer(away_on_1:away_on_7, values_to = "on")
        ) |>
        dplyr::left_join(
          readr::read_csv("../eh_pbp/eh_skaters_xgaa_all.csv") |>
            dplyr::transmute(
              on = EH_ID,
              season = Season,
              team = Team,
              position = ifelse(stringr::str_detect(Position, "D"), "D", "F"),
              evo_rf_rate = EV_RF / (TOI_EV / 60),
              evd_ra_rate = EV_RA / (TOI_EV / 60),
              evo_qf_rate = EV_QF / (TOI_EV / 60),
              evd_qa_rate = EV_QA / (TOI_EV / 60)
            ) |>
            dplyr::distinct() |>
            dplyr::filter(season == "23-24")
        ) |>
        dplyr::filter(position == "F") |>
        dplyr::group_by(game_id, venue, shift_index) |>
        dplyr::summarise(
          teammate_evo_rf = sum(evo_rf_rate),
          teammate_evd_ra = sum(evd_ra_rate),
          teammate_evo_qf = sum(evo_qf_rate),
          teammate_evd_qa = sum(evd_qa_rate),
          .groups = "drop"
        )
    )

  qoc <-
    pairing_shifts |>
    dplyr::select(game_id, shift_index, venue) |>
    dplyr::left_join(
      test_shifts |>
        dplyr::select(
          game_id, team = away_team, shift_index, away_on_1:away_on_7
        ) |>
        dplyr::mutate(venue = "home") |>
        tidyr::pivot_longer(away_on_1:away_on_7, values_to = "on") |>
        dplyr::bind_rows(
          test_shifts |>
            dplyr::select(
              game_id, team = home_team, shift_index, home_on_1:home_on_7
            ) |>
            dplyr::mutate(venue = "away") |>
            tidyr::pivot_longer(home_on_1:home_on_7, values_to = "on")
        ) |>
        dplyr::left_join(
          readr::read_csv("../eh_pbp/eh_skaters_xgaa_all.csv") |>
            dplyr::transmute(
              on = EH_ID,
              season = Season,
              team = Team,
              position = ifelse(stringr::str_detect(Position, "D"), "D", "F"),
              evo_rf_rate = EV_RF / (TOI_EV / 60),
              evd_ra_rate = EV_RA / (TOI_EV / 60),
              evo_qf_rate = EV_QF / (TOI_EV / 60),
              evd_qa_rate = EV_QA / (TOI_EV / 60)
            ) |>
            dplyr::distinct() |>
            dplyr::filter(season == "23-24")
        ) |>
        dplyr::filter(!is.na(position)) |>
        dplyr::group_by(game_id, venue, shift_index) |>
        dplyr::summarise(
          opp_f_evo_rf = sum(evo_rf_rate * (position == "F"), na.rm = T),
          opp_f_evd_ra = sum(evd_ra_rate * (position == "F"), na.rm = T),
          opp_f_evo_qf = sum(evo_qf_rate * (position == "F"), na.rm = T),
          opp_f_evd_qa = sum(evd_qa_rate * (position == "F"), na.rm = T),
          opp_d_evo_rf = sum(evo_rf_rate * (position == "D"), na.rm = T),
          opp_d_evd_ra = sum(evd_ra_rate * (position == "D"), na.rm = T),
          opp_d_evo_qf = sum(evo_qf_rate * (position == "D"), na.rm = T),
          opp_d_evd_qa = sum(evd_qa_rate * (position == "D"), na.rm = T),
          .groups = "drop"
        )
    )

  pairing_shifts |>
    dplyr::left_join(score_diffs) |>
    dplyr::left_join(on_ice_shots) |>
    dplyr::left_join(qot) |>
    dplyr::left_join(qoc)
} |>
  dplyr::group_by(
    game_id, team, venue, pairing, balanced
  ) |>
  dplyr::summarise(
    toi = sum(shift_length),
    score_diff = weighted.mean(score_diff, shift_length),
    cf = sum(cf, na.rm = T),
    ca = sum(ca, na.rm = T),
    xgf = sum(xgf, na.rm = T),
    xga = sum(xga, na.rm = T),
    pairing_evo_rf = mean(pairing_evo_rf),
    pairing_evd_ra = mean(pairing_evd_ra),
    pairing_evo_qf = mean(pairing_evo_qf),
    pairing_evd_qa = mean(pairing_evd_qa),
    teammate_f_evo_rf = weighted.mean(teammate_evo_rf, shift_length),
    teammate_f_evd_ra = weighted.mean(teammate_evd_ra, shift_length),
    teammate_f_evo_qf = weighted.mean(teammate_evo_qf, shift_length),
    teammate_f_evd_qa = weighted.mean(teammate_evd_qa, shift_length),
    opp_f_evo_rf = weighted.mean(opp_f_evo_rf, shift_length),
    opp_f_evd_ra = weighted.mean(opp_f_evd_ra, shift_length),
    opp_f_evo_qf = weighted.mean(opp_f_evo_qf, shift_length),
    opp_f_evd_qa = weighted.mean(opp_f_evd_qa, shift_length),
    opp_d_evo_rf = weighted.mean(opp_d_evo_rf, shift_length),
    opp_d_evd_ra = weighted.mean(opp_d_evd_ra, shift_length),
    opp_d_evo_qf = weighted.mean(opp_d_evo_qf, shift_length),
    opp_d_evd_qa = weighted.mean(opp_d_evd_qa, shift_length),
    .groups = "drop"
  ) |>
  dplyr::filter(toi >= 200) |>
  dplyr::arrange(dplyr::desc(toi)) |>
  View()













