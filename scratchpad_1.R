individual_player_xgar <-
  readr::read_csv("../eh_pbp/eh_skaters_xgaa_all.csv") |>
  dplyr::filter(stringr::str_detect(Position, "D")) |>
  dplyr::group_by(
    eh_id = EH_ID,
    season = Season
  ) |>
  dplyr::summarise(
    toi = TOI_EV |> sum(),
    evo_rf = sum(EV_RF),
    evd_ra = sum(EV_RA),
    evo_qf = sum(EV_QF),
    evd_qa = sum(EV_QA),
    .groups = "drop") |>
  dplyr::group_by(eh_id) |>
  dplyr::mutate(
    prev_2_season_toi =
      zoo::rollsum(
        toi, 2, fill = NA, align = "right"
      ) |>
      dplyr::lag(),
    prev_2_season_evo_rf =
      zoo::rollsum(
        evo_rf, 2, fill = NA, align = "right"
      ) |>
      dplyr::lag(),
    prev_2_season_evd_ra=
      zoo::rollsum(
        evd_ra, 2, fill = NA, align = "right"
      ) |>
      dplyr::lag(),
    prev_2_season_evo_qf =
      zoo::rollsum(
        evo_qf, 2, fill = NA, align = "right"
      ) |>
      dplyr::lag(),
    prev_2_season_evd_qa =
      zoo::rollsum(
        evd_qa, 2, fill = NA, align = "right"
      ) |>
      dplyr::lag(),
  ) |>
  dplyr::ungroup()

stringr::str_c(
  "^",
  individual_player_xgar$eh_id |>
    unique() |>
    stringr::str_c("-")
) |>
  stringr::str_c(collapse = "|")

players_balanced_pairing_time <-
  all_pairing_gamelogs |>
  dplyr::group_by(season, pairing, balanced) |>
  dplyr::summarise(toi = sum(toi), .groups = "drop") |>
  dplyr::distinct() |>
  dplyr::group_by(pairing) |>
  dplyr::mutate(
    pairing_player_1 =
      stringr::str_extract(
        pairing,
        stringr::str_c(
          "^",
          individual_player_xgar$eh_id |>
            unique() |>
            stringr::str_c("-")
        ) |>
          stringr::str_c(collapse = "|")
      ) |>
      stringr::str_remove("-$"),
    pairing_player_2 =
      stringr::str_extract(
        pairing,
        stringr::str_c(
          "-",
          individual_player_xgar$eh_id |>
            unique() |>
            stringr::str_c("$")
        ) |>
          stringr::str_c(collapse = "|")
      ) |>
      stringr::str_remove("-")
  ) |>
  tidyr::pivot_longer(c(pairing_player_1, pairing_player_2)) |>
  dplyr::group_by(season, value) |>
  dplyr::summarise(
    perc_balanced_pairing = weighted.mean(as.double(balanced), toi),
    toi = sum(toi),
    .groups = "drop"
  )

players_balanced_pairing_time <-
  players_balanced_pairing_time |>
  dplyr::arrange(value, season) |>
  dplyr::group_by(value) |>
  dplyr::mutate(
    prev_2_year_balanced_time =
      ((dplyr::lag(perc_balanced_pairing, 2)*dplyr::lag(toi, 2)) +
      (dplyr::lag(perc_balanced_pairing)*dplyr::lag(toi))) /
      (dplyr::lag(toi, 2) + dplyr::lag(toi))
  ) |>
  dplyr::ungroup()

pairing_2_year_history <-
  all_pairing_gamelogs |>
  dplyr::filter(toi >= 200) |>
  dplyr::filter(season >= "11-12") |>
  dplyr::group_by(season, pairing, balanced, pairing_evo_rf, pairing_evd_ra, pairing_evo_qf, pairing_evd_qa) |>
  dplyr::summarise(toi = sum(toi), .groups = "drop") |>
  dplyr::distinct() |>
  dplyr::group_by(pairing) |>
  dplyr::mutate(
    pairing_2_year_evo_rf =
      ((pairing_evo_rf * toi) + (dplyr::lead(pairing_evo_rf) * dplyr::lead(toi))) /
      (toi + dplyr::lead(toi)),
    pairing_2_year_evd_ra =
      ((pairing_evd_ra * toi) + (dplyr::lead(pairing_evd_ra) * dplyr::lead(toi))) /
      (toi + dplyr::lead(toi)),
    pairing_2_year_evo_qf =
      ((pairing_evo_qf * toi) + (dplyr::lead(pairing_evo_qf) * dplyr::lead(toi))) /
      (toi + dplyr::lead(toi)),
    pairing_2_year_evd_qa =
      ((pairing_evd_qa * toi) + (dplyr::lead(pairing_evd_qa) * dplyr::lead(toi))) /
      (toi + dplyr::lead(toi)),
    pairing_player_1 =
      stringr::str_extract(
        pairing,
        stringr::str_c(
          "^",
          individual_player_xgar$eh_id |>
            unique() |>
            stringr::str_c("-")
        ) |>
          stringr::str_c(collapse = "|")
      ),
    pairing_player_2 =
      stringr::str_extract(
        pairing,
        stringr::str_c(
          "-",
          individual_player_xgar$eh_id |>
            unique() |>
            stringr::str_c("$")
        ) |>
          stringr::str_c(collapse = "|")
      )
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    individual_player_xgar |>
      dplyr::transmute(
        pairing_player_1 = eh_id |> stringr::str_c("-"),
        season,
        player_1_prev_2_evo_rf_rate = prev_2_season_evo_rf / (prev_2_season_toi / 60),
        player_1_prev_2_evd_ra_rate = prev_2_season_evd_ra / (prev_2_season_toi / 60),
        player_1_prev_2_evo_qf_rate = prev_2_season_evo_qf / (prev_2_season_toi / 60),
        player_1_prev_2_evd_qa_rate = prev_2_season_evd_qa / (prev_2_season_toi / 60)
      )
  ) |>
  dplyr::left_join(
    individual_player_xgar |>
      dplyr::transmute(
        pairing_player_2 = "-" |> stringr::str_c(eh_id),
        season,
        player_2_prev_2_evo_rf_rate = prev_2_season_evo_rf / (prev_2_season_toi / 60),
        player_2_prev_2_evd_ra_rate = prev_2_season_evd_ra / (prev_2_season_toi / 60),
        player_2_prev_2_evo_qf_rate = prev_2_season_evo_qf / (prev_2_season_toi / 60),
        player_2_prev_2_evd_qa_rate = prev_2_season_evd_qa / (prev_2_season_toi / 60)
      )
  ) |>
  dplyr::group_by(pairing) |>
  dplyr::filter(
    season == min(season),
    !is.na(pairing_2_year_evo_rf) &
      !is.na(player_1_prev_2_evo_rf_rate) &
      !is.na(player_2_prev_2_evo_rf_rate)
  ) |>
  dplyr::ungroup()



pairing_2_year_history <-
  pairing_2_year_history |>
  dplyr::left_join(
    players_balanced_pairing_time |>
      dplyr::transmute(
        pairing_player_1 = value |> stringr::str_c("-"),
        season,
        player_1_prev_2_year_balanced_time = prev_2_year_balanced_time
      )
  ) |>
  dplyr::left_join(
    players_balanced_pairing_time |>
      dplyr::transmute(
        pairing_player_2 =  "-" |> stringr::str_c(value),
        season,
        player_2_prev_2_year_balanced_time = prev_2_year_balanced_time
      )
  )



lm(
  formula =
    pairing_2_year_evo_rf ~
    (player_1_prev_2_evo_rf_rate * player_1_prev_2_year_balanced_time) +
    (player_2_prev_2_evo_rf_rate * player_2_prev_2_year_balanced_time) +
    # player_1_prev_2_year_balanced_time +
    # player_2_prev_2_year_balanced_time +
    balanced,
  data =
    pairing_2_year_history |>
    dplyr::filter(toi >= 3000)
    # dplyr::filter(season > "15-16")
) |>
  summary()

lm(
  formula =
    pairing_2_year_evd_ra ~
    (player_1_prev_2_evd_ra_rate * player_1_prev_2_year_balanced_time) +
    (player_2_prev_2_evd_ra_rate * player_2_prev_2_year_balanced_time) +
    # player_1_prev_2_evd_ra_rate +
    # player_2_prev_2_evd_ra_rate +
    # player_1_prev_2_year_balanced_time +
    # player_2_prev_2_year_balanced_time +
    balanced,
  data =
    pairing_2_year_history |>
  dplyr::filter(season > "15-16")
) |>
  summary()

lm(
  formula =
    pairing_2_year_evo_qf ~
    (player_1_prev_2_evo_qf_rate * player_1_prev_2_year_balanced_time) +
    (player_2_prev_2_evo_qf_rate * player_2_prev_2_year_balanced_time) +
    # player_1_prev_2_evo_qf_rate +
    # player_2_prev_2_evo_qf_rate +
    # player_1_prev_2_year_balanced_time +
    # player_2_prev_2_year_balanced_time +
    balanced,
  data =
    pairing_2_year_history |>
  dplyr::filter(season <= "15-16")
) |>
  summary()

lm(
  formula =
    pairing_2_year_evd_qa ~
    (player_1_prev_2_evd_qa_rate * player_1_prev_2_year_balanced_time) +
    (player_2_prev_2_evd_qa_rate * player_2_prev_2_year_balanced_time) +
    # player_1_prev_2_evd_qa_rate +
    # player_2_prev_2_evd_qa_rate +
    # player_1_prev_2_year_balanced_time +
    # player_2_prev_2_year_balanced_time +
    balanced,
  data =
    pairing_2_year_history
  # dplyr::filter(season >= "15-16")
) |>
  summary()
