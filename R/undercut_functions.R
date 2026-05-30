`%>%` <- magrittr::`%>%`

python_has_fastf1 <- function(python) {
  if (!nzchar(python) || !file.exists(python)) {
    return(FALSE)
  }

  status <- suppressWarnings(system2(
    python,
    c("-c", "import fastf1"),
    stdout = FALSE,
    stderr = FALSE
  ))

  identical(status, 0L)
}

configure_fastf1_python <- function(python = Sys.getenv("RETICULATE_PYTHON", unset = "")) {
  candidates <- unique(c(
    python,
    "/opt/anaconda3/bin/python3",
    Sys.which("python3"),
    "/usr/bin/python3"
  ))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]

  if (!length(candidates)) {
    stop("No Python executable was found. Set RETICULATE_PYTHON before running the analysis.")
  }

  preferred <- purrr::keep(candidates, python_has_fastf1)
  selected <- if (length(preferred)) preferred[[1]] else candidates[[1]]

  if (reticulate::py_available(initialize = FALSE)) {
    current <- tryCatch(reticulate::py_config()$python, error = function(e) "")
    if (nzchar(current) && normalizePath(current) != normalizePath(selected)) {
      stop(
        "reticulate is already initialized with ", current, ". ",
        "Restart the R session, then rerun the notebook so it can bind to ",
        selected, "."
      )
    }
  }

  Sys.setenv(RETICULATE_PYTHON = selected)
  reticulate::use_python(selected, required = TRUE)
  invisible(selected)
}

assert_fastf1_available <- function(python = Sys.getenv("RETICULATE_PYTHON", unset = "")) {
  if (!nzchar(python) || !file.exists(python)) {
    stop("RETICULATE_PYTHON is not set to a valid Python executable.")
  }

  Sys.setenv(RETICULATE_PYTHON = python)
  reticulate::py_config()

  if (!reticulate::py_module_available("fastf1")) {
    stop(
      "FastF1 is not available in ", python,
      ". Install it with `python3 -m pip install fastf1`."
    )
  }

  invisible(reticulate::import("fastf1")$`__version__`)
}

is_green_flag_lap <- function(track_status) {
  stringr::str_detect(dplyr::coalesce(track_status, ""), "^1+$")
}

is_normal_classification_status <- function(status) {
  status <- stringr::str_trim(dplyr::coalesce(status, ""))
  status == "Finished" | stringr::str_detect(status, "^\\+[0-9]+\\s+Lap")
}

load_schedule_cached <- function(season, cache_dir = "cache/schedules") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("season_%s_schedule.rds", season))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  schedule <- f1dataR::load_schedule(season)
  saveRDS(schedule, cache_file)
  schedule
}

load_schedule_table <- function(seasons, cache_dir = "cache/schedules") {
  purrr::map_dfr(
    seasons,
    function(season) {
      load_schedule_cached(season = season, cache_dir = cache_dir) %>%
        dplyr::mutate(
          season = as.integer(season),
          round = as.integer(round),
          date = as.Date(date)
        )
    }
  )
}

load_drivers_cached <- function(season, cache_dir = "cache/drivers") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("season_%s_drivers.rds", season))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  drivers <- f1dataR::load_drivers(season)
  saveRDS(drivers, cache_file)
  drivers
}

load_results_cached <- function(season, round, cache_dir = "cache/results") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("season_%s_round_%02d_results.rds", season, round))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  results <- f1dataR::load_results(season = season, round = round)
  saveRDS(results, cache_file)
  results
}

load_pitstops_cached <- function(season, round, cache_dir = "cache/pitstops") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("season_%s_round_%02d_pitstops.rds", season, round))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  pitstops <- f1dataR::load_pitstops(season = season, round = round)
  saveRDS(pitstops, cache_file)
  pitstops
}

load_laps_cached <- function(season, round, cache_dir = "cache/laps", log_level = "WARNING") {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("season_%s_round_%02d_race_laps.rds", season, round))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  laps <- f1dataR::load_session_laps(
    season = season,
    round = round,
    session = "R",
    log_level = log_level
  )

  saveRDS(laps, cache_file)
  laps
}

prepare_race_laps <- function(laps) {
  laps %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      lap_number = as.integer(lap_number),
      stint = as.integer(stint),
      position = as.integer(position),
      time = as.numeric(time),
      lap_time = as.numeric(lap_time),
      pit_in_time = as.numeric(pit_in_time),
      pit_out_time = as.numeric(pit_out_time),
      tyre_life = as.numeric(tyre_life),
      deleted = dplyr::coalesce(deleted, FALSE),
      has_pit_in = is.finite(pit_in_time),
      has_pit_out = is.finite(pit_out_time),
      is_green_flag = is_green_flag_lap(track_status)
    ) %>%
    dplyr::filter(.data$session_type == "R", !.data$deleted) %>%
    dplyr::arrange(.data$driver, .data$lap_number)
}

extract_stop_table <- function(laps) {
  previous_lap <- laps %>%
    dplyr::transmute(
      driver,
      stop_lap = .data$lap_number + 1L,
      pre_lap_number = .data$lap_number,
      pre_position = .data$position,
      pre_time = .data$time,
      pre_compound = .data$compound,
      pre_tyre_life = .data$tyre_life,
      pre_track_status = .data$track_status,
      pre_green_flag = .data$is_green_flag
    )

  out_lap <- laps %>%
    dplyr::filter(.data$has_pit_out) %>%
    dplyr::transmute(
      driver,
      stop_lap = .data$lap_number - 1L,
      out_lap_number = .data$lap_number,
      out_position = .data$position,
      out_time = .data$time,
      out_compound = .data$compound,
      out_tyre_life = .data$tyre_life,
      out_track_status = .data$track_status,
      out_green_flag = .data$is_green_flag,
      pit_out_time = .data$pit_out_time
    )

  laps %>%
    dplyr::filter(.data$has_pit_in) %>%
    dplyr::arrange(.data$driver, .data$lap_number) %>%
    dplyr::group_by(.data$driver) %>%
    dplyr::mutate(stop_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      driver,
      team,
      stop_number,
      stop_lap = .data$lap_number,
      stop_stint = .data$stint,
      inlap_position = .data$position,
      inlap_time = .data$time,
      pit_in_time = .data$pit_in_time,
      inlap_track_status = .data$track_status,
      inlap_green_flag = .data$is_green_flag
    ) %>%
    dplyr::left_join(previous_lap, by = c("driver", "stop_lap")) %>%
    dplyr::left_join(out_lap, by = c("driver", "stop_lap")) %>%
    dplyr::group_by(.data$driver) %>%
    dplyr::arrange(.data$stop_lap, .by_group = TRUE) %>%
    dplyr::mutate(
      previous_stop_lap = dplyr::lag(.data$stop_lap),
      next_stop_lap = dplyr::lead(.data$stop_lap)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stop_duration = .data$pit_out_time - .data$pit_in_time,
      green_stop_window = .data$pre_green_flag & .data$inlap_green_flag & .data$out_green_flag
    )
}

build_driver_status_table <- function(season, results = NULL, drivers = NULL) {
  if (is.null(results) || is.null(drivers)) {
    return(NULL)
  }

  drivers %>%
    janitor::clean_names() %>%
    dplyr::select(driver_id, code) %>%
    dplyr::inner_join(
      results %>%
        janitor::clean_names() %>%
        dplyr::select(driver_id, status),
      by = "driver_id"
    ) %>%
    dplyr::transmute(
      season = as.integer(season),
      driver = .data$code,
      finish_status = .data$status,
      normal_finish_status = is_normal_classification_status(.data$status)
    )
}

build_driver_lap_quality_table <- function(race_laps, damage_lap_buffer_sec = 20) {
  baseline <- race_laps %>%
    dplyr::filter(
      .data$is_green_flag,
      .data$is_accurate,
      is.finite(.data$lap_time),
      !.data$has_pit_in,
      !.data$has_pit_out
    ) %>%
    dplyr::group_by(.data$driver) %>%
    dplyr::summarise(
      baseline_lap_time = stats::median(.data$lap_time, na.rm = TRUE),
      .groups = "drop"
    )

  race_laps %>%
    dplyr::left_join(baseline, by = "driver") %>%
    dplyr::mutate(
      broken_lap = dplyr::case_when(
        !is.finite(.data$time) ~ TRUE,
        .data$has_pit_in ~ TRUE,
        .data$has_pit_out ~ TRUE,
        !dplyr::coalesce(.data$is_accurate, FALSE) ~ TRUE,
        !is.finite(.data$lap_time) ~ TRUE,
        !is.na(.data$baseline_lap_time) &
          .data$lap_time > (.data$baseline_lap_time + damage_lap_buffer_sec) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select(.data$driver, .data$lap_number, .data$broken_lap)
}

identify_undercut_events <- function(laps,
                                     season,
                                     round,
                                     race_name,
                                     circuit_name,
                                     results = NULL,
                                     drivers = NULL,
                                     max_pre_gap = 5,
                                     max_stop_lap_delta = 4,
                                     settle_laps = 2,
                                     green_flag_only = TRUE,
                                     extra_stop_buffer_laps = 2,
                                     max_stop_duration_sec = 35,
                                     damage_lap_buffer_sec = 20) {
  race_laps <- prepare_race_laps(laps)
  stops <- extract_stop_table(race_laps)
  status_tbl <- build_driver_status_table(season = season, results = results, drivers = drivers)
  lap_quality_tbl <- build_driver_lap_quality_table(
    race_laps = race_laps,
    damage_lap_buffer_sec = damage_lap_buffer_sec
  )

  snapshots <- race_laps %>%
    dplyr::transmute(
      driver,
      lap_number,
      position,
      time,
      compound,
      tyre_life,
      track_status,
      is_green_flag
    )

  rival_pre_snapshot <- snapshots %>%
    dplyr::transmute(
      pre_lap_number = .data$lap_number,
      rival_driver = .data$driver,
      rival_pre_position = .data$position,
      rival_pre_time = .data$time,
      rival_pre_compound = .data$compound,
      rival_pre_tyre_life = .data$tyre_life,
      rival_pre_track_status = .data$track_status,
      rival_pre_green_flag = .data$is_green_flag
    )

  rival_stop_table <- stops %>%
    dplyr::transmute(
      rival_driver = .data$driver,
      rival_team = .data$team,
      rival_stop_number = .data$stop_number,
      rival_stop_lap = .data$stop_lap,
      rival_stop_stint = .data$stop_stint,
      rival_pit_in_time = .data$pit_in_time,
      rival_pit_out_time = .data$pit_out_time,
      rival_stop_duration = .data$stop_duration,
      rival_green_stop_window = .data$green_stop_window,
      rival_previous_stop_lap = .data$previous_stop_lap,
      rival_next_stop_lap = .data$next_stop_lap,
      rival_out_compound = .data$out_compound,
      rival_out_tyre_life = .data$out_tyre_life
    )

  evaluation_snapshot <- snapshots %>%
    dplyr::transmute(
      driver,
      evaluation_lap = .data$lap_number,
      eval_position = .data$position,
      eval_time = .data$time,
      eval_track_status = .data$track_status,
      eval_green_flag = .data$is_green_flag
    )

  events <- stops %>%
    dplyr::filter(!is.na(.data$pre_position), !is.na(.data$pre_time), .data$pre_position > 1L) %>%
    dplyr::mutate(rival_pre_position = .data$pre_position - 1L) %>%
    dplyr::left_join(
      rival_pre_snapshot,
      by = c("pre_lap_number", "rival_pre_position")
    ) %>%
    dplyr::filter(!is.na(.data$rival_driver), .data$rival_driver != .data$driver) %>%
    dplyr::left_join(rival_stop_table, by = "rival_driver") %>%
    dplyr::filter(
      .data$rival_stop_lap >= .data$stop_lap + 1L,
      .data$rival_stop_lap <= .data$stop_lap + max_stop_lap_delta
    ) %>%
    dplyr::group_by(.data$driver, .data$stop_number) %>%
    dplyr::slice_min(.data$rival_stop_lap, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(evaluation_lap = .data$rival_stop_lap + settle_laps) %>%
    dplyr::left_join(
      evaluation_snapshot,
      by = c("driver", "evaluation_lap")
    ) %>%
    dplyr::rename(
      first_eval_position = .data$eval_position,
      first_eval_time = .data$eval_time,
      first_eval_track_status = .data$eval_track_status,
      first_eval_green_flag = .data$eval_green_flag
    ) %>%
    dplyr::left_join(
      evaluation_snapshot,
      by = c("rival_driver" = "driver", "evaluation_lap")
    ) %>%
    dplyr::rename(
      rival_eval_position = .data$eval_position,
      rival_eval_time = .data$eval_time,
      rival_eval_track_status = .data$eval_track_status,
      rival_eval_green_flag = .data$eval_green_flag
    )

  if (!is.null(status_tbl)) {
    events <- events %>%
      dplyr::left_join(
        status_tbl %>%
          dplyr::select("driver", pit_first_finish_status = "finish_status", pit_first_normal_finish = "normal_finish_status"),
        by = c("driver")
      ) %>%
      dplyr::left_join(
        status_tbl %>%
          dplyr::select("driver", rival_finish_status = "finish_status", rival_normal_finish = "normal_finish_status"),
        by = c("rival_driver" = "driver")
      )
  } else {
    events <- events %>%
      dplyr::mutate(
        pit_first_finish_status = NA_character_,
        pit_first_normal_finish = NA,
        rival_finish_status = NA_character_,
        rival_normal_finish = NA
      )
  }

  events <- events %>%
    dplyr::mutate(
      season = as.integer(season),
      round = as.integer(round),
      race_name = race_name,
      circuit_name = circuit_name,
      gap_before_sec = .data$pre_time - .data$rival_pre_time,
      gap_after_sec = .data$first_eval_time - .data$rival_eval_time,
      undercut_gain_sec = .data$gap_before_sec - .data$gap_after_sec,
      stop_lap_delta = .data$rival_stop_lap - .data$stop_lap,
      position_gain = .data$pre_position - .data$first_eval_position,
      pit_first_close_extra_stop = (
        (!is.na(.data$previous_stop_lap) & (.data$stop_lap - .data$previous_stop_lap) <= extra_stop_buffer_laps) |
          (!is.na(.data$next_stop_lap) & (.data$next_stop_lap - .data$stop_lap) <= extra_stop_buffer_laps)
      ),
      rival_close_extra_stop = (
        (!is.na(.data$rival_previous_stop_lap) & (.data$rival_stop_lap - .data$rival_previous_stop_lap) <= extra_stop_buffer_laps) |
          (!is.na(.data$rival_next_stop_lap) & (.data$rival_next_stop_lap - .data$rival_stop_lap) <= extra_stop_buffer_laps)
      ),
      jumped_rival = .data$first_eval_position < .data$rival_eval_position,
      undercut_succeeded = .data$gap_after_sec < 0 | .data$jumped_rival
    ) %>%
    dplyr::filter(
      is.finite(.data$gap_before_sec),
      is.finite(.data$gap_after_sec),
      .data$gap_before_sec > 0,
      .data$gap_before_sec <= max_pre_gap,
      !is.na(.data$first_eval_position),
      !is.na(.data$rival_eval_position),
      !.data$pit_first_close_extra_stop,
      !.data$rival_close_extra_stop,
      is.finite(.data$stop_duration),
      is.finite(.data$rival_stop_duration),
      .data$stop_duration <= max_stop_duration_sec,
      .data$rival_stop_duration <= max_stop_duration_sec
    )

  if (!is.null(status_tbl)) {
    events <- events %>%
      dplyr::filter(
        dplyr::coalesce(.data$pit_first_normal_finish, FALSE),
        dplyr::coalesce(.data$rival_normal_finish, FALSE)
      )
  }

  if (green_flag_only) {
    events <- events %>%
      dplyr::filter(
        .data$pre_green_flag,
        .data$rival_pre_green_flag,
        .data$green_stop_window,
        .data$rival_green_stop_window,
        .data$first_eval_green_flag,
        .data$rival_eval_green_flag
      )
  }

  clean_window <- function(driver, start_lap, end_lap) {
    if (length(driver) != 1L || length(start_lap) != 1L || length(end_lap) != 1L) {
      return(FALSE)
    }

    if (!nzchar(dplyr::coalesce(as.character(driver), ""))) {
      return(FALSE)
    }

    if (anyNA(c(start_lap, end_lap)) || !is.finite(start_lap) || !is.finite(end_lap)) {
      return(FALSE)
    }

    if (end_lap < start_lap) {
      return(FALSE)
    }

    window <- lap_quality_tbl %>%
      dplyr::filter(
        .data$driver == .env$driver,
        .data$lap_number >= .env$start_lap,
        .data$lap_number <= .env$end_lap
      )

    expected_rows <- end_lap - start_lap + 1L
    isTRUE(nrow(window) == expected_rows) && !isTRUE(any(window$broken_lap))
  }

  events <- events %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pit_first_clean_window = clean_window(.data$driver, .data$out_lap_number + 1L, .data$evaluation_lap),
      rival_clean_window = clean_window(.data$rival_driver, .data$rival_stop_lap + 2L, .data$evaluation_lap)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$pit_first_clean_window, .data$rival_clean_window)

  events %>%
    dplyr::select(
      .data$season,
      .data$round,
      .data$race_name,
      .data$circuit_name,
      pit_first_driver = .data$driver,
      pit_first_team = .data$team,
      .data$stop_number,
      .data$stop_lap,
      .data$pre_lap_number,
      .data$pre_position,
      .data$pre_time,
      .data$pre_compound,
      .data$pre_tyre_life,
      .data$out_compound,
      .data$out_tyre_life,
      .data$stop_duration,
      rival_driver = .data$rival_driver,
      rival_team = .data$rival_team,
      .data$rival_stop_number,
      .data$rival_stop_lap,
      .data$rival_pre_position,
      .data$rival_pre_time,
      .data$rival_pre_compound,
      .data$rival_pre_tyre_life,
      .data$rival_out_compound,
      .data$rival_out_tyre_life,
      .data$rival_stop_duration,
      .data$evaluation_lap,
      .data$first_eval_position,
      .data$rival_eval_position,
      .data$gap_before_sec,
      .data$gap_after_sec,
      .data$undercut_gain_sec,
      .data$position_gain,
      .data$stop_lap_delta,
      pit_first_finish_status = .data$pit_first_finish_status,
      rival_finish_status = .data$rival_finish_status,
      .data$jumped_rival,
      .data$undercut_succeeded
    ) %>%
    dplyr::arrange(.data$season, .data$round, dplyr::desc(.data$undercut_gain_sec))
}

run_race_analysis <- function(season,
                              round,
                              race_name,
                              circuit_name,
                              cache_dir = "cache/laps",
                              results_cache_dir = "cache/results",
                              drivers_cache_dir = "cache/drivers",
                              max_pre_gap = 5,
                              max_stop_lap_delta = 4,
                              settle_laps = 2,
                              green_flag_only = TRUE,
                              extra_stop_buffer_laps = 2,
                              max_stop_duration_sec = 35,
                              damage_lap_buffer_sec = 20,
                              log_level = "WARNING") {
  laps <- load_laps_cached(
    season = season,
    round = round,
    cache_dir = cache_dir,
    log_level = log_level
  )
  results <- load_results_cached(season = season, round = round, cache_dir = results_cache_dir)
  drivers <- load_drivers_cached(season = season, cache_dir = drivers_cache_dir)

  identify_undercut_events(
    laps = laps,
    season = season,
    round = round,
    race_name = race_name,
    circuit_name = circuit_name,
    results = results,
    drivers = drivers,
    max_pre_gap = max_pre_gap,
    max_stop_lap_delta = max_stop_lap_delta,
    settle_laps = settle_laps,
    green_flag_only = green_flag_only,
    extra_stop_buffer_laps = extra_stop_buffer_laps,
    max_stop_duration_sec = max_stop_duration_sec,
    damage_lap_buffer_sec = damage_lap_buffer_sec
  )
}

run_schedule_analysis <- function(schedule_tbl,
                                  cache_dir = "cache/laps",
                                  results_cache_dir = "cache/results",
                                  drivers_cache_dir = "cache/drivers",
                                  max_pre_gap = 5,
                                  max_stop_lap_delta = 4,
                                  settle_laps = 2,
                                  green_flag_only = TRUE,
                                  extra_stop_buffer_laps = 2,
                                  max_stop_duration_sec = 35,
                                  damage_lap_buffer_sec = 20,
                                  log_level = "WARNING") {
  results <- purrr::pmap(
    schedule_tbl,
    function(season, round, race_name, circuit_name, ...) {
      tryCatch(
        list(
          events = run_race_analysis(
            season = season,
            round = round,
            race_name = race_name,
            circuit_name = circuit_name,
            cache_dir = cache_dir,
            results_cache_dir = results_cache_dir,
            drivers_cache_dir = drivers_cache_dir,
            max_pre_gap = max_pre_gap,
            max_stop_lap_delta = max_stop_lap_delta,
            settle_laps = settle_laps,
            green_flag_only = green_flag_only,
            extra_stop_buffer_laps = extra_stop_buffer_laps,
            max_stop_duration_sec = max_stop_duration_sec,
            damage_lap_buffer_sec = damage_lap_buffer_sec,
            log_level = log_level
          ),
          error = NULL
        ),
        error = function(e) {
          list(
            events = NULL,
            error = tibble::tibble(
              season = as.integer(season),
              round = as.integer(round),
              race_name = race_name,
              circuit_name = circuit_name,
              error = conditionMessage(e)
            )
          )
        }
      )
    }
  )

  list(
    events = dplyr::bind_rows(purrr::map(results, "events")),
    failures = dplyr::bind_rows(purrr::map(results, "error"))
  )
}

summarize_track_undercuts <- function(events) {
  events %>%
    dplyr::group_by(.data$circuit_name) %>%
    dplyr::summarise(
      attempts = dplyr::n(),
      success_rate = mean(.data$undercut_succeeded, na.rm = TRUE),
      mean_gain_sec = mean(.data$undercut_gain_sec, na.rm = TRUE),
      median_gain_sec = stats::median(.data$undercut_gain_sec, na.rm = TRUE),
      mean_position_gain = mean(.data$position_gain, na.rm = TRUE),
      mean_stop_lap_delta = mean(.data$stop_lap_delta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$mean_gain_sec))
}

derive_overcut_events <- function(events) {
  events %>%
    dplyr::transmute(
      season = .data$season,
      round = .data$round,
      race_name = .data$race_name,
      circuit_name = .data$circuit_name,
      overcut_driver = .data$rival_driver,
      overcut_team = .data$rival_team,
      rival_driver = .data$pit_first_driver,
      rival_team = .data$pit_first_team,
      first_stop_lap = .data$stop_lap,
      delayed_stop_lap = .data$rival_stop_lap,
      gap_before_sec = .data$gap_before_sec,
      gap_after_sec = .data$gap_after_sec,
      overcut_gain_sec = .data$gap_after_sec - .data$gap_before_sec,
      overcut_succeeded = (.data$gap_after_sec > 0) | (!.data$jumped_rival),
      rival_finish_status = .data$pit_first_finish_status,
      overcut_finish_status = .data$rival_finish_status
    ) %>%
    dplyr::arrange(.data$season, .data$round, dplyr::desc(.data$overcut_gain_sec))
}

summarize_track_overcuts <- function(overcut_events) {
  overcut_events %>%
    dplyr::group_by(.data$circuit_name) %>%
    dplyr::summarise(
      attempts = dplyr::n(),
      success_rate = mean(.data$overcut_succeeded, na.rm = TRUE),
      mean_gain_sec = mean(.data$overcut_gain_sec, na.rm = TRUE),
      median_gain_sec = stats::median(.data$overcut_gain_sec, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$mean_gain_sec))
}

estimate_degradation_slope <- function(lap_time, tyre_life) {
  model_data <- tibble::tibble(
    lap_time = as.numeric(lap_time),
    tyre_life = as.numeric(tyre_life)
  ) %>%
    dplyr::filter(is.finite(.data$lap_time), is.finite(.data$tyre_life))

  if (nrow(model_data) < 5L || dplyr::n_distinct(model_data$tyre_life) < 2L) {
    return(NA_real_)
  }

  tryCatch(
    unname(stats::coef(stats::lm(lap_time ~ tyre_life, data = model_data))[[2]]),
    error = function(e) NA_real_
  )
}

compute_race_context <- function(laps,
                                 season,
                                 round,
                                 race_name,
                                 circuit_name,
                                 damage_lap_buffer_sec = 20) {
  race_laps <- prepare_race_laps(laps)

  clean_laps <- race_laps %>%
    dplyr::filter(
      .data$is_green_flag,
      dplyr::coalesce(.data$is_accurate, FALSE),
      is.finite(.data$lap_time),
      !.data$has_pit_in,
      !.data$has_pit_out
    )

  stint_degradation <- clean_laps %>%
    dplyr::filter(!is.na(.data$tyre_life)) %>%
    dplyr::group_by(.data$driver, .data$stint) %>%
    dplyr::filter(dplyr::n() >= 5, dplyr::n_distinct(.data$tyre_life) >= 4) %>%
    dplyr::summarise(
      degradation_slope = estimate_degradation_slope(.data$lap_time, .data$tyre_life),
      .groups = "drop"
    )

  position_churn <- clean_laps %>%
    dplyr::group_by(.data$driver) %>%
    dplyr::arrange(.data$lap_number, .by_group = TRUE) %>%
    dplyr::mutate(position_change = .data$position != dplyr::lag(.data$position)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$position_change)) %>%
    dplyr::summarise(position_churn_rate = mean(.data$position_change, na.rm = TRUE)) %>%
    dplyr::pull(.data$position_churn_rate)

  lap_quality_tbl <- build_driver_lap_quality_table(
    race_laps = race_laps,
    damage_lap_buffer_sec = damage_lap_buffer_sec
  )

  tibble::tibble(
    season = as.integer(season),
    round = as.integer(round),
    race_name = race_name,
    circuit_name = circuit_name,
    degradation_proxy_sec_per_lap = stats::median(stint_degradation$degradation_slope, na.rm = TRUE),
    position_churn_rate = position_churn,
    broken_lap_rate = mean(lap_quality_tbl$broken_lap, na.rm = TRUE)
  )
}

run_schedule_context <- function(schedule_tbl,
                                 cache_dir = "cache/laps",
                                 damage_lap_buffer_sec = 20,
                                 log_level = "WARNING") {
  purrr::pmap_dfr(
    schedule_tbl,
    function(season, round, race_name, circuit_name, ...) {
      laps <- load_laps_cached(
        season = season,
        round = round,
        cache_dir = cache_dir,
        log_level = log_level
      )

      compute_race_context(
        laps = laps,
        season = season,
        round = round,
        race_name = race_name,
        circuit_name = circuit_name,
        damage_lap_buffer_sec = damage_lap_buffer_sec
      )
    }
  )
}

summarize_track_context <- function(race_context) {
  race_context %>%
    dplyr::group_by(.data$circuit_name) %>%
    dplyr::summarise(
      races = dplyr::n(),
      degradation_proxy_sec_per_lap = mean(.data$degradation_proxy_sec_per_lap, na.rm = TRUE),
      position_churn_rate = mean(.data$position_churn_rate, na.rm = TRUE),
      broken_lap_rate = mean(.data$broken_lap_rate, na.rm = TRUE),
      .groups = "drop"
    )
}

combine_track_views <- function(track_summary, overcut_summary, track_context) {
  track_summary %>%
    dplyr::rename(
      undercut_attempts = .data$attempts,
      undercut_success_rate = .data$success_rate,
      undercut_mean_gain_sec = .data$mean_gain_sec,
      undercut_median_gain_sec = .data$median_gain_sec,
      undercut_mean_position_gain = .data$mean_position_gain,
      undercut_mean_stop_lap_delta = .data$mean_stop_lap_delta
    ) %>%
    dplyr::left_join(
      overcut_summary %>%
        dplyr::rename(
          overcut_attempts = .data$attempts,
          overcut_success_rate = .data$success_rate,
          overcut_mean_gain_sec = .data$mean_gain_sec,
          overcut_median_gain_sec = .data$median_gain_sec
        ),
      by = "circuit_name"
    ) %>%
    dplyr::left_join(track_context, by = "circuit_name")
}
