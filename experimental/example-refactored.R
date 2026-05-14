#############################################################################
# TRIAL EMULATION: SURGERY WITHIN 6 MONTHS AMONG OLDER LUNG CANCER PATIENTS
# Refactored Clemence Leyrat's R code published on https://academic.oup.com/ije/article/49/5/1719/5835351
#############################################################################

# Read data
tab <- read.csv(
  "example_data/ije-2019-08-1035-File008.csv",
  sep = ",",
  header = TRUE
)

# print(tab)

#########################
# Step: Cloning
#########################

clone_arms <- function(data, arms) {
  n <- length(arms)
  if (n <= 1) {
    stop("`arms` must have more than one value.")
  }

  res <- vector("list", length = n)
  names(res) <- arms
  for (i in seq_len(n)) {
    res[[arms[[i]]]] <- data
  }

  res
}

arms <- c("Control", "Surgery")

clones <- clone_arms(tab, arms)

apply_policy_old <- function(
  arms,
  clones,
  policies,
  clone_outcome,
  clone_followup
) {
  res <- vector("list", length = length(arms))
  names(res) <- arms

  for (arm in arms) {
    res[[arm]] <-
      clones[[arm]] |>
      dplyr::mutate(
        {{ clone_outcome }} := dplyr::case_when(
          !!!rlang::parse_exprs(policies[[arm]][[clone_outcome]]),
          TRUE ~ NA
        ),
        {{ clone_followup }} := dplyr::case_when(
          !!!rlang::parse_exprs(policies[[arm]][[clone_followup]]),
          TRUE ~ NA
        )
      )
  }

  res
}

# apply case_when logics (e.g. policy, censoring) to clones
apply_logics <- function(
  clones,
  logics
) {
  stopifnot(names(clones) == names(logics))

  arms <- names(clones)

  res <- vector("list", length = length(arms))
  names(res) <- arms

  for (arm in arms) {
    vars <- names(logics[[arm]])

    res[[arm]] <- clones[[arm]]

    for (var in vars) {
      res[[arm]] <-
        res[[arm]] |>
        dplyr::mutate(
          {{ var }} := dplyr::case_when(
            !!!rlang::parse_exprs(logics[[arm]][[var]]),
            TRUE ~ NA
          )
        )
    }
  }

  res
}


# create clone policy for scenario A
# return as a nested list
create_policy_A <- function(
  arms,
  treatment,
  time_to_treatment,
  grace_period,
  outcome,
  followup,
  clone_outcome = ".outcome",
  clone_followup = ".fup"
) {
  res <- list(
    list(
      outcome = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ 0"
        ),
        glue::glue(
          "{treatment} == 0 | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ {outcome}"
        )
      ),
      followup = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ {time_to_treatment}"
        ),
        glue::glue(
          "{treatment} == 0 | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ {followup}"
        )
      )
    ),
    list(
      outcome = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ {outcome}"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ {outcome}"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 0 & {time_to_treatment} > {grace_period}) ~ 0"
        )
      ),
      followup = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ {followup}"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ {followup}"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 0 & {time_to_treatment} > {grace_period}) ~ {grace_period}"
        )
      )
    )
  )

  names(res) <- arms
  for (i in seq_along(res)) {
    names(res[[i]]) <- c(clone_outcome, clone_followup)
  }

  res
}

policies <- create_policy_A(
  arms,
  "surgery",
  "timetosurgery",
  182.62,
  "death",
  "fup_obs",
  "outcome",
  "fup"
)

testthat::expect_equal(
  policies,
  list(
    Control = list(
      outcome = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ 0",
        "surgery == 0 | (surgery == 1 & timetosurgery > 182.62) ~ death"
      ),
      fup = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ timetosurgery",
        "surgery == 0 | (surgery == 1 & timetosurgery > 182.62) ~ fup_obs"
      )
    ),
    Surgery = list(
      outcome = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ death",
        "surgery == 0 & fup_obs <= 182.62 ~ death",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 0 & timetosurgery > 182.62) ~ 0"
      ),
      fup = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ fup_obs",
        "surgery == 0 & fup_obs <= 182.62 ~ fup_obs",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 0 & timetosurgery > 182.62) ~ 182.62"
      )
    )
  )
)

clones_policy_applied <- apply_logics(clones, policies)

testthat::expect_equal(
  clones_policy_applied,
  apply_policy_old(arms, clones, policies, "outcome", "fup")
)

#########################
# Step: Censoring
#########################

censor_arms_old <- function(
  arms,
  clones,
  logics,
  censoring,
  followup_uncensored
) {
  res <- vector("list", length = length(arms))
  names(res) <- arms

  for (arm in arms) {
    res[[arm]] <-
      clones[[arm]] |>
      dplyr::mutate(
        {{ censoring }} := dplyr::case_when(
          !!!rlang::parse_exprs(logics[[arm]][["censoring"]]),
          TRUE ~ NA
        ),
        {{ followup_uncensored }} := dplyr::case_when(
          !!!rlang::parse_exprs(logics[[arm]][["fup_uncensored"]]),
          TRUE ~ NA
        )
      )
  }

  res
}

# create censoring logic for scenario A
# return as a nested list
create_censoring_logics_A <- function(
  arms,
  treatment,
  time_to_treatment,
  grace_period,
  followup,
  clone_censoring = ".censoring",
  clone_uncensored_followup = ".fup_uncensored"
) {
  res <- list(
    list(
      censoring = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ 1"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ 0"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ 0"
        )
      ),
      fup_uncensored = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ {time_to_treatment}"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ {followup}"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ {grace_period}"
        )
      )
    ),
    list(
      censoring = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ 0"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ 0"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ 1"
        )
      ),
      fup_uncensored = c(
        glue::glue(
          "{treatment} == 1 & {time_to_treatment} <= {grace_period} ~ {time_to_treatment}"
        ),
        glue::glue(
          "{treatment} == 0 & {followup} <= {grace_period} ~ {followup}"
        ),
        glue::glue(
          "({treatment} == 0 & {followup} > {grace_period}) | ({treatment} == 1 & {time_to_treatment} > {grace_period}) ~ {grace_period}"
        )
      )
    )
  )

  names(res) <- arms
  for (i in seq_along(res)) {
    names(res[[i]]) <- c(clone_censoring, clone_uncensored_followup)
  }

  res
}

censoring_logics <- create_censoring_logics_A(
  arms,
  "surgery",
  "timetosurgery",
  182.62,
  "fup_obs",
  "censoring",
  "fup_uncensored"
)

testthat::expect_equal(
  censoring_logics,
  list(
    Control = list(
      censoring = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ 1",
        "surgery == 0 & fup_obs <= 182.62 ~ 0",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 1 & timetosurgery > 182.62) ~ 0"
      ),
      fup_uncensored = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ timetosurgery",
        "surgery == 0 & fup_obs <= 182.62 ~ fup_obs",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 1 & timetosurgery > 182.62) ~ 182.62"
      )
    ),
    Surgery = list(
      censoring = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ 0",
        "surgery == 0 & fup_obs <= 182.62 ~ 0",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 1 & timetosurgery > 182.62) ~ 1"
      ),
      fup_uncensored = c(
        "surgery == 1 & timetosurgery <= 182.62 ~ timetosurgery",
        "surgery == 0 & fup_obs <= 182.62 ~ fup_obs",
        "(surgery == 0 & fup_obs > 182.62) | (surgery == 1 & timetosurgery > 182.62) ~ 182.62"
      )
    )
  )
)

clones_censored <- apply_logics(clones_policy_applied, censoring_logics)

testthat::expect_equal(
  clones_censored,
  censor_arms_old(
    arms,
    clones_policy_applied,
    censoring_logics,
    "censoring",
    "fup_uncensored"
  )
)


#########################
# Step: Weighting (No bootstrapping)
#########################

# create timestamp table
create_timestamp_table_old <- function(clones) {
  timestamps <- sapply(clones, `[[`, i = "fup", simplify = FALSE)
  t_events <- sort(unique(unlist(timestamps)))
  res <- dplyr::tibble(tevent = t_events, ID_t = seq_along(t_events))

  res
}

# create timestamp table
create_timestamp_table <- function(clones, clone_followup) {
  timestamps <- sapply(clones, `[[`, i = clone_followup, simplify = FALSE)
  t_events <- sort(unique(unlist(timestamps)))
  res <- dplyr::tibble(tevent = t_events, ID_t = seq_along(t_events))

  res
}


# split data at each time event
split_at_timestamp_old <- function(clones, t_events, event) {
  arms <- names(clones)

  res <- vector("list", length = length(arms))
  names(res) <- arms
  for (arm in arms) {
    res[[arm]] <-
      clones[[arm]] |>
      survival::survSplit(
        cut = t_events,
        end = "fup",
        start = "Tstart",
        event = event,
        id = "ID"
      )
  }

  res
}

# split data at each time event
split_at_timestamp <- function(
  clones,
  clone_followup,
  t_events,
  event,
  timestamp_start = "Tstart",
  timestamp_id = "ID"
) {
  arms <- names(clones)

  res <- vector("list", length = length(arms))
  names(res) <- arms
  for (arm in arms) {
    res[[arm]] <-
      clones[[arm]] |>
      survival::survSplit(
        cut = t_events,
        end = clone_followup,
        start = timestamp_start,
        event = event,
        id = timestamp_id
      )
  }

  res
}


# Function to create training data for censoring probability estimation
# TO DO: Eliminate dependency on {purrr}
create_final_data_old <- function(clones) {
  df_timestamp <- create_timestamp_table_old(clones)

  clones_splitted_by_outcome <- split_at_timestamp_old(
    clones,
    df_timestamp$tevent,
    "outcome"
  )

  clones_splitted_by_censoring <- split_at_timestamp_old(
    clones,
    df_timestamp$tevent,
    "censoring"
  )

  # merge two tables and create Tstop column
  clones_splitted <-
    purrr::map2(
      clones_splitted_by_outcome,
      clones_splitted_by_censoring,
      \(x, y) {
        dplyr::inner_join(
          x |> dplyr::select(!censoring),
          y |> dplyr::select(id, fup, censoring),
          by = dplyr::join_by(id, fup)
        ) |>
          dplyr::mutate(Tstop = fup)
      }
    )

  # Merge with timestamp table
  res <-
    purrr::map(
      clones_splitted,
      \(x, df_timestamp) {
        dplyr::left_join(x, df_timestamp, by = dplyr::join_by(Tstart == tevent))
      },
      df_timestamp = df_timestamp |>
        dplyr::bind_rows(dplyr::tibble(tevent = 0, ID_t = 0))
    )

  res
}

# Function to create training data for censoring probability estimation
create_final_data <- function(
  clones,
  clone_followup,
  clone_outcome,
  clone_censoring,
  col_ids,
  timestamp_start = "Tstart",
  timestamp_id = "ID",
  timestamp_stop = "Tstop"
) {
  df_timestamp <- create_timestamp_table(clones, clone_followup)

  clones_splitted_by_outcome <- split_at_timestamp(
    clones,
    clone_followup,
    df_timestamp$tevent,
    clone_outcome,
    timestamp_start,
    timestamp_id
  )

  clones_splitted_by_censoring <- split_at_timestamp(
    clones,
    clone_followup,
    df_timestamp$tevent,
    clone_censoring,
    timestamp_start,
    timestamp_id
  )

  # merge two tables and create column to represent end of timestamp
  df_timestamp_with_time_zero <-
    df_timestamp |>
    dplyr::bind_rows(dplyr::tibble(tevent = 0, ID_t = 0))

  n_clones <- length(clones)
  arms <- names(clones)
  res <- vector("list", length = n_clones)
  names(res) <- arms

  for (i in seq_len(n_clones)) {
    x <-
      clones_splitted_by_outcome[[i]] |>
      dplyr::select(!{{ clone_censoring }})

    y <-
      clones_splitted_by_censoring[[i]] |>
      dplyr::select(
        all_of(col_ids),
        {{ clone_followup }},
        {{ clone_censoring }}
      )

    res[[i]] <-
      dplyr::inner_join(
        x,
        y,
        by = dplyr::join_by({{ col_ids }}, {{ clone_followup }})
      ) |>
      dplyr::mutate({{ timestamp_stop }} := .data[[clone_followup]])

    # Merge with timestamp table
    res[[i]] <-
      res[[i]] |>
      dplyr::left_join(
        df_timestamp_with_time_zero,
        by = dplyr::join_by({{ timestamp_start }} == tevent)
      )
  }

  res
}


clones_final_old <- create_final_data_old(clones_censored)
clones_final <- create_final_data(
  clones_censored,
  "fup",
  "outcome",
  "censoring",
  c("id"),
  "Tstart",
  "ID",
  "Tstop"
)

testthat::expect_equal(clones_final, clones_final_old)


# Function to estimate (un)censoring probability
# TO DO: better understand the estimation method
estimate_censoring <- function(clones, predictors) {
  arms <- names(clones)

  res <- vector("list", length = length(arms))
  names(res) <- arms

  formula <-
    paste(
      "survival::Surv(Tstart, Tstop, censoring)",
      paste(predictors, collapse = " + "),
      sep = " ~ "
    )

  for (arm in arms) {
    ms_cens <- survival::coxph(
      as.formula(formula),
      ties = "efron",
      data = clones[[arm]]
    ) #We can also includes their interactions

    # Design matrix
    model_terms <- terms(ms_cens)
    attr(model_terms, "intercept") <- 0
    design_mat <- model.matrix(model_terms, data = clones[[arm]])

    # Vector of regression coefficients
    beta <- coef(ms_cens)

    # Calculation of XB (linear combination of the covariates)
    lin_pred <- as.vector(design_mat %*% beta)

    # Estimating the cumulative hazard (when covariates=0)
    base_hazard <- dplyr::as_tibble(
      survival::basehaz(ms_cens, centered = FALSE)
    )
    names(base_hazard) <- c("hazard", "t")

    # Merging and reordering the dataset
    # And estimating the probability of remaining uncensored at each time of event
    res[[arm]] <-
      clones[[arm]] |>
      dplyr::mutate(lin_pred = .env[["lin_pred"]]) |>
      dplyr::left_join(
        base_hazard,
        by = dplyr::join_by(Tstart == t)
      ) |>
      dplyr::mutate(
        hazard = dplyr::coalesce(hazard, 0)
      ) |>
      dplyr::mutate(
        P_uncens = exp(-hazard * exp(lin_pred))
      )
  }

  res
}

predictors <- c(
  "age",
  "sex",
  "emergency",
  "stage",
  "deprivation",
  "charlson",
  "perf"
)

clones_estimated <- estimate_censoring(clones_final, predictors)


weight_cases <- function(clones) {
  arms <- names(clones)

  res <- vector("list", length = length(arms))
  names(res) <- arms

  for (arm in arms) {
    res[[arm]] <-
      clones[[arm]] |>
      dplyr::mutate(
        weight_Cox = 1 / P_uncens # IPCW
      )
  }

  res
}

clones_weighted <- weight_cases(clones_estimated)

#########################
# TO DO: Step: Main analysis (Emulated trial with Cox weights (Kaplan-Meier))
#########################

emul_Cox <- purrr::map(
  clones_weighted,
  \(x) {
    survival::survfit(
      survival::Surv(Tstart, Tstop, outcome) ~ 1,
      data = x,
      weights = weight_Cox
    )
  }
)

# 1 year survival in the surgery group
purrr::map_dbl(
  emul_Cox,
  \(x) min(x$surv)
)

#########################
# TO DO: Bootstrapping for confidence interval computation
#########################
