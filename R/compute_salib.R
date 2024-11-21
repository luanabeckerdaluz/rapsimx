si_to_df <- function(Si) {
  df <- data.frame(
    names =   unlist(Si["names"]),
    S1 =      unlist(Si["S1"]),
    S1_conf = unlist(Si["S1_conf"]),
    ST =      unlist(Si["ST"]),
    ST_conf = unlist(Si["ST_conf"])
  )
  return(df)
}

si_to_df_sobol <- function(Si, names) {
  df <- data.frame(
    names =   names,
    S1 =      Si$S1,
    S1_conf = Si$S1_conf,
    ST =      Si$ST,
    ST_conf = Si$ST_conf
  )
  return(df)
}

salib_for_each_field_and_param <- function(param, df, fields, salib_sobol = FALSE, verbose_NAs, fix_NAs) {
  np <- import("numpy")
  analyze <- import("SALib.analyze.fast")
  if (salib_sobol) {
    analyze <- import("SALib.analyze.sobol")
  }

  list_dfs_salibs <- lapply(
    X = fields,
    FUN = function(f) {
      arr <- df %>%
        filter(field == !!f) %>%
        pull(!!param)

      if (verbose_NAs) {
        count_NAs <- sum(is.na(arr))
        if (count_NAs > 0) {
          print(paste("Array for", param, "and field", f, "has", count_NAs, "NAs"))
        }
      }

      if (fix_NAs) {
        arr[is.na(arr)] <- -9999
      }

      np_arr <- np$array(arr)
      si_df <- NA
      if (salib_sobol) {
        Si <- analyze$analyze(problem, np_arr, calc_second_order = TRUE)
        si_df <- si_to_df_sobol(Si, problem$names)
      } else {
        Si <- analyze$analyze(problem, np_arr)
        si_df <- si_to_df(Si)
      }

      si_df %>%
        mutate(field = !!f) %>%
        select(field, everything())
    }
  )

  rbind_salibs <- Reduce(function(x, y) rbind(x, y, all = TRUE), list_dfs_salibs)
  # print(dim(rbind_salibs))
  # print(head(rbind_salibs))

  salib_vars <- c("S1", "ST", "S1_conf", "ST_conf")

  rbind_salibs_pivotlonger <- rbind_salibs %>%
    # select("names", "field", "S1", "ST") %>%
    select(c("names", "field", all_of(salib_vars))) %>%
    tidyr::pivot_longer(cols = salib_vars, names_to = "variable", values_to = "value") %>%
    mutate(param = !!param)

  return(rbind_salibs_pivotlonger)
}

# Function to compute SALIB for each field and all params
compute_salib_for_all_params_each_field <- function(df, salib_sobol, verbose_NAs = FALSE, fix_NAs = FALSE) {
  fields <- unique(df$field)

  yield               <- salib_for_each_field_and_param("yield", df, fields, salib_sobol, verbose_NAs, fix_NAs)
  biomass             <- salib_for_each_field_and_param("biomass", df, fields, salib_sobol, verbose_NAs, fix_NAs)
  start_flowering     <- salib_for_each_field_and_param("flowering", df, fields, salib_sobol, verbose_NAs, fix_NAs)
  pod_development     <- salib_for_each_field_and_param("pod_development", df, fields, salib_sobol, verbose_NAs, fix_NAs)
  start_grain_filling <- salib_for_each_field_and_param("start_grain_filling", df, fields, salib_sobol, verbose_NAs, fix_NAs)
  maturity            <- salib_for_each_field_and_param("maturity", df, fields, salib_sobol, verbose_NAs, fix_NAs)

  big_tibble <- rbind(
      yield,
      biomass,
      start_flowering,
      pod_development,
      start_grain_filling,
      maturity
    ) %>%
    filter(names != TRUE)

  return(big_tibble)
}