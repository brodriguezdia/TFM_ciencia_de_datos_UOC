# ============================================================
# TFM Canarias - Pipeline final de datos, ODS y predicción
# ============================================================
# Objetivo:
#   1) Descargar y preparar datos climáticos AEMET con climaemet
#   2) Seleccionar estaciones representativas por isla
#   3) Generar capa climática diaria, mensual, anual e isla-año
#   4) Integrar indicadores ODS 7, 13, 14 y 15
#   5) Generar datasets finales: model_island_year y model_canarias_year
#   6) Ejecutar análisis final: EDA, predicción hasta 2030, validación y ODS
#
# Requisitos:
#   - R >= 4.2
#   - API key de AEMET en variable de entorno:
#       Sys.setenv(AEMET_API_KEY = "TU_API_KEY")
#     o bien:
#       climaemet::aemet_api_key("TU_API_KEY", install = TRUE)
#
# Nota importante:
#   No se incrusta la API key en el script por seguridad y reproducibilidad.
# ============================================================

# -----------------------------
# Paquetes
# -----------------------------
required_pkgs <- c(
  "climaemet", "tidyverse", "janitor", "sf", "cli", "units",
  "lubridate", "readr", "stringr", "broom", "yardstick",
  "tsibble", "fable", "feasts", "fabletools", "slider",
  "tidymodels", "ranger", "xgboost", "vip", "gtsummary"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(climaemet)
  library(tidyverse)
  library(janitor)
  library(sf)
  library(cli)
  library(units)
  library(lubridate)
  library(broom)
  library(yardstick)
  library(tsibble)
  library(fable)
  library(feasts)
  library(fabletools)
  library(slider)
  library(tidymodels)
  library(vip)
  library(gtsummary)
})

# Evita conflictos frecuentes
options(dplyr.summarise.inform = FALSE)

# -----------------------------
# Configuración general
# -----------------------------

# La API Key de Aemet tiene una validez de 5 días
api_key <- Sys.getenv("AEMET_API_KEY")
if (api_key == "") {
  stop(
    "No se encontró AEMET_API_KEY en las variables de entorno.\n",
    "Ejecuta antes: Sys.setenv(AEMET_API_KEY = 'TU_API_KEY')\n",
    "o climaemet::aemet_api_key('TU_API_KEY', install = TRUE)."
  )
}

climaemet::aemet_detect_api_key()

YEAR_START <- 2015L
YEAR_END   <- 2025L
YEARS <- YEAR_START:YEAR_END
FORECAST_END_YEAR <- 2030L
N_STATIONS_PER_ISLAND <- 5L
MIN_TEMP_COVERAGE_ANNUAL <- 0.80

# Directorios
base_dir <- "datos_clima_canarias"
out_dir <- "tfm_outputs"

dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(base_dir, "raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(base_dir, "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(base_dir, "metadata"), showWarnings = FALSE, recursive = TRUE)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "models"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Helpers generales
# -----------------------------
safe_write_csv <- function(df, path) {
  readr::write_csv(df, path, na = "")
  invisible(path)
}

safe_write_rds <- function(obj, path) {
  saveRDS(obj, path)
  invisible(path)
}

save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, "figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

sum_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

weighted_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w = w[ok])
}

# Asignación simple de isla por bounding boxes.
# Suficiente para estaciones AEMET en Canarias, pero se documenta como aproximación.
assign_island <- function(lon, lat) {
  dplyr::case_when(
    lon >= -18.20 & lon <= -17.80 & lat >= 27.60 & lat <= 27.85 ~ "El Hierro",
    lon >= -18.10 & lon <= -17.65 & lat >= 28.45 & lat <= 28.90 ~ "La Palma",
    lon >= -17.40 & lon <= -17.00 & lat >= 27.95 & lat <= 28.25 ~ "La Gomera",
    lon >= -16.95 & lon <= -16.05 & lat >= 28.00 & lat <= 28.65 ~ "Tenerife",
    lon >= -15.90 & lon <= -15.15 & lat >= 27.70 & lat <= 28.25 ~ "Gran Canaria",
    lon >= -14.65 & lon <= -13.75 & lat >= 27.85 & lat <= 28.85 ~ "Fuerteventura",
    lon >= -13.98 & lon <= -13.30 & lat >= 28.80 & lat <= 29.35 ~ "Lanzarote",
    TRUE ~ NA_character_
  )
}

# Conversor robusto para campos numéricos AEMET.
# Gestiona coma decimal, texto, NA e "Ip" en precipitación.
to_numeric_aemet <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))

  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NULL", "NAN", "NaN", "///", "--")] <- NA_character_
  x[x %in% c("Ip", "IP", "ip")] <- "0"

  readr::parse_number(
    x,
    locale = readr::locale(decimal_mark = ",")
  )
}

# -----------------------------
# Estaciones AEMET de Canarias
# -----------------------------

stations_sf_raw <- climaemet::aemet_stations(return_sf = TRUE) |>
  janitor::clean_names() |>
  sf::st_make_valid()

stations_sf_raw <- stations_sf_raw |>
  filter(provincia %in% c("LAS PALMAS", "SANTA CRUZ DE TENERIFE", "STA. CRUZ DE TENERIFE"))

coords <- sf::st_coordinates(stations_sf_raw)

stations_sf <- stations_sf_raw |>
  mutate(
    lon = coords[, 1],
    lat = coords[, 2],
    island = assign_island(lon, lat),
    island = if_else(nombre == "LA GRACIOSA", "La Graciosa", island)
  ) |>
  filter(!is.na(island)) |>
  rename(
    station_id = indicativo,
    station_name = nombre,
    province = provincia,
    altitude = altitud
  )

stations <- stations_sf |>
  sf::st_drop_geometry()

safe_write_csv(stations, file.path(base_dir, "metadata", "aemet_stations_canarias.csv"))
safe_write_rds(stations, file.path(base_dir, "metadata", "aemet_stations_canarias.rds"))

stations_utm <- sf::st_transform(stations_sf, 32628)

p_stations_all <- ggplot(stations_sf) +
  geom_sf(aes(color = island), size = 2, alpha = 0.9) +
  theme_minimal() +
  labs(
    title = "Estaciones meteorológicas AEMET en Canarias",
    color = "Isla"
  )
save_plot(p_stations_all, "figura_01_estaciones_aemet_canarias.png", width = 9, height = 6)

# -----------------------------
# Métricas de distancia y selección espacial
# -----------------------------

calc_distance_metrics <- function(sf_obj, id_col = "station_id", name_col = "station_name", island_col = "island") {
  sf_obj |>
    group_by(.data[[island_col]]) |>
    group_modify(~{
      n <- nrow(.x)
      if (n == 1) {
        return(tibble(
          station_id = .x[[id_col]],
          station_name = .x[[name_col]],
          mean_dist_km = NA_real_,
          min_nn_km = NA_real_,
          max_dist_km = NA_real_,
          medoid_score = 0
        ))
      }

      d <- sf::st_distance(.x)
      d_num <- units::drop_units(d) / 1000
      diag(d_num) <- NA_real_

      tibble(
        station_id = .x[[id_col]],
        station_name = .x[[name_col]],
        mean_dist_km = rowMeans(d_num, na.rm = TRUE),
        min_nn_km = apply(d_num, 1, min, na.rm = TRUE),
        max_dist_km = apply(d_num, 1, max, na.rm = TRUE),
        medoid_score = rowSums(d_num, na.rm = TRUE)
      ) |>
        arrange(medoid_score)
    }) |>
    ungroup()
}

select_spread_stations <- function(sf_obj, k = 5, id_col = "station_id", island_col = "island") {
  split_list <- split(sf_obj, sf_obj[[island_col]])

  selected <- purrr::imap_dfr(split_list, function(df_island, island_name) {
    df_island <- sf::st_as_sf(df_island)
    n <- nrow(df_island)
    k_use <- min(k, n)

    d <- sf::st_distance(df_island)
    d_num <- units::drop_units(d) / 1000

    selected_idx <- which.min(rowSums(d_num, na.rm = TRUE))

    if (k_use > 1) {
      while (length(selected_idx) < k_use) {
        remaining <- setdiff(seq_len(n), selected_idx)
        min_to_selected <- sapply(remaining, function(i) min(d_num[i, selected_idx], na.rm = TRUE))
        next_idx <- remaining[which.max(min_to_selected)]
        selected_idx <- c(selected_idx, next_idx)
      }
    }

    out <- df_island[selected_idx, ]
    out$selection_rank <- seq_len(nrow(out))
    out
  })

  sf::st_as_sf(selected)
}

distance_metrics <- calc_distance_metrics(stations_utm)
safe_write_csv(distance_metrics, file.path(base_dir, "metadata", "aemet_station_distance_metrics.csv"))

best_5_per_island <- select_spread_stations(stations_utm, k = N_STATIONS_PER_ISLAND)
best_5_map <- sf::st_transform(best_5_per_island, sf::st_crs(stations_sf))

p_stations_selected <- ggplot() +
  geom_sf(data = stations_sf, color = "grey75", size = 1.3, alpha = 0.6) +
  geom_sf(data = best_5_map, aes(color = island), size = 2.4) +
  theme_minimal() +
  labs(
    title = "Selección espacial de estaciones AEMET",
    subtitle = paste0(N_STATIONS_PER_ISLAND, " estaciones por isla cuando hay disponibilidad suficiente"),
    color = "Isla"
  )
save_plot(p_stations_selected, "figura_02_estaciones_seleccionadas.png", width = 9, height = 6)

selected_station_ids <- unique(best_5_map$station_id)
selected_station_ids <- unique(c(selected_station_ids, manual_extra_stations))

safe_write_csv(
  best_5_map |> sf::st_drop_geometry() |> select(station_id, station_name, province, island, lon, lat, altitude, selection_rank),
  file.path(base_dir, "metadata", "aemet_selected_stations.csv")
)

# -----------------------------
# Descarga diaria por estación y año
# -----------------------------

hour_cols <- c(
  "horatmin", "horatmax", "horaracha",
  "hora_hr_min", "hora_hr_max",
  "hora_pres_min", "hora_pres_max"
)

numeric_cols <- c(
  "altitud", "tmed", "prec", "tmin", "tmax", "dir", "velmedia", "racha",
  "hr_media", "hr_max", "hr_min", "sol", "pres_max", "pres_min"
)

normalize_aemet_daily <- function(df, drop_hour_cols = TRUE) {
  df <- df |>
    janitor::clean_names()

  if ("fecha" %in% names(df)) {
    df <- df |>
      mutate(fecha = as.Date(fecha))
  }

  df <- df |>
    mutate(across(any_of(hour_cols), as.character)) |>
    mutate(across(any_of(numeric_cols), to_numeric_aemet))

  if (drop_hour_cols) {
    df <- df |>
      select(-any_of(hour_cols))
  }

  df
}

fetch_station_year <- function(station_id, year, drop_hour_cols = TRUE) {
  cli::cli_alert_info("Estación {station_id} - año {year}")

  out <- tryCatch(
    climaemet::aemet_daily_period(
      station = station_id,
      start = year,
      end = year,
      verbose = FALSE,
      return_sf = FALSE,
      extract_metadata = FALSE,
      progress = FALSE
    ) |>
      normalize_aemet_daily(drop_hour_cols = drop_hour_cols),
    error = function(e) {
      cli::cli_alert_warning("Fallo en {station_id} - {year}: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(out) || nrow(out) == 0) return(NULL)

  out |>
    mutate(
      station_id = as.character(station_id),
      year_req = as.integer(year)
    )
}

raw_daily <- purrr::map_dfr(
  selected_station_ids,
  function(st) {
    purrr::map_dfr(
      YEARS,
      ~ fetch_station_year(station_id = st, year = .x, drop_hour_cols = TRUE)
    )
  }
)

# Vemos que estaciones están incompletas
station_not_complete <- raw_daily %>% group_by(indicativo) %>% summarise(n_year = n_distinct(year_req)) %>% filter(n_year < 11) %>% pull(indicativo)

raw_daily <- raw_daily %>% filter(!indicativo %in% station_not_complete)

raw_daily2 <- purrr::map_dfr(
  stations$indicativo[stations$indicativo %in% c("C619Y","C018J")], #Sustituimos por dos estaciones que si tienen datos 
  function(st) {
    purrr::map_dfr(
      YEARS,
      ~ fetch_station_year(st, .x, drop_hour_cols = TRUE)
    )
  }
)

raw_daily <- rbind(raw_daily, raw_daily2)

safe_write_csv(raw_daily, file.path(base_dir, "raw", "aemet_daily_raw_canarias.csv"))
safe_write_rds(raw_daily, file.path(base_dir, "raw", "aemet_daily_raw_canarias.rds"))

# -----------------------------
# Limpieza, estandarización y flags de calidad
# -----------------------------

clean_daily <- raw_daily |>
  select(any_of(c(
    "fecha", "indicativo", "station_id", "nombre", "provincia", "altitud",
    "tmin", "tmed", "tmax", "prec", "pres_min", "pres_max",
    "dir", "velmedia", "racha", "hr_min", "hr_media", "hr_max", "sol", "year_req"
  ))) |>
  mutate(
    station_id = coalesce(as.character(station_id), as.character(indicativo)),
    date = as.Date(fecha),
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date)
  ) |>
  left_join(
    stations |>
      select(station_id, station_name_meta = station_name, province_meta = province, island, lon, lat, altitude_meta = altitude),
    by = "station_id"
  ) |>
  distinct(station_id, date, .keep_all = TRUE) |>
  mutate(
    # Flags de calidad fisica basica
    flag_missing_temp = is.na(tmed) & is.na(tmin) & is.na(tmax),
    flag_bad_tmin_tmax = !is.na(tmin) & !is.na(tmax) & tmin > tmax,
    flag_bad_tmed_low = !is.na(tmed) & !is.na(tmin) & tmed < tmin,
    flag_bad_tmed_high = !is.na(tmed) & !is.na(tmax) & tmed > tmax,
    flag_bad_prec = !is.na(prec) & prec < 0,
    flag_implausible_tmin = !is.na(tmin) & (tmin < -20 | tmin > 40),
    flag_implausible_tmax = !is.na(tmax) & (tmax < -10 | tmax > 55),
    flag_implausible_tmed = !is.na(tmed) & (tmed < -15 | tmed > 45),
    flag_any = flag_bad_tmin_tmax | flag_bad_tmed_low | flag_bad_tmed_high |
      flag_bad_prec | flag_implausible_tmin | flag_implausible_tmax | flag_implausible_tmed,
    # Se conservan flags y se invalidan valores físicamente incoherentes para agregación.
    tmin = if_else(flag_bad_tmin_tmax | flag_implausible_tmin, NA_real_, tmin),
    tmax = if_else(flag_bad_tmin_tmax | flag_implausible_tmax, NA_real_, tmax),
    tmed = if_else(flag_bad_tmed_low | flag_bad_tmed_high | flag_implausible_tmed | flag_bad_tmin_tmax, NA_real_, tmed),
    prec = if_else(flag_bad_prec, NA_real_, prec)
  )

# Cobertura por estacion-ano
coverage_station_year <- clean_daily |>
  group_by(station_id, island, year) |>
  summarise(
    station_name = first(coalesce(nombre, station_name_meta)),
    expected_days = if_else(lubridate::leap_year(first(date)), 366L, 365L),
    n_rows = n(),
    n_temp = sum(!is.na(tmed)),
    n_prec = sum(!is.na(prec)),
    pct_temp = n_temp / expected_days,
    pct_prec = n_prec / expected_days,
    n_flags = sum(flag_any, na.rm = TRUE),
    .groups = "drop"
  )

# Regla de calidad: para agregaciones anuales exigir al menos 80% de cobertura termica
valid_station_year <- coverage_station_year |>
  mutate(use_for_annual = pct_temp >= MIN_TEMP_COVERAGE_ANNUAL)

safe_write_csv(clean_daily, file.path(base_dir, "processed", "aemet_daily_clean_canarias.csv"))
safe_write_rds(clean_daily, file.path(base_dir, "processed", "aemet_daily_clean_canarias.rds"))
safe_write_csv(valid_station_year, file.path(base_dir, "processed", "aemet_station_year_coverage.csv"))

# -----------------------------
# Agregaciones mensuales, anuales y por isla-año
# -----------------------------

monthly_station <- clean_daily |>
  group_by(station_id, island, year, month) |>
  summarise(
    station_name = first(coalesce(nombre, station_name_meta)),
    expected_days = lubridate::days_in_month(as.Date(paste0(year[1], "-", month[1], "-01"))),
    n_days = n(),
    n_temp = sum(!is.na(tmed)),
    n_prec = sum(!is.na(prec)),
    pct_temp = n_temp / as.integer(expected_days),
    tmed_month = mean(tmed, na.rm = TRUE),
    tmin_month = mean(tmin, na.rm = TRUE),
    tmax_month = mean(tmax, na.rm = TRUE),
    prec_month = sum_na(prec),
    tropical_nights_month = sum(tmin >= 20, na.rm = TRUE),
    equatorial_nights_month = sum(tmin >= 25, na.rm = TRUE),
    torrid_nights_month = sum(tmin >= 30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(tmed_month, tmin_month, tmax_month, prec_month), ~ ifelse(is.nan(.x), NA_real_, .x)))

annual_station <- clean_daily |>
  group_by(station_id, island, year) |>
  summarise(
    station_name = first(coalesce(nombre, station_name_meta)),
    n_days  = n(),
    n_temp = sum(!is.na(tmed)),
    n_prec = sum(!is.na(prec)),
    n_hr   = sum(!is.na(hr_media)),
    n_wind = sum(!is.na(velmedia)),
    n_sol  = sum(!is.na(sol)),
    n_pres = sum(!is.na(pres_min) | !is.na(pres_max)),
    
    pct_temp = n_temp / n_days,
    pct_prec = n_prec / n_days,
    pct_hr   = n_hr / n_days,
    pct_wind = n_wind / n_days,
    pct_sol  = n_sol / n_days,
    pct_pres = n_pres / n_days,
    
    altitud = min(altitud),
    
    tmed_annual = mean(tmed, na.rm = TRUE),
    tmin_annual = mean(tmin, na.rm = TRUE),
    tmax_annual = mean(tmax, na.rm = TRUE),
    
    prec_annual = ifelse(all(is.na(prec)), NA_real_, sum(prec, na.rm = TRUE)),
    
    hr_media_annual = mean(hr_media, na.rm = TRUE),
    hr_min_annual   = mean(hr_min, na.rm = TRUE),
    hr_max_annual   = mean(hr_max, na.rm = TRUE),
    
    velmedia_annual = mean(velmedia, na.rm = TRUE),
    racha_mean_annual = mean(racha, na.rm = TRUE),
    
    sol_annual = ifelse(all(is.na(sol)), NA_real_, sum(sol, na.rm = TRUE)),
    
    pres_min_annual = mean(pres_min, na.rm = TRUE),
    pres_max_annual = mean(pres_max, na.rm = TRUE),
    
    tropical_nights_annual = sum(tmin >= 20, na.rm = TRUE),
    equatorial_nights_annual = sum(tmin >= 25, na.rm = TRUE),
    torrid_nights_annual = sum(tmin >= 30, na.rm = TRUE),
    warm_days_30 = sum(tmax >= 30, na.rm = TRUE),
    warm_days_35 = sum(tmax >= 35, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    across(
      c(
        tmed_annual, tmin_annual, tmax_annual,
        hr_media_annual, hr_min_annual, hr_max_annual,
        velmedia_annual, racha_mean_annual,
        sol_annual, pres_min_annual, pres_max_annual
      ),
      ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  ) |>
  left_join(valid_station_year |> select(station_id, year, pct_temp, pct_prec, use_for_annual), by = c("station_id", "year"))

# Mantener anos-estacion con cobertura suficiente para analisis anual
annual_station_good <- annual_station |>
  filter(use_for_annual)

# Peso simple por dias validos; evita que una estacion con poca cobertura pese igual.
annual_island <- annual_station_good |>
  group_by(island, year) |>
  summarise(
    n_stations = n_distinct(station_id),
    
    weight_temp = sum(n_temp, na.rm = TRUE),
    weight_prec = sum(n_prec, na.rm = TRUE),
    weight_hr   = sum(n_hr, na.rm = TRUE),
    weight_wind = sum(n_wind, na.rm = TRUE),
    weight_sol  = sum(n_sol, na.rm = TRUE),
    weight_pres = sum(n_pres, na.rm = TRUE),
    
    tmed_annual = weighted_mean_safe(tmed_annual, n_temp),
    tmin_annual = weighted_mean_safe(tmin_annual, n_temp),
    tmax_annual = weighted_mean_safe(tmax_annual, n_temp),
    
    prec_annual = weighted_mean_safe(prec_annual, n_prec),
    
    hr_media_annual = weighted_mean_safe(hr_media_annual, n_hr),
    hr_min_annual   = weighted_mean_safe(hr_min_annual, n_hr),
    hr_max_annual   = weighted_mean_safe(hr_max_annual, n_hr),
    
    velmedia_annual = weighted_mean_safe(velmedia_annual, n_temp),
    racha_mean_annual = weighted_mean_safe(racha_mean_annual, n_wind),
    
    sol_annual = weighted_mean_safe(sol_annual, n_sol),
    
    pres_min_annual = weighted_mean_safe(pres_min_annual, n_pres),
    pres_max_annual = weighted_mean_safe(pres_max_annual, n_pres),
    
    tropical_nights_annual = round(weighted_mean_safe(tropical_nights_annual, n_temp), 1),
    equatorial_nights_annual = round(weighted_mean_safe(equatorial_nights_annual, n_temp), 1),
    torrid_nights_annual = round(weighted_mean_safe(torrid_nights_annual, n_temp), 1),
    warm_days_30 = round(weighted_mean_safe(warm_days_30, n_temp), 1),
    warm_days_35 = round(weighted_mean_safe(warm_days_35, n_temp), 1),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x) | is.infinite(.x), NA_real_, .x)))

# Anomalías respecto al periodo disponible de cada isla.
island_baseline <- annual_island |>
  group_by(island) |>
  summarise(
    baseline_tmed = mean(tmed_annual, na.rm = TRUE),
    baseline_tmin = mean(tmin_annual, na.rm = TRUE),
    baseline_tmax = mean(tmax_annual, na.rm = TRUE),
    .groups = "drop"
  )

annual_island <- annual_island |>
  left_join(island_baseline, by = "island") |>
  mutate(
    anom_tmed = tmed_annual - baseline_tmed,
    anom_tmin = tmin_annual - baseline_tmin,
    anom_tmax = tmax_annual - baseline_tmax
  ) |>
  arrange(island, year)

safe_write_csv(monthly_station, file.path(base_dir, "processed", "aemet_monthly_station_canarias.csv"))
safe_write_csv(annual_station, file.path(base_dir, "processed", "aemet_annual_station_canarias_all.csv"))
safe_write_csv(annual_station_good, file.path(base_dir, "processed", "aemet_annual_station_canarias_good.csv"))
safe_write_csv(annual_island, file.path(base_dir, "processed", "aemet_annual_island_canarias_with_anomalies.csv"))

climate_model_table <- annual_island |>
  select(
    island, year, n_stations,
    
    tmed_annual, tmin_annual, tmax_annual,
    anom_tmed, anom_tmin, anom_tmax,
    
    prec_annual,
    hr_media_annual, hr_min_annual, hr_max_annual,
    velmedia_annual, racha_mean_annual,
    sol_annual,
    pres_min_annual, pres_max_annual,
    
    tropical_nights_annual, equatorial_nights_annual, torrid_nights_annual,
    warm_days_30, warm_days_35
  ) |>
  arrange(island, year)

safe_write_csv(climate_model_table, file.path(base_dir, "processed", "climate_model_island_year.csv"))
safe_write_rds(climate_model_table, file.path(base_dir, "processed", "climate_model_island_year.rds"))

# -----------------------------
# Integración ODS
# -----------------------------

#Diccionario
territory_map <- tribble(
  ~territory_code, ~territory_name,     ~level,
  "ES",            "España",            "pais",
  "ES70",          "Canarias",          "canarias",
  "ES703",         "El Hierro",         "isla",
  "ES704",         "Fuerteventura",     "isla",
  "ES705",         "Gran Canaria",      "isla",
  "ES706",         "La Gomera",         "isla",
  "ES707",         "La Palma",          "isla",
  "ES708",         "Lanzarote",         "isla",
  "ES709",         "Tenerife",          "isla"
)

# Función para leer los ods
read_ods_csv <- function(path, indicator_id) {
  if (!file.exists(path)) {
    cli::cli_alert_warning("No existe el fichero ODS: {path}")
    return(tibble())
  }

  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    transmute(
      year = as.integer(year),
      territory_code = as.character(territorio),
      units = as.character(units),
      value = to_numeric_aemet(value),
      indicator_id = indicator_id
    ) |>
    left_join(territory_map, by = "territory_code")
}

# Dataframe para los ods
ods_files <- tribble(
  ~path,                              ~indicator_id,
  # ODS 7
  "Datos/ODS/7-2-1-SERIE-A.csv",      "ods_7_2_1",
  "Datos/ODS/7-3-1-SERIE-A.csv",      "ods_7_3_1",
  # ODS 13
  "Datos/ODS/13-1-1-SERIE-A.csv",     "ods_13_1_1",
  "Datos/ODS/13-2-2-SERIE-A.csv",     "ods_13_2_2_A",
  "Datos/ODS/13-2-2-SERIE-B.csv",     "ods_13_2_2_B",
  "Datos/ODS/13-2-2-SERIE-C.csv",     "ods_13_2_2_C",
  "Datos/ODS/13-2-2-SERIE-D.csv",     "ods_13_2_2_D",
  # ODS 14
  "Datos/ODS/14-3-1-SERIE-A.csv",     "ods_14_3_1",
  # ODS 15
  "Datos/ODS/15-1-1-SERIE-A.csv",     "ods_15_1_1",
  "Datos/ODS/15-1-2-SERIE-A.csv",     "ods_15_1_2_A",
  "Datos/ODS/15-1-2-SERIE-B.csv",     "ods_15_1_2_B",
  "Datos/ODS/15-1-2-SERIE-C.csv",     "ods_15_1_2_C",
  "Datos/ODS/15-1-2-SERIE-D.csv",     "ods_15_1_2_D",
  "Datos/ODS/15-1-2-SERIE-E.csv",     "ods_15_1_2_E",
  "Datos/ODS/15-1-2-SERIE-F.csv",     "ods_15_1_2_F",
  "Datos/ODS/15-1-2-SERIE-G.csv",     "ods_15_1_2_G",
  "Datos/ODS/15-2-1-SERIE-A.csv",     "ods_15_2_1_A",
  "Datos/ODS/15-2-1-SERIE-B.csv",     "ods_15_2_1_B",
  "Datos/ODS/15-4-1-SERIE-A.csv",     "ods_15_4_1_A",
  "Datos/ODS/15-4-1-SERIE-B.csv",     "ods_15_4_1_B",
  "Datos/ODS/15-4-1-SERIE-C.csv",     "ods_15_4_1_C",
  "Datos/ODS/15-4-1-SERIE-D.csv",     "ods_15_4_1_D",
  "Datos/ODS/15-4-1-SERIE-E.csv",     "ods_15_4_1_E",
  "Datos/ODS/15-4-1-SERIE-F.csv",     "ods_15_4_1_F",
  "Datos/ODS/15-4-1-SERIE-G.csv",     "ods_15_4_1_G",
  "Datos/ODS/15-4-2-SERIE-A.csv",     "ods_15_4_2",
  "Datos/ODS/15-8-1-SERIE-A.csv",     "ods_15_8_1"
)

ods_long <- purrr::map2_dfr(ods_files$path, ods_files$indicator_id, read_ods_csv)

ods_coverage <- ods_long |>
  group_by(indicator_id, level) |>
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_years  = n_distinct(year),
    n_territories = n_distinct(territory_code),
    coverage_class = case_when(
      n_years >= 8 ~ "core",
      n_years >= 4 ~ "extended",
      TRUE ~ "sparse"
    ),
    .groups = "drop"
  ) |>
  arrange(level, desc(n_years), indicator_id)

ods_dict <- ods_long |>
  distinct(indicator_id, units, level) |>
  left_join(ods_coverage, by = c("indicator_id", "level"))

safe_write_csv(ods_long, file.path(base_dir, "processed", "ods_long.csv"))
safe_write_csv(ods_coverage, file.path(out_dir, "tables", "tabla_01_cobertura_ods.csv"))
safe_write_csv(ods_dict, file.path(out_dir, "tables", "tabla_02_diccionario_ods.csv"))

# Separamos los ODS por nivel territorial
ods_isla <- ods_long |> filter(level == "isla")
ods_canarias <- ods_long |> filter(level == "canarias")
ods_es <- ods_long |> filter(level == "pais")

# Construimos un dataset analítico a nivel isla-año
ods_isla_wide <- ods_isla |>
  select(year, island = territory_name, indicator_id, value) |>
  pivot_wider(names_from = indicator_id, values_from = value)

model_island_year <- climate_model_table |>
  mutate(year = as.integer(year), island = as.character(island)) |>
  left_join(ods_isla_wide, by = c("island", "year")) |>
  arrange(island, year)

# Dataset a nivel isla-año
climate_canarias_year <- climate_model_table |>
  group_by(year) |>
  summarise(
    n_islands = n_distinct(island),
    tmed_mean = mean(tmed_annual, na.rm = TRUE),
    tmin_mean = mean(tmin_annual, na.rm = TRUE),
    tmax_mean = mean(tmax_annual, na.rm = TRUE),
    prec_mean = mean(prec_annual, na.rm = TRUE),
    hrmin_mean = mean(hr_min_annual, na.rm = TRUE),
    hrmed_mean = mean(hr_media_annual, na.rm = TRUE),
    hrmax_mean = mean(hr_max_annual, na.rm = TRUE),
    velmed_mean = mean(velmedia_annual, na.rm = TRUE),
    
    presmin_mean = mean(pres_min_annual, na.rm = TRUE),
    presmax_mean = mean(pres_max_annual, na.rm = TRUE),
    
    tropical_nights_mean = mean(tropical_nights_annual, na.rm = TRUE),
    warm_days_30_mean = mean(warm_days_30, na.rm = TRUE),
    anom_tmed_mean = mean(anom_tmed, na.rm = TRUE),
    .groups = "drop"
  )

ods_canarias_wide <- ods_canarias |>
  select(year, indicator_id, value) |>
  pivot_wider(names_from = indicator_id, values_from = value)

model_canarias_year <- climate_canarias_year |>
  left_join(ods_canarias_wide, by = "year") |>
  arrange(year)

safe_write_csv(model_island_year, file.path(base_dir, "processed", "model_island_year.csv"))
safe_write_csv(model_canarias_year, file.path(base_dir, "processed", "model_canarias_year.csv"))
safe_write_csv(model_island_year, file.path(out_dir, "tables", "dataset_model_island_year.csv"))
safe_write_csv(model_canarias_year, file.path(out_dir, "tables", "dataset_model_canarias_year.csv"))

# -----------------------------
# Resumen de calidad y cobertura
# -----------------------------

tabla_resumen_datasets <- tibble(
  dataset = c("model_island_year", "model_canarias_year"),
  unidad = c("isla-año", "Canarias-año"),
  n_filas = c(nrow(model_island_year), nrow(model_canarias_year)),
  n_columnas = c(ncol(model_island_year), ncol(model_canarias_year)),
  min_year = c(min(model_island_year$year, na.rm = TRUE), min(model_canarias_year$year, na.rm = TRUE)),
  max_year = c(max(model_island_year$year, na.rm = TRUE), max(model_canarias_year$year, na.rm = TRUE))
)

coverage_island <- model_island_year |>
  group_by(island) |>
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    n_years = n_distinct(year),
    mean_n_stations = mean(n_stations, na.rm = TRUE),
    missing_tmed = mean(is.na(tmed_annual)),
    .groups = "drop"
  ) |>
  arrange(island)

missing_island <- model_island_year |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") |>
  arrange(desc(missing_pct))

missing_canarias <- model_canarias_year |>
  summarise(across(everything(), ~ mean(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "missing_pct") |>
  arrange(desc(missing_pct))

safe_write_csv(tabla_resumen_datasets, file.path(out_dir, "tables", "tabla_03_resumen_datasets.csv"))
safe_write_csv(coverage_island, file.path(out_dir, "tables", "tabla_04_cobertura_por_isla.csv"))
safe_write_csv(missing_island, file.path(out_dir, "tables", "tabla_05_missing_model_island_year.csv"))
safe_write_csv(missing_canarias, file.path(out_dir, "tables", "tabla_06_missing_model_canarias_year.csv"))

# -----------------------------
# Análisis exploratorio climático
# -----------------------------

model_island_year <- model_island_year |>
  mutate(
    island = as.factor(island),
    year = as.integer(year),
    date = as.Date(paste0(year, "-01-01"))
  )

p_tmed <- ggplot(model_island_year, aes(x = year, y = tmed_annual, color = island)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Temperatura media anual por isla", x = "Año", y = "Temperatura media anual (°C)", color = "Isla") +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 1))
save_plot(p_tmed, "figura_03_tmed_anual_por_isla.png")

p_tmin <- ggplot(model_island_year, aes(x = year, y = tmin_annual, color = island)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Temperatura mínima anual por isla", x = "Año", y = "Temperatura mínima anual (°C)", color = "Isla") +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 1))
save_plot(p_tmin, "figura_04_tmin_anual_por_isla.png")

p_anom <- ggplot(model_island_year, aes(x = year, y = anom_tmed, color = island)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Anomalía de temperatura media por isla", x = "Año", y = "Anomalía térmica (°C)", color = "Isla") +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 1))
save_plot(p_anom, "figura_05_anomalia_tmed_por_isla.png")

p_tropical <- ggplot(model_island_year, aes(x = year, y = tropical_nights_annual, color = island)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Noches tropicales anuales por isla", x = "Año", y = "Noches tropicales anuales", color = "Isla") +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 1))
save_plot(p_tropical, "figura_06_noches_tropicales_por_isla.png")

p_prec <- ggplot(model_island_year, aes(x = year, y = prec_annual, color = island)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  theme_minimal() +
  labs(title = "Precipitación anual por isla", x = "Año", y = "Precipitación anual media por estación (mm)", color = "Isla") +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 1))
save_plot(p_prec, "figura_07_precipitacion_anual_por_isla.png")

tendencias_isla <- model_island_year |>
  group_by(island) |>
  group_modify(~{
    fit <- lm(tmed_annual ~ year, data = .x)
    broom::tidy(fit) |>
      filter(term == "year") |>
      transmute(
        trend_deg_per_year = estimate,
        trend_deg_per_decade = estimate * 10,
        p_value = p.value,
        r_squared = broom::glance(fit)$r.squared
      )
  }) |>
  ungroup() |>
  arrange(desc(trend_deg_per_decade))

safe_write_csv(tendencias_isla, file.path(out_dir, "tables", "tabla_07_tendencias_lineales_por_isla.csv"))

# -----------------------------
# Forecast temporal hasta 2030 por isla
# -----------------------------

ts_island <- model_island_year |>
  select(year, island, tmed_annual) |>
  drop_na(tmed_annual) |>
  as_tsibble(key = island, index = year)

max_year_ts <- max(ts_island$year, na.rm = TRUE)
horizon_2030 <- max(FORECAST_END_YEAR - max_year_ts, 1L)
h_test <- min(2L, max(1L, length(unique(ts_island$year)) - 3L))

train_ts <- ts_island |>
  filter(year <= max_year_ts - h_test)

test_ts <- ts_island |>
  filter(year > max_year_ts - h_test)

fits_train_ts <- train_ts |>
  model(
    naive = NAIVE(tmed_annual),
    drift = RW(tmed_annual ~ drift()),
    ets = ETS(tmed_annual),
    arima = ARIMA(tmed_annual),
    arima_010_const = ARIMA(tmed_annual ~ 1 + pdq(0, 1, 0) + PDQ(0, 0, 0)),
    tslm = TSLM(tmed_annual ~ trend())
  )

fc_test_ts <- fits_train_ts |>
  forecast(h = h_test)

forecast_flatness <- fc_test_ts %>%
  as_tibble() %>%
  group_by(island, .model) %>%
  summarise(
    forecast_sd = sd(.mean, na.rm = TRUE),
    forecast_range = max(.mean, na.rm = TRUE) - min(.mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    is_flat = forecast_range < 0.01
  )

accuracy_test_ts <- fc_test_ts |>
  accuracy(test_ts) |>
  arrange(RMSE)

accuracy_test_summary <- accuracy_test_ts |>
  group_by(.model) |>
  summarise(
    mean_rmse = mean(RMSE, na.rm = TRUE),
    mean_mae = mean(MAE, na.rm = TRUE),
    mean_mape = mean(MAPE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(mean_rmse)

best_ts_model <- accuracy_test_ts %>%
  left_join(forecast_flatness, by = c("island", ".model")) %>%
  filter(!is_flat | .model %in% c("drift", "tslm", "arima_010_const")) %>%
  group_by(island) %>%
  arrange(RMSE, MAE, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>% 
  pull(.model) %>% unique()

fits_full_ts <- ts_island |>
  model(
    naive = NAIVE(tmed_annual),
    drift = RW(tmed_annual ~ drift()),
    ets = ETS(tmed_annual),
    arima = ARIMA(tmed_annual),
    arima_010_const = ARIMA(tmed_annual ~ 1 + pdq(0, 1, 0) + PDQ(0, 0, 0)),
    tslm = TSLM(tmed_annual ~ trend())
  )

fc_2030_all <- fits_full_ts |>
  forecast(h = horizon_2030)

fc_2030_best <- fc_2030_all |>
  filter(.model == best_ts_model)

pred_2030_table <- fc_2030_best |>
  as_tibble() |>
  filter(year == FORECAST_END_YEAR) |>
  transmute(
    island,
    model = .model,
    year,
    pred_tmed_annual = .mean
  ) |>
  arrange(island)

safe_write_csv(accuracy_test_ts, file.path(out_dir, "tables", "tabla_08_accuracy_forecast_temporal_por_isla.csv"))
safe_write_csv(accuracy_test_summary, file.path(out_dir, "tables", "tabla_09_accuracy_forecast_temporal_resumen.csv"))
safe_write_csv(pred_2030_table, file.path(out_dir, "tables", "tabla_10_prediccion_tmed_2030_por_isla.csv"))
safe_write_rds(fits_full_ts, file.path(out_dir, "models", "modelos_temporales_fable_tmed.rds"))

fc_trend <- fc_test_ts %>%
  filter(.model %in% c("drift", "arima_010_const", "tslm"))

p_fc_test <- autoplot(fc_trend, train_ts) +
  autolayer(test_ts, tmed_annual, color = "black") +
  facet_wrap(~ island, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Validación temporal de modelos de predicción",
    subtitle = paste0("Entrenamiento hasta ", max_year_ts - h_test, "; test: últimos ", h_test, " años"),
    x = "Año",
    y = "Temperatura media anual (°C)"
  ) +
  scale_x_continuous(breaks = seq(min(model_island_year$year), max(model_island_year$year), 2))
save_plot(p_fc_test, "figura_08_validacion_forecast_temporal.png", width = 12, height = 8)

p_fc_2030 <- autoplot(fc_2030_best, ts_island) +
  facet_wrap(~ island, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Predicción de temperatura media anual por isla hasta 2030",
    subtitle = paste0("Modelo temporal seleccionado por RMSE medio: ", best_ts_model),
    x = "Año",
    y = "Temperatura media anual (°C)"
  )
save_plot(p_fc_2030, "figura_09_forecast_tmed_2030_modelo_seleccionado.png", width = 12, height = 8)

p_fc_all <- autoplot(fc_2030_all, ts_island) +
  facet_wrap(~ island, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Predicción de temperatura media anual hasta 2030",
    subtitle = "Comparación de modelos: naive, drift, ETS, ARIMA y tendencia lineal",
    x = "Año",
    y = "Temperatura media anual (°C)"
  )
save_plot(p_fc_all, "figura_10_forecast_tmed_2030_comparacion_modelos.png", width = 12, height = 8)

# Forecast complementario para tmin y noches tropicales.
forecast_target_by_island <- function(data, target, filename_prefix, y_label) {
  ts_data <- data |>
    select(year, island, value = all_of(target)) |>
    drop_na(value) |>
    as_tsibble(key = island, index = year)

  h <- max(FORECAST_END_YEAR - max(ts_data$year, na.rm = TRUE), 1L)

  fits <- ts_data |>
    model(
      naive = NAIVE(value),
      drift = RW(value ~ drift()),
      ets = ETS(value),
      arima = ARIMA(value),
      arima_010_const = ARIMA(value ~ 1 + pdq(0, 1, 0) + PDQ(0, 0, 0)),
      tslm = TSLM(value ~ trend())
    )

  fc <- fits |> forecast(h = h)

  p <- autoplot(fc, ts_data) +
    facet_wrap(~ island, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste0("Forecast de ", y_label, " por isla hasta 2030"),
      x = "Año",
      y = y_label
    )

  save_plot(p, paste0(filename_prefix, ".png"), width = 12, height = 8)
  safe_write_rds(fits, file.path(out_dir, "models", paste0(filename_prefix, ".rds")))

  invisible(fc)
}

fc_tmin <- forecast_target_by_island(model_island_year, "tmin_annual", "figura_11_forecast_tmin_2030", "temperatura mínima anual")
fc_tropical <- forecast_target_by_island(model_island_year, "tropical_nights_annual", "figura_12_forecast_noches_tropicales_2030", "noches tropicales anuales")

# -----------------------------
# Machine learning panel isla-año
# -----------------------------

ml_panel <- model_island_year |>
  arrange(island, year) |>
  group_by(island) |>
  mutate(
    year_trend = year - min(year, na.rm = TRUE),
    lag_tmed_1 = lag(tmed_annual, 1),
    lag_tmed_2 = lag(tmed_annual, 2),
    lag_tmin_1 = lag(tmin_annual, 1),
    lag_tmax_1 = lag(tmax_annual, 1),
    lag_prec_1 = lag(prec_annual, 1),
    lag_hr_media_1 = lag(hr_media_annual, 1),
    lag_velmedia_1 = lag(velmedia_annual, 1),
    lag_sol_1 = lag(sol_annual, 1),
    lag_pres_min_1 = lag(pres_min_annual, 1),
    lag_pres_max_1 = lag(pres_max_annual, 1),
    roll_tmed_3 = slider::slide_dbl(tmed_annual, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    roll_prec_3 = slider::slide_dbl(prec_annual, mean, .before = 2, .complete = TRUE, na.rm = TRUE),
    roll_hr_media_3 = slider::slide_dbl(hr_media_annual, mean, .before = 2, .complete = TRUE, na.rm = TRUE)
  ) |>
  ungroup() |>
  drop_na(tmed_annual, lag_tmed_1)

# ODS 7.2.1 se conserva si existe; si no existe, el recipe ignora columnas ausentes.
ml_predictors <- c(
  "year_trend", "island", "lag_tmed_1", "lag_tmed_2", "lag_tmin_1", "lag_tmax_1",
  "lag_prec_1", "lag_hr_media_1", "lag_velmedia_1", "lag_sol_1", "lag_pres_min_1",
  "lag_pres_max_1","roll_tmed_3", "roll_prec_3", "ods_7_2_1"
)
ml_predictors <- ml_predictors[ml_predictors %in% names(ml_panel)]

ml_data <- ml_panel |>
  select(tmed_annual, all_of(ml_predictors), year)

ml_test_years <- sort(unique(ml_data$year), decreasing = TRUE)[seq_len(min(2, length(unique(ml_data$year))))]
train_ml <- ml_data |> filter(!year %in% ml_test_years)
test_ml <- ml_data |> filter(year %in% ml_test_years)

rec_ml <- recipe(tmed_annual ~ ., data = train_ml |> select(-year)) |>
  step_impute_median(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

models_ml <- list(
  lm_regularized = linear_reg(penalty = 0.01, mixture = 0.5) |>
    set_engine("glmnet") |>
    set_mode("regression"),
  random_forest = rand_forest(trees = 500, min_n = 3) |>
    set_engine("ranger", importance = "permutation") |>
    set_mode("regression"),
  xgboost = boost_tree(trees = 500, learn_rate = 0.05, tree_depth = 3, min_n = 3) |>
    set_engine("xgboost") |>
    set_mode("regression")
)

fit_ml_model <- function(model_spec, model_name) {
  wf <- workflow() |>
    add_recipe(rec_ml) |>
    add_model(model_spec)

  fit <- fit(wf, data = train_ml |> select(-year))

  preds <- predict(fit, new_data = test_ml |> select(-tmed_annual, -year)) |>
    bind_cols(test_ml |> select(tmed_annual, year)) |>
    mutate(model = model_name)

  mets <- yardstick::metrics(preds, truth = tmed_annual, estimate = .pred) |>
    mutate(model = model_name)

  list(fit = fit, preds = preds, metrics = mets)
}

ml_results <- purrr::imap(models_ml, fit_ml_model)
ml_metrics <- purrr::map_dfr(ml_results, "metrics") |>
  select(model, .metric, .estimator, .estimate) |>
  arrange(.metric, .estimate)
ml_predictions <- purrr::map_dfr(ml_results, "preds")

safe_write_csv(ml_metrics, file.path(out_dir, "tables", "tabla_11_metricas_ml_panel_holdout.csv"))
safe_write_csv(ml_predictions, file.path(out_dir, "tables", "tabla_12_predicciones_ml_panel_holdout.csv"))
safe_write_rds(ml_results, file.path(out_dir, "models", "modelos_ml_panel_tmed.rds"))

p_ml_obs_pred <- ggplot(ml_predictions, aes(x = tmed_annual, y = .pred, color = model)) +
  geom_point(size = 2) +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Modelos ML panel: observado vs predicho",
    subtitle = paste0("Test temporal: años ", paste(sort(ml_test_years), collapse = ", ")),
    x = "Temperatura observada",
    y = "Temperatura predicha",
    color = "Modelo"
  )
save_plot(p_ml_obs_pred, "figura_13_ml_panel_observado_vs_predicho.png", width = 8, height = 6)

# Importancia de variables del Random Forest si está disponible.
rf_fit <- ml_results$random_forest$fit
rf_engine <- tryCatch(extract_fit_engine(rf_fit), error = function(e) NULL)
if (!is.null(rf_engine)) {
  p_vip <- vip::vip(rf_engine, num_features = 15) +
    theme_minimal() +
    labs(title = "Importancia de variables - Random Forest panel")
  save_plot(p_vip, "figura_14_importancia_variables_random_forest.png", width = 8, height = 6)
}

# -----------------------------
# Relación ODS insular 7.2.1 y temperatura
# -----------------------------

if ("ods_7_2_1" %in% names(model_island_year)) {
  island_core <- model_island_year |>
    drop_na(ods_7_2_1, anom_tmed) |>
    mutate(
      year_c = year - min(year, na.rm = TRUE),
      ods_7_2_1_z = as.numeric(scale(ods_7_2_1))
    )

  if (nrow(island_core) >= 10) {
    m_lm_island <- lm(anom_tmed ~ ods_7_2_1_z + year_c + island, data = island_core)
    m_lm_tropical <- lm(tropical_nights_annual ~ ods_7_2_1_z + year_c + island, data = island_core)

    safe_write_csv(broom::tidy(m_lm_island), file.path(out_dir, "tables", "tabla_13_modelo_ods721_anom_tmed_coeficientes.csv"))
    safe_write_csv(broom::glance(m_lm_island), file.path(out_dir, "tables", "tabla_14_modelo_ods721_anom_tmed_ajuste.csv"))
    safe_write_csv(broom::tidy(m_lm_tropical), file.path(out_dir, "tables", "tabla_15_modelo_ods721_noches_tropicales_coeficientes.csv"))

    p_ods_721 <- ggplot(island_core, aes(x = ods_7_2_1, y = anom_tmed, color = island)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal() +
      labs(
        title = "Relación entre ODS 7.2.1 y anomalía térmica por isla",
        subtitle = "Asociación exploratoria; no implica causalidad",
        x = "ODS 7.2.1: proporción de renovables en producción eléctrica",
        y = "Anomalía de temperatura media (°C)",
        color = "Isla"
      )
    save_plot(p_ods_721, "figura_15_ods_721_vs_anom_tmed.png", width = 9, height = 6)
  }
}

# -----------------------------
# Análisis regional Canarias-año con ODS 7, 13, 14 y 15
# -----------------------------

ods_vars <- names(model_canarias_year) |>
  stringr::str_subset("^ods_")

coverage_canarias <- model_canarias_year |>
  summarise(across(all_of(ods_vars), ~ mean(!is.na(.)))) |>
  pivot_longer(everything(), names_to = "indicator", values_to = "coverage") |>
  mutate(
    coverage_class = case_when(
      coverage >= 0.80 ~ "core",
      coverage >= 0.60 ~ "extended",
      TRUE ~ "sparse"
    )
  ) |>
  arrange(desc(coverage), indicator)

ods_core_vars <- coverage_canarias |>
  filter(coverage >= 0.60) |>
  pull(indicator)

cor_canarias <- purrr::map_dfr(ods_core_vars, function(v) {
  df <- model_canarias_year |>
    select(year, tmed_mean, all_of(v)) |>
    drop_na()

  if (nrow(df) < 6 || sd(df[[v]], na.rm = TRUE) == 0 || sd(df$tmed_mean, na.rm = TRUE) == 0) {
    return(tibble(indicator = v, n = nrow(df), spearman = NA_real_, p_value = NA_real_))
  }

  ct <- cor.test(df[[v]], df$tmed_mean, method = "spearman", exact = FALSE)

  tibble(
    indicator = v,
    n = nrow(df),
    spearman = unname(ct$estimate),
    p_value = ct$p.value
  )
}) |>
  arrange(desc(abs(spearman)))

regional_models <- purrr::map_dfr(ods_core_vars, function(v) {
  df <- model_canarias_year |>
    select(year, tmed_mean, all_of(v)) |>
    drop_na()

  if (nrow(df) < 6 || sd(df[[v]], na.rm = TRUE) == 0) {
    return(tibble(indicator = v, n = nrow(df), adj_r2 = NA_real_, aic = NA_real_))
  }

  fit <- lm(as.formula(paste("tmed_mean ~", v, "+ year")), data = df)

  tibble(
    indicator = v,
    n = nrow(df),
    adj_r2 = summary(fit)$adj.r.squared,
    aic = AIC(fit)
  )
}) |>
  arrange(desc(adj_r2))

safe_write_csv(coverage_canarias, file.path(out_dir, "tables", "tabla_16_cobertura_ods_canarias.csv"))
safe_write_csv(cor_canarias, file.path(out_dir, "tables", "tabla_17_correlaciones_ods_tmed_canarias.csv"))
safe_write_csv(regional_models, file.path(out_dir, "tables", "tabla_18_modelos_regionales_simples_ods_tmed.csv"))

# Diccionario simple indicador-ODS para interpretación en memoria
indicator_goal_map <- tibble::tribble(
  ~indicator, ~ods,
  "ods_7_2_1", "ODS 7",
  "ods_7_3_1", "ODS 7",
  "ods_13_1_1", "ODS 13",
  "ods_13_2_2_A", "ODS 13",
  "ods_13_2_2_B", "ODS 13",
  "ods_13_2_2_C", "ODS 13",
  "ods_13_2_2_D", "ODS 13",
  "ods_14_3_1", "ODS 14",
  "ods_15_1_1", "ODS 15",
  "ods_15_1_2_A", "ODS 15",
  "ods_15_1_2_B", "ODS 15",
  "ods_15_1_2_C", "ODS 15",
  "ods_15_1_2_D", "ODS 15",
  "ods_15_1_2_E", "ODS 15",
  "ods_15_1_2_F", "ODS 15",
  "ods_15_1_2_G", "ODS 15",
  "ods_15_2_1_A", "ODS 15",
  "ods_15_2_1_B", "ODS 15",
  "ods_15_4_1_A", "ODS 15",
  "ods_15_4_1_B", "ODS 15",
  "ods_15_4_1_C", "ODS 15",
  "ods_15_4_1_D", "ODS 15",
  "ods_15_4_1_E", "ODS 15",
  "ods_15_4_1_F", "ODS 15",
  "ods_15_4_1_G", "ODS 15",
  "ods_15_4_2", "ODS 15",
  "ods_15_8_1", "ODS 15"
)

cor_canarias <- cor_canarias |>
  left_join(indicator_goal_map, by = "indicator") |>
  relocate(ods, .after = indicator)

regional_models <- regional_models |>
  left_join(indicator_goal_map, by = "indicator") |>
  relocate(ods, .after = indicator)

cor_canarias_by_ods <- cor_canarias |>
  filter(!is.na(spearman)) |>
  group_by(ods) |>
  summarise(
    n_indicators = n(),
    mean_abs_spearman = mean(abs(spearman), na.rm = TRUE),
    max_abs_spearman = max(abs(spearman), na.rm = TRUE),
    best_indicator = indicator[which.max(abs(spearman))],
    .groups = "drop"
  ) |>
  arrange(desc(max_abs_spearman))

p_cor_ods <- cor_canarias |>
  filter(!is.na(spearman)) |>
  ggplot(aes(x = reorder(indicator, spearman), y = spearman, fill = ods)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Correlación entre indicadores ODS ambientales y temperatura media en Canarias",
    subtitle = "Correlación de Spearman; análisis regional exploratorio",
    x = "Indicador ODS",
    y = "Spearman",
    fill = "Objetivo"
  )

save_plot(p_cor_ods, "figura_16_correlaciones_ods_tmed_canarias.png", width = 9, height = 7)
safe_write_csv(cor_canarias_by_ods, file.path(out_dir, "tables", "tabla_19_resumen_correlaciones_por_ods.csv"))


# -----------------------------
# Resumen final
# -----------------------------
quality_summary <- c(
  paste0("Periodo climático: ", YEAR_START, "-", YEAR_END),
  paste0("Año final de forecast: ", FORECAST_END_YEAR),
  paste0("Estaciones seleccionadas inicialmente: ", length(selected_station_ids)),
  paste0("Filas diarias raw: ", nrow(raw_daily)),
  paste0("Filas diarias limpias: ", nrow(clean_daily)),
  paste0("Estación-año válidos: ", nrow(annual_station_good)),
  paste0("Isla-año generados: ", nrow(annual_island)),
  paste0("Modelo temporal seleccionado: ", best_ts_model),
  paste0("Dataset isla-año: ", nrow(model_island_year), " filas"),
  paste0("Dataset Canarias-año: ", nrow(model_canarias_year), " filas")
)

writeLines(quality_summary, con = file.path(base_dir, "metadata", "resumen_calidad_y_analisis.txt"))
writeLines(quality_summary, con = file.path(out_dir, "tables", "resumen_calidad_y_analisis.txt"))



