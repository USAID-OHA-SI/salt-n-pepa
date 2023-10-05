##  PROJECT: PUSH Countries 8 + 1 + 1
##  AUTHOR:  Baboyma Kagniniwa | USAID
##  PURPOSE: TX ART Saturation over time
##  REF. ID: a0acc5b1
##  LICENCE: MIT
##  DATE:    2023-10-03
##  Updated: 2023-10-03


# DEPENDENCIES ----

  library(tidyverse)
  library(readxl)
  library(janitor)
  library(gophr)
  library(glitr)
  library(glamr)
  library(gisr)
  library(grabr)
  library(extrafont)
  library(scales)
  library(ggtext)
  library(sf)
  #library(ggrepel)
  #library(ggnewscale)
  library(patchwork)
  library(glue)

# SETUP ----

  dir_data <- "Data"
  dir_dataout <- "Dataout"
  dir_gis <- "GIS"
  dir_graphics <- "Graphics"

  dir_geodata <- si_path("path_vector")
  dir_terr <- si_path("path_raster")
  dir_merdata <- si_path("path_msd")

  # MER Data - get the latest MSD PSNU x IM file
  file_psnu_im <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_PSNU_IM_FY21.*\\d.zip$")

  # NAT Data - get the latest NAT_SUBNAT file
  file_natsub <- dir_merdata %>%
    return_latest(pattern = "^MER_.*_NAT_SUBNAT_FY21.*\\d.zip$")

  # Shapefile path
  file_shp <- dir_geodata %>%
    return_latest(
      pattern = "VcPepfarPolygons.*.shp",
      recursive = TRUE
    )

# GLOBALS ----

  # Params

  ou <- "Nigeria"
  cntry <- "Nigeria"
  agency <- "USAID"
  ref_id <- "a0acc5b1"

  get_metadata(file_psnu_im)

  meta <- metadata


# FUNCTIONS ----

  #' @title generate coverage map
  #'
  #'
  generate_cov <- function(.data, terr, spdf, cntry) {

    # Extract admin 0 and 1 for base map
    admin0 <- spdf %>%
      filter(str_trim(regionorcountry_name) == str_trim(cntry),
             orgunit_label == "country")

    admin1 <- spdf %>%
      filter(str_trim(regionorcountry_name) == str_trim(cntry),
             orgunit_label == "prioritization") %>%
      clean_column(colname = "orgunit_name")

    # Filter country orgs
    .data <- .data %>%
      filter(regionorcountry_name == cntry)

    lbls <- .data %>%
      st_drop_geometry() %>%
      distinct(orgunit_label)

    if (length(lbls) > 1 & "country" %in% lbls) {
      .data <- .data %>%
        filter(orgunit_label != "country")
    }

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    # Return base map only when there is no data

    if (nrow(.data) == 0) return(basemap)

    # Generate cov map

    map_cov <- basemap +
      geom_sf(data = .data,
              aes(fill = agency_color),
              lwd = .3,
              color = grey10k) +
      geom_sf(data = admin0,
              colour = grey10k,
              fill = NA,
              size = 1.5) +
      geom_sf(data = admin0,
              colour = grey90k,
              fill = NA,
              size = .3) +
      # geom_sf_text(data = .data,
      #              aes(label = str_replace_all(psnu, " ", "\n"),
      #                  color = label_color),
      #              size = 2) +
      scale_fill_identity() +
      #scale_color_identity() +
      labs(x = "", y = "",
           title = glue("{str_to_upper(cntry)} - AGENCIES COVERAGE"),
           subtitle = glue("<span style='color:{denim}'>**USAID**</span>, <span style='color:{denim_light}'>**CDC**</span> & <span style='color:{moody_blue}'>**1+ AGENCY**</span>")) +
      si_style_map() +
      theme(legend.position = "top",
            legend.title = element_blank(),
            plot.subtitle = element_markdown())

    if (cntry == "South Africa") {
      map_cov <- map_cov +
        ggplot2::xlim(c(15, 35)) +
        ggplot2::ylim(c(-38, -20))
    }

    return(map_cov)
  }

  #' @title generate map
  #'
  generate_map <- function(.data, terr, spdf, cntry) {

    # Param

    max <- .data %>%
      pull(art_sat) %>%
      max(na.rm = T)

    has_data <- ifelse(max == -Inf, F, T)

    print(glue("Max ART SAT = {max}"))

    max <- ifelse(max < 1, 1, max)

    sep <- ifelse(max > 1.25, .5, .25)

    no_psnu_in_lbls <- c("South Africa", "Zambia", "Tanzania", "Mozambique")

    # Geom

    # Extract admin 0 and 1 for base map
    admin0 <- spdf %>%
      filter(regionorcountry_name == cntry,
             orgunit_label == "country")

    admin1 <- spdf %>%
      filter(regionorcountry_name == cntry,
             orgunit_label == "prioritization")

    spdf_art <- .data %>%
      filter(regionorcountry_name == cntry,
             str_detect(orgunit_name, "_Mil", negate = T),
             period == meta$curr_pd)

    # Produce basemap
    basemap <- terrain_map(countries = admin0,
                           adm0 = admin0,
                           adm1 = admin1,
                           mask = TRUE,
                           terr = terr)

    map_art_sat <- basemap +
      geom_sf(data = .data,
              aes(fill = art_sat),
              lwd = .3,
              color = grey10k,
              show.legend = has_data) +
      scale_fill_si(
        palette = "burnt_siennas",
        discrete = FALSE,
        alpha = 0.7,
        na.value = NA,
        breaks = seq(0, max, sep),
        limits = c(0, max),
        labels = percent
      ) +
      geom_sf(data = admin0,
              colour = grey10k,
              fill = NA,
              size = 1.5) +
      geom_sf(data = admin0,
              colour = grey90k,
              fill = NA,
              size = .3)

    # Remove psnu from labels
    if (cntry %in% no_psnu_in_lbls) {

      map_art_sat <- map_art_sat +
        geom_sf_text(data = .data,
                     aes(label = percent(art_sat, 1)),
                     size = 2,
                     color = grey10k)
    }
    else {

      map_art_sat <- map_art_sat +
        geom_sf_text(data = .data,
                     aes(label = case_when(
                       is.na(art_sat) ~ psnu,
                       TRUE ~ paste0(psnu, "\n(", percent(art_sat, 1), ")"))),
                     size = 2,
                     color = grey10k)
    }

    map_art_sat <- map_art_sat +
      labs(x = "", y = "") +
      si_style_map() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.key.height = unit(.4, "cm"),
            legend.key.width = unit(1.5, "cm"))

    if (cntry == "South Africa") {
      map_art_sat <- map_art_sat +
        ggplot2::xlim(c(15, 35)) +
        ggplot2::ylim(c(-38, -20))
    }

    return(map_art_sat)
  }

  #' @title generate dumbbell
  #'
  generate_dumbbell <- function(.data, cntry) {

    # Filter data
    .data_cntry <- .data %>% filter(country == cntry)

    # DOT Change
    dots_art_change <- .data_cntry %>%
      ggplot(aes(x = reorder(psnu, FY23Q3), y = FY22Q4)) +
      geom_hline(yintercept = .9,
                 lty = "dashed",
                 lwd = 1,
                 color = usaid_darkgrey) +
      geom_segment(aes(xend = psnu,
                       y = FY22Q4,
                       yend = FY23Q3,
                       color = FY23Q3),
                   size = 1, alpha = .7,
                   show.legend = FALSE) +
      geom_point(aes(y = FY22Q4),
                 shape = 21,
                 fill = grey50k,
                 size = 4 ,
                 color = grey10k) +
      geom_point(aes(y = FY23Q3, fill = FY23Q3),
                 shape = 21, size = 5,
                 color = grey10k,
                 show.legend = F) +
      scale_fill_si(
        palette = "burnt_siennas",
        discrete = FALSE,
        alpha = 1,
        breaks = c(.25, .5, .75, 1)
      ) +
      scale_color_si(
        palette = "burnt_siennas",
        discrete = FALSE
      ) +
      scale_y_continuous(labels = percent, position = "right") +
      #scale_color_identity() +
      coord_flip() +
      labs(x = "", y = "") +
      si_style() +
      theme(axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 12))

    return(dots_art_change)
  }

# LOAD DATA ----

  # MER Data

  df_nat <- file_natsub %>% read_psd()

  df_psnu <- file_psnu_im %>% read_psd()

  # SPATIAL DATA

  terr <- gisr::get_raster(path = dir_terr)

  spdf_pepfar <- file_shp %>% sf::read_sf()

  # PEPFAR PUSH Countries

  push_cntries <- pepfar_country_list %>%
    filter(pepfar_accel == T) %>%
    pull(country)

  # Orgunits level

  df_levels <- get_levels(
      username = datim_user(),
      password = datim_pwd()
    ) %>%
    pivot_longer(
      cols = where(is.numeric),
      names_to = "orgunit_label",
      values_to = "orgunit_level"
    )

  # Orgunits attributes

  df_attrs <- push_cntries %>%
    map_dfr(~grabr::datim_orgunits(
      cntry = .x,
      username = datim_user(),
      password = datim_pwd()
    ))

  df_push_attrs <- df_levels %>%
    filter(countryname %in% push_cntries) %>%
    select(countryname, orgunit_label, orgunit_level) %>%
    mutate(orgunit_level = as.character(orgunit_level)) %>%
    left_join(df_attrs, .,
              by = c("regionorcountry_name" = "countryname", "orgunit_level"),
              relationship = "many-to-many") %>%
    rename_with(.cols = contains("_internal_id"),
                .fn = ~str_replace(.x, "internal_id", "uid")) %>%
    relocate(orgunit_label, .after = orgunit_level) %>%
    select(-contains(c("_parent", "_code", "moh"))) %>%
    filter(str_detect(orgunit_name, "_Mil", negate = T))

  df_push_attrs %>% glimpse()
  df_push_attrs %>% distinct(regionorcountry_name)

# MUNGE ----

  # GEO - Append Orgunit attributes (cntry & psnu only)

  spdf_pepfar <- spdf_pepfar %>%
    left_join(df_push_attrs, by = c("uid" = "orgunit_uid")) %>%
    filter(!is.na(orgunit_name))

  spdf_pepfar %>%
    st_drop_geometry() %>%
    distinct(orgunit_label)

  # PLHIV

  df_plhiv <- df_nat %>%
    filter(
      fiscal_year %in% c(meta$curr_fy -1, meta$curr_fy),
      country %in% push_cntries,
      indicator %in% c("PLHIV"),
      standardizeddisaggregate == "Total Numerator"
    ) %>%
    summarise(across(targets, \(x) sum(x, na.rm = TRUE)),
              .by = c(fiscal_year, country, psnuuid, psnu, indicator)) %>%
    mutate(period = paste0("FY", str_sub(fiscal_year, 3, 4))) %>%
    relocate(period, .before = 1) %>%
    rename(value = targets) %>%
    select(-fiscal_year) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    rename_with(str_to_lower)

  # Agency coverage

  df_cov <- df_psnu %>%
    filter(fiscal_year %in% c(meta$curr_fy),
           country %in% push_cntries,
           funding_agency %ni% c("Dedup", "Default"),
           str_detect(psnu, "_Mil", negate = TRUE),
           indicatortype == "DSD",
           !is.na(cumulative)) %>%
    distinct(country, funding_agency, psnuuid, psnu) %>%
    clean_agency()

  df_cov <- df_cov %>%
    group_by(country, psnuuid, psnu) %>%
    mutate(
      funding_agency = case_when(
        n_distinct(funding_agency) > 1 ~ paste(funding_agency, collapse = ", "),
        TRUE ~ funding_agency
      )) %>%
    ungroup() %>%
    clean_psnu() %>%
    distinct(country, funding_agency, psnuuid, psnu)

  # Viral Load

  df_vl <- df_psnu %>%
    clean_agency() %>%
    clean_indicator() %>%
    filter(
      fiscal_year %in% c(meta$curr_fy -1, meta$curr_fy),
      country %in% push_cntries,
      funding_agency == agency,
      indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
      standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")
    ) %>%
    reshape_msd() %>%
    filter(period_type == "results") %>%
    summarise(across(value, \(x) sum(x, na.rm = TRUE)),
              .by = c(period, country, psnuuid, psnu, indicator)) %>%
    mutate(fiscal_year = str_sub(period, 1, 4)) %>%
    pivot_wider(names_from = indicator, values_from = value) %>%
    arrange(country, psnuuid, psnu, period) %>%
    group_by(country, psnuuid, psnu) %>%
    mutate(TX_CURR_LAG2 = lag(TX_CURR, n = 2, order_by = period)) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(VLC = TX_PVLS_D / TX_CURR_LAG2,
           VLS = TX_PVLS / TX_PVLS_D) %>%
    ungroup() %>%
    rename_with(str_to_lower)

  # TX Saturation

  df_tx <- df_vl %>%
    left_join(df_plhiv ,
              by = c("country", "psnuuid", "psnu", "fiscal_year" = "period"),
              keep = FALSE) %>%
    rowwise() %>%
    mutate(art_sat = tx_curr / plhiv) %>%
    ungroup() %>%
    clean_psnu()

  df_tx_viz <- df_tx %>%
    mutate(
      label = paste0(psnu, " (", comma(plhiv, 1), ")"),
        pd_order = as.integer(str_remove_all(period, "FY|Q")),
        period1 = paste0(str_replace(period, "Q", " Q"),
                         " (", percent(art_sat, 1),
                         ")"),
        period2 = case_when(
          endsWith(period, "1") ~ paste0("Dec `",
                                         as.integer(str_sub(period, 3, 4)) - 1),
          endsWith(period, "2") ~ str_replace(period, "FY", "Mar `"),
          endsWith(period, "3") ~ str_replace(period, "FY", "Jun `"),
          endsWith(period, "4") ~ str_replace(period, "FY", "Sep `")
        ),
        period2 = str_remove(period2, "Q.*"),
        period2 = fct_reorder(period2, pd_order))

  df_tx_viz %>%
    filter(period == meta$curr_pd) %>%
    count(country) %>%
    arrange(country) %>%
    prinf()

  df_tx_art_trend <- df_tx_viz %>%
    filter(period %in% c("FY22Q4", meta$curr_pd)) %>%
    select(period, country, psnu, art_sat) %>%
    distinct() %>%
    pivot_wider(names_from = period, values_from = art_sat) %>%
    mutate(
      psnu = case_when(
        is.na(FY23Q3) ~ psnu,
        TRUE ~ paste0(psnu, " (", percent(FY23Q3, 1), ")")
      ),
      change_color = if_else(FY23Q3 - FY22Q4 > 0, burnt_sienna, usaid_red)
    )

  # Join to spatial file

  spdf_tx <- spdf_pepfar %>%
    #filter(orgunit_label == "prioritization") %>%
    left_join(df_tx_viz,
              by = c("regionorcountry_name" = "country",
                     "uid" = "psnuuid"),
              relationship = "many-to-many") %>%
    filter(!is.na(art_sat), period == meta$curr_pd)

  spdf_vl <- spdf_pepfar %>%
    filter(orgunit_label == "prioritization") %>%
    left_join(df_vl,
              by = c("regionorcountry_name" = "country",
                     "uid" = "psnuuid")) %>%
    filter(!is.na(vls), period == meta$curr_pd)

  spdf_cov <- spdf_pepfar %>%
    left_join(df_cov, by = c("regionorcountry_name" = "country", "uid" = "psnuuid")) %>%
    mutate(
      agency_color = case_when(
        funding_agency == "USAID" ~ denim,
        funding_agency == "CDC" ~ denim_light,
        str_detect(funding_agency, ", ") ~ moody_blue,
        TRUE ~ NA_character_
      ),
      label_color = case_when(
        funding_agency == "USAID" ~ grey10k,
        TRUE ~ grey90k
      )) %>%
    filter(!is.na(funding_agency))


# VIZ ----

  # COVERAGE ----

  push_cntries %>%
    walk(function(.cntry) {

      print(.cntry)

      map_cov <- generate_cov(.data = spdf_cov,
                              terr = terr,
                              spdf = spdf_pepfar,
                              cntry = .cntry)

      si_save(
        filename = file.path(
          dir_graphics,
          paste0(meta$curr_pd, " - ",
                 str_to_upper(.cntry),
                 " - AGENCY COVERAGE - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map_cov,
        width = 10,
        height = 5,
        dpi = 320,
        scale = 1.2)

      return(map_cov)
    })

  # ART SAT VIZ ----

  push_cntries %>%
    #nth(1) %>%
    walk(function(.cntry) {

      spdf_art <- spdf_tx %>%
        filter(regionorcountry_name == .cntry,
               str_detect(orgunit_name, "_Mil", negate = T),
               period == meta$curr_pd)

      print(glue("{.cntry}: {nrow(spdf_art)}"))

      # Stop here for no Data
      if (nrow(spdf_art) == 0) return(NULL)

      map_art_sat <- generate_map(.data = spdf_art,
                                  terr = terr,
                                  spdf = spdf_pepfar,
                                  cntry = .cntry)

      print(map_art_sat)

      # DOT Change

      dots_art_change <- generate_dumbbell(.data = df_tx_art_trend, cntry = .cntry)

      print(dots_art_change)

      # # Map + bar plot
      art_plot <- (map_art_sat + dots_art_change)

      print(art_plot)

      si_save(
        filename = file.path(
          dir_graphics,
          paste0(meta$curr_pd, " - ",
                 str_to_upper(.cntry),
                 " - USAID",
                 " - ART Saturation Map - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = map_art_sat,
        width = 10,
        height = 5,
        dpi = 320,
        scale = 1.2)

      si_save(
        filename = file.path(
          dir_graphics,
          paste0(meta$curr_pd, " - ",
                 str_to_upper(.cntry),
                 " - USAID",
                 " - ART Saturation Plot - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = dots_art_change,
        width = 10,
        height = 5,
        dpi = 320,
        scale = 1.2)

      si_save(
        filename = file.path(
          dir_graphics,
          paste0(meta$curr_pd, " - ",
                 str_to_upper(.cntry),
                 " - USAID",
                 " - ART Saturation Map and Plot - ",
                 format(Sys.Date(), "%Y%m%d"),
                 ".png")),
        plot = art_plot,
        width = 10,
        height = 5,
        dpi = 320,
        scale = 1.2)
    })



