# AUTHOR:   K. Srikanth | USAID
# PURPOSE:
# REF ID:   5c8c81a8
# LICENSE:  MIT
# DATE:     2023-10-16
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  library(glamr)
  library(tidyverse)
  library(glitr)
  library(gophr)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)
  library(googlesheets4)
  library(gt)
  library(gtExtras)


# GLOBAL VARIABLES --------------------------------------------------------

  # SI specific paths/functions
    load_secrets()

  filepath <- si_path() %>%
    return_latest("^MER_.*_PSNU_IM_FY21.*_S.*.zip$")

  # Grab metadata
    get_metadata(filepath)

  ref_id <- "5c8c81a8"

# FUNCTIONS ---------------------------------------------------------------
  # Custom theme for tables
  gt_theme_phc <- function (gt_object, ...)
  {
    stopifnot(`'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?` = "gt_tbl" %in%
                class(gt_object))

    gt_object %>%
      tab_options(
        heading.align = "left",
        #column_labels.border.top.style = "none",
        table.border.top.style = "none",
        #column_labels.border.bottom.style = "none",
        column_labels.border.bottom.width = 1,
        column_labels.border.bottom.color = "#334422",
        #table_body.border.top.style = "none",
        table_body.border.bottom.color = "white",
        #heading.border.bottom.style = "none",
        data_row.padding = px(7),
        column_labels.font.size = px(12), ...
      ) %>%
      tab_style(
        style = cell_text(
          color = "darkgrey",
          font = google_font("Source Sans Pro"), transform = "uppercase"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      tab_style(
        style = cell_text(
          color = grey60k,
          weight = "600",
          font = google_font("Source Sans Pro"), transform = "uppercase"
        ),
        locations = cells_column_spanners(spanners = everything())
      ) %>%
      tab_style(style = cell_text(
        font = google_font("Libre Franklin"),
        weight = 800
      ),
      locations = cells_title(groups = "title")) %>%
      tab_style(style = cell_text(
        font = google_font("Source Sans Pro"),
        weight = 400
      ),
      locations = cells_body()
      # ) %>%
      # tab_style(style = cell_text(
      #   font = google_font("Source Sans Pro"),
      #   weight = 400
      # ),
      # locations = cells_summary()
      )
  }


  # Shrink size of rows in GT tables
  shrink_rows <- function(gt_obj){
    gt_obj %>%
      tab_options(
        data_row.padding = px(1),
        row_group.padding = px(2),
        heading.padding = px(1)
      )
  }


# IMPORT ------------------------------------------------------------------

df_msd <- read_psd(filepath)

# MUNGE -------------------------------------------------------------------

#NET NEW without  CS and known issues resolved
df_nn <- df_msd %>%
  resolve_knownissues() %>%
    filter(fiscal_year %in% c(2022, 2023),
           funding_agency == "USAID",
           indicator == "TX_NET_NEW",
           snuprioritization != "5 - Centrally Supported",
           standardizeddisaggregate == "Total Numerator")  %>%
    group_by(fiscal_year,funding_agency, indicator, psnu) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd() %>%
    mutate(abs_val = abs(value)) %>%
    mutate(fy = str_sub(period, end = 4),
         qtr = str_sub(period, start= -2)) %>%
    group_by(psnu, fy) %>%
  mutate(cumulative = cumsum(value))

#grab tx_curr target for net_new target calc
tx_curr_targ <- df_msd %>%
  #resolve_knownissues() %>%
  filter(
    fiscal_year %in% c(2022, 2023),
         funding_agency == "USAID",
         indicator == "TX_CURR",
         snuprioritization != "5 - Centrally Supported",
         standardizeddisaggregate == "Total Numerator")  %>%
  # count(snuprioritization)
  group_by(fiscal_year,funding_agency, indicator, psnu) %>%
  summarise(across(starts_with("target"), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(fy = ifelse(fiscal_year == 2022, "FY21", "FY22"))

#create net new target

#([TX_CURR (Target)] - [TX_CURR (prev Q4)])/4
#FY23 Target - FY22Q4 TX_CURR
#FY22 Target - FY21Q4 TX_CURR
nn_target <- df_msd %>%
  filter(
    funding_agency == "USAID",
    indicator == "TX_CURR",
    snuprioritization != "5 - Centrally Supported",
    standardizeddisaggregate == "Total Numerator")  %>%
  group_by(fiscal_year,funding_agency, indicator, psnu) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd(qtrs_keep_cumulative = FALSE) %>%
  filter(str_detect(period, "Q4")) %>%
  mutate(fy = str_sub(period, end = 4),
         qtr = str_sub(period, start= -2)) %>%
  left_join(tx_curr_targ %>% select(-c(indicator)), by = c("funding_agency", "psnu", "fy")) %>%
  rename(prev_q4_result = value) %>%
  mutate(tx_net_new_target = targets - prev_q4_result) %>%
  select(psnu, fiscal_year, tx_net_new_target) %>%
  mutate(fy = ifelse(fiscal_year == 2022, "FY22", "FY23"))


#made dfs for each period (probably a better way to do this but this is fine for now lol)
fy22q4 <- df_nn %>%
  left_join(nn_target, by = c("psnu", "fy")) %>%
  ungroup() %>%
 # filter(period %ni% c("FY22Q1", "FY22Q2", "FY22Q3")) %>%
  select(period, psnu, cumulative, tx_net_new_target) %>%
  mutate(fy22q4_targ_achv = cumulative/tx_net_new_target) %>%
  filter(period == "FY22Q4") %>%
  rename(fy22q4 = cumulative,
         fy22q4_targ = tx_net_new_target)

fy23q1 <- df_nn %>%
  left_join(nn_target, by = c("psnu", "fy")) %>%
  ungroup() %>%
  select(period, psnu, cumulative, tx_net_new_target) %>%
  mutate(fy23q1_targ_achv = cumulative/tx_net_new_target) %>%
  filter(period == "FY23Q1") %>%
  rename(fy23q1 = cumulative,
         fy23q1_targ = tx_net_new_target)

fy23q2 <- df_nn %>%
  left_join(nn_target, by = c("psnu", "fy")) %>%
  ungroup() %>%
  select(period, psnu, cumulative, tx_net_new_target) %>%
  mutate(fy23q2_targ_achv = cumulative/tx_net_new_target) %>%
  filter(period == "FY23Q2") %>%
  rename(fy23q2 = cumulative,
         fy23q2_targ = tx_net_new_target)

fy23q3 <- df_nn %>%
  left_join(nn_target, by = c("psnu", "fy")) %>%
  ungroup() %>%
  select(period, psnu, cumulative, tx_net_new_target) %>%
  mutate(fy23q3_targ_achv = cumulative/tx_net_new_target) %>%
  filter(period == "FY23Q3") %>%
  rename(fy23q3 = cumulative,
         fy23q3_targ = tx_net_new_target)

#bind all rows and make a table
fy22q4 %>%
  left_join(fy23q1 %>% select(-period), by = "psnu") %>%
  left_join(fy23q2 %>% select(-period), by = "psnu") %>%
  left_join(fy23q3 %>% select(-period), by = "psnu") %>%
  select(-c(period, ends_with("_targ"))) %>%
  arrange(desc(fy23q3)) %>%
  gt() %>%
  fmt_percent(columns = c(3,5,7,9),
              decimals = 0) %>%
  fmt_number(columns = c(2,4,6,8),
             decimals = 0) %>%
  tab_spanner(
    label = "FY22Q4",
    columns = c(2:3)) %>%
  tab_spanner(
    label = "FY23Q1",
    columns = c(4:5)) %>%
  tab_spanner(
    label = "FY23Q2",
    columns = c(6:7)) %>%
  tab_spanner(
    label = "FY23Q3",
    columns = c(8:9)) %>%
  tab_row_group(
    label = "Less than 15% NET_NEW Goal in FY23Q3",
    rows = psnu %ni% c("ec Alfred Nzo District Municipality",
                       "gp Sedibeng District Municipality",
                       "ec Buffalo City Metropolitan Municipality",
                       "mp Nkangala District Municipality")
  ) %>%
  tab_row_group(
    label = "Greater than 15% NET_NEW Goal in FY23Q3",
    rows = psnu %in% c("ec Alfred Nzo District Municipality",
                       "gp Sedibeng District Municipality",
                       "ec Buffalo City Metropolitan Municipality",
                       "mp Nkangala District Municipality")
  ) %>%
  cols_label(fy22q4 = "Cumulative NET_NEW",
             fy22q4_targ_achv = "Goal Achv",
             fy23q1 = "Cumulative NET_NEW",
             fy23q1_targ_achv = "Goal Achv",
             fy23q2 = "Cumulative NET_NEW",
             fy23q2_targ_achv = "Goal Achv",
             fy23q3 = "Cumulative NET_NEW",
             fy23q3_targ_achv = "Goal Achv",
             psnu = "USAID District") %>%
  #gt_highlight_rows(rows = c(1:4), font_weight = "normal", fill = "#8AB75E") %>%
#  gt_highlight_rows(rows = c(1:4), font_weight = "normal", fill = golden_sand) %>%
  gt_theme_phc() %>%
  shrink_rows() %>%
  tab_style(
    style = list(
      cell_text(weight = 600)
    ),
    locations = cells_body(
      columns = c(0)
    )
  ) %>%
  tab_options(
    source_notes.font.size = px(10),
    data_row.padding = px(1),
    row_group.font.weight = "bold",
    column_labels.font.size = px(15)) %>%
  gt_color_rows(columns = c(4,6,8), na.color = "white",
                palette = c("#f7f7f7", scooter_med)) %>%
  tab_header(
    title = glue("Cumulative NET_NEW by USAID District" %>% toupper())
  ) %>%
  tab_source_note(
    source_note = gt::md(glue("{metadata$caption}"))) %>%
  gtsave_extra(filename = glue("Images/SA_NET_NEW_psnu.png"))


