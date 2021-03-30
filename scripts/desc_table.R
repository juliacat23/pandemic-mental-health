source("Data_Clean.R")

#descriptive statistics
social.daily.summary <- describe(social_daily) 
print(social.daily.summary)

social.daily.summary <- as.data.frame(social.daily.summary)
social.daily.summary <- rowid_to_column(social.daily.summary)
social.daily.summary[1,1] <- "Total SM"
social.daily.summary[2,1] <- "Twitter"
social.daily.summary[3,1] <- "Instagram"
social.daily.summary[4,1] <- "Snapchat"
social.daily.summary[5,1] <- "Facebook"
social.daily.summary[6,1] <- "Linkedin"
social.daily.summary[7,1] <- "Tiktok"
social.daily.summary[8,1] <- "Anxiety"
social.daily.summary[9,1] <- "Depression"
social.daily.summary[10,1] <- "Focus"
social.daily.summary[11,1] <- "Lonliness"
social.daily.summary[12,1] <- "Daily Score"
table_1 <- social.daily.summary %>% 
  filter(rowid != c("Total SM", "Twitter", "Instagram", "Snapchat", "Facebook", "Linkedin", "Tiktok")) %>% 
  gt() %>% 
  cols_hide(
    columns = vars(vars, n, median, trimmed, mad, skew, kurtosis)) %>% 
  fmt_number(
    columns = vars("mean", "sd", "se"), decimals = 1) %>% 
  fmt_number(
    columns = vars("min", "max", "range"), decimals = 1) %>% 
  cols_label(
    mean = "Mean",
    sd = "S.D.",
    min = "Min",
    max = "Max",
    range = "Range", 
    se = "S.E.") %>% 
  tab_header(
    title = "Table 1",
    subtitle = md("Descriptive statistics of scores of high frequency mood assessment to capture daily fluctuations in mood. Daily scores were defined as the sum of the four subscores: anxiety, depression, focus, and lonliness.")) %>% 
  opt_align_table_header(align = c("left")) %>% 
  opt_table_font(font = c("Times New Roman")) %>% 
  tab_options(
    table.border.top.color = "white",
    heading.title.font.size = px(16),
    column_labels.border.top.width = 3,
    column_labels.border.top.color = "black",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = 3,
    table.border.bottom.color = "white",
    table.width = pct(100),
    table.background.color = "white") %>% 
  cols_align(align="center") %>% 
  cols_align(align="center") %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = px(1)),
      cell_text(
        align="center"),
      cell_fill(color = "white", alpha = NULL)),
    locations = cells_body(
      columns = everything(),
      rows = everything())) %>% 
  tab_footnote(
    footnote = "S.D. = Standard Deviation",
    locations = cells_column_labels(
      columns = vars(sd))) %>% 
  tab_footnote(
    footnote = "S.E. = Standard Error",
    locations = cells_column_labels(
      columns = vars(se))) %>% 
  gtsave("table_1.png")

