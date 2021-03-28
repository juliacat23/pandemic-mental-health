source("Date_Clean.R")
library(tidyverse)
library(gt)
library(summarytools)
library(webshot)

#descriptive statistics
social.daily.summary <- as.data.frame(apply(social_daily, 2, summary))
social.daily.summary <- t(social.daily.summary)
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
social.daily.summary %>% 
  filter(rowid != c("Total SM", "Twitter", "Instagram", "Snapchat", "Facebook", "Linkedin", "Tiktok")) %>% 
  gt() %>% 
  fmt_number(
    columns = vars("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")) %>% 
  tab_header(
    title = "Table 1",
    subtitle = md("*Descriptive Statistics of High Frequence Mood Assessment*")) %>% 
  cols_label(
    rowid = " ",
    Min. = "Min",
    Max. = "Max") %>% 
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
  tab_source_note(
    source_note = ("Note: The overall daily mood score is the sum of the four subscores: anxiety, depression, focus, and lonliness.")) 



## correlation 
cor.screen <- corstars(social_daily.screen, result="none")
cor.screen <- rowid_to_column(cor.screen)
cor.screen[1, 1] <- "Total SM Use"
cor.screen[2, 1] <- "Twitter"
cor.screen[3,1]  <- "Instagram" 
cor.screen[4,1]  <- "Snapchat"
cor.screen[5,1]  <- "Facebook" 
cor.screen[6,1] <- "Linkedin"
cor.screen[7,1] <- "Tiktok"
cor.screen[8,1] <- "Daily Score"
cor.screen <- cor.screen[-1,]

cor.screen %>% 
  gt() %>% 
  tab_header(
    title = "Table B2",
    subtitle = md("*Correlations of Social Media Usage and Overall Daily Mood Score*")) %>% 
  cols_label(
    rowid = " ",
    social_overall = "Total SM",
    twitter = "Twitter",
    instagram = "Instagram",
    snapchat = "Snapchat",
    facebook = "Facebook", 
    linkedin = "Linkein",
    tiktok = "Tiktok") %>% 
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
  tab_source_note(
    source_note = ("* p < .05, ** p < .01, *** p < .001, **** p < .0001")) %>% 
  tab_footnote(
    footnote = "Social Media is abrebriated as 'SM'",
    locations = cells_column_labels(
      columns = vars(social_overall))) 

## cor.symptoms
cor.symptoms <- corstars(social_daily.symptoms, result="none")
cor.symptoms <- rowid_to_column(cor.symptoms)
cor.symptoms[1, 1] <- "Total SM"
cor.symptoms[2, 1] <- "Anxiety"
cor.symptoms[3,1]  <- "Depression" 
cor.symptoms[4,1]  <- "Focus"
cor.symptoms[5,1]  <- "Lonliness" 
cor.symptoms[6,1] <- "Daily Score"
cor.symptoms <- cor.symptoms[-1,]


cor.symptoms %>% 
  gt() %>% 
  tab_header(
    title = "Table 3",
    subtitle = md("*Correlations of Composite Daily Mood Assessment and Social Media Use*")) %>% 
  cols_label(
    rowid = " ",
    social_overall = "Total SM",
    daily.anxiety = "Anxiety",
    daily.depression = "Depression",
    daily.focus = "Focus",
    daily.lonley = "Lonliness") %>% 
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
  tab_source_note(
    source_note = ("* p < .05, ** p < .01, *** p < .001, **** p < .0001")) %>% 
  tab_footnote(
    footnote = "Social Media is abrebriated as 'SM'",
    locations = cells_column_labels(
      columns = vars(social_overall))) %>% 
  gtsave("cor_symptoms.png")

wilcox.test(total_score ~ date, data = composite, paired = TRUE, alternative = "greater")

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

