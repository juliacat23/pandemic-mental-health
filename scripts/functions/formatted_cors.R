library(Hmisc)
#adapted from https://www.khstats.com/blog/corr-plots/corr-plots/

cors <- function(df) {
  M <- Hmisc::rcorr(as.matrix(df))
  Mdf <- map(M, ~data.frame(.x))
  return(Mdf)
}

formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>%
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    rename(p = P) %>%
    mutate(sig_p = ifelse(p < .05, T, F),
           p_if_sig = ifelse(sig_p, p, NA),
           r_if_sig = ifelse(sig_p, r, NA)) 
}