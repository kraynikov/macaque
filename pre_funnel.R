td_mat_old  <- function(y) {
  y %>%
    arrange(desc(trans_date)) %>%
    mutate(teh_row = row_number()) %>%
    
    mutate(teh_period = case_when(teh_row < 8 ~ "m_week_1",
                                  teh_row >= 8 ~ "m_week_0" )) 
  
}

td_mat  <- function(y) {
  y <- arrange(y, desc(trans_date)) 
  y <- y[3:16, ]
  y <- y %>%
    mutate(teh_row = row_number()) %>%
    mutate(teh_period = case_when(teh_row < 8 ~ "m_week_1",
                                  teh_row >= 8 ~ "m_week_0" ))
  return(y)
}

#last 3 days vs 3 previous days 
#last 3 days vs 3 same weedays previous week

#for analysis with given split date
td_ref_date <- function(y) {
  y <- arrange(y, desc(trans_date))
  y$lt <- y$trans_date - split_date_pre
  y <- filter(y, abs(lt) <= qWindow, lt != qWindow)
  y <- mutate(y, teh_period = case_when(lt >= 0 ~ "m_week_1", lt < 0 ~ "m_week_0"))
  return(y)
  }

#for given range?
td_mat_r  <- function(y) {
  y <- arrange(y, desc(trans_date)) 
  y <- y %>%
    mutate(teh_row = row_number()) %>%
    mutate(teh_period = case_when(teh_row < 8 ~ "m_week_1",
                                  teh_row >= 8 ~ "m_week_0" ))
  return(y)
}

#for via_name added segmentation, range
pre_via <- function(x) {
  temp_d <- x %>% 
    filter(between(trans_date, rdate0, rdate1)) %>%
    mutate(teh_period = case_when(trans_date < rdate0 + 7 ~ "m_week_0",
                                  trans_date >= rdate0 + 7 ~ "m_week_1" ))
  return(temp_d)
  }

pre_prod <- function(x) {
  temp_d <- x %>% 
    filter(between(trans_date, rdate0, rdate1)) %>%
    mutate(teh_period = case_when(trans_date < rdate0 + 7 ~ "m_week_0",
                                  trans_date >= rdate0 + 7 ~ "m_week_1" ), 
            package = case_when(grepl("3 Days", product) ~ "3d_trial", 
                          (grepl("1 Week", product) | grepl("day.7", product)) ~ "1_week", 
                          (grepl("1 Month", product) | grepl("day.30", product) | grepl("month.1", product) | grepl("day.31", product)) ~ "1_month",
                          (grepl("3 Months", product) | grepl("day.90", product) | grepl("month.3", product)) ~ "3_month", 
                          (grepl("6 Month", product) | grepl("day.180", product) | grepl("month.6", product)) ~ "6_ month",
                          TRUE ~ "other" )
                  )
  return(temp_d)
  }
