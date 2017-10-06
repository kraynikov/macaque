SN <- sqlQuery(r, "select SITE_ID, SITE_NAME from DM_OWNER.d_site", stringsAsFactors = FALSE)
RP <- sqlQuery(r, "select PLATFORM_ID, PLATFORM_NAME from DM_OWNER.d_platform", stringsAsFactors = FALSE)
RP <- RP %>%
  mutate(PLATFORM_ID = as.character(PLATFORM_ID)) %>%
  add_row(PLATFORM_ID = '6, 7', PLATFORM_NAME = 'mob') %>%
  add_row(PLATFORM_ID = '4, 5', PLATFORM_NAME = 'widget')
CG <- sqlQuery(r, "select country_id, country_iso2_source_key from DM_OWNER.d_country", stringsAsFactors = FALSE)

TS  <- sqlQuery(r, "select TRAFFIC_SOURCE_ID, TRAFFIC_SOURCE_NAME from DM_OWNER.d_traffic_source", stringsAsFactors = FALSE)

GN <- data.frame(gn_group = c('0. male', '1. female', '3. all'), zquery_gn = c('and GENDER_ID = 1', 'and GENDER_ID = 2', ' '), stringsAsFactors = FALSE)
AG <- data.frame(ag_group = c('0. 18-24', '1. 25-34', '2. 35-44', '3. 45 and older', '4. all'), 
                  zquery_ag = c('and AGE_ID between 18 and 24', 'and AGE_ID between 25 and 34', 'and AGE_ID between 35 and 44', 'and AGE_ID  > 45', ' '), stringsAsFactors = FALSE)

# LIFETIME
# LT <- data.frame(
#   lt_group = c('0. entry', '1. 1 to 7', '2. 8 to 30', '3. 30 and older', '4. all'),
#   zquery_lt = c('and date(TRANSACTION_DATE) - date(REG_DATE) = 0', 'and date(TRANSACTION_DATE) - date(REG_DATE) between 1 and 7',
#                 'and date(TRANSACTION_DATE) - date(REG_DATE) between 8 and 30', 
#                 'and date(TRANSACTION_DATE) - date(REG_DATE) > 30', ' '),
#   stringsAsFactors = FALSE)

LT <- data.frame(
  lt_group = c('0. entry', '1. 1 to 7', '2. 8 to 30', '3. 30 and older', '4. all'),
  zquery_lt = c('and (case when auth_order_source_key = - 99 then (date(TRANSACTION_DATE)) else (date(TRANSACTION_DATE) - 2) end) - date(REG_DATE) = 0',
                'and (case when auth_order_source_key = - 99 then (date(TRANSACTION_DATE)) else (date(TRANSACTION_DATE) - 2) end) - date(REG_DATE) between 1 and 7',
                'and (case when auth_order_source_key = - 99 then (date(TRANSACTION_DATE)) else (date(TRANSACTION_DATE) - 2) end) - date(REG_DATE) between 8 and 30', 
                'and (case when auth_order_source_key = - 99 then (date(TRANSACTION_DATE)) else (date(TRANSACTION_DATE) - 2) end) - date(REG_DATE) > 30', ' '),
  stringsAsFactors = FALSE)

#exogenous
input_segments <- read.table(file = "fuck.csv", sep = ';', header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
input_segments_all <- read.table(file = "wednesday_all.txt", sep = ';', header = TRUE, fill = TRUE, stringsAsFactors = FALSE) 

#