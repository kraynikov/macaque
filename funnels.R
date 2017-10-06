#THE FIRST, BELOVED ONE
funnel  <- function(y)  {
  
  #ready, steady, medvedy
  y_funnel <- 
    y %>%
    group_by(teh_period) %>%
    summarize(logins = sum(logins),
              redirects_u = sum(redirects_u),
              attempts_u = sum(attempts_u),
              charged_u = sum(charged_u),
              rev = sum(AMOUNT_USD)) %>%
    
    #levels
    mutate(red2logins = redirects_u/logins, attempts2logins = attempts_u/logins, charged2logins = charged_u/logins, rev2logins = rev/logins) %>%
      
    #diffs
    mutate(rv2l_diff = (rev2logins/lag(rev2logins, n = 1L, order_by = teh_period) - 1),
           c2l_diff = (charged2logins/lag(charged2logins, n = 1L, order_by = teh_period) - 1),
           a2l_diff = (attempts2logins/lag(attempts2logins, n = 1L, order_by = teh_period) - 1),
           r2l_diff = (red2logins/lag(red2logins, n = 1L, order_by = teh_period) - 1),
           #rates
           ac = rev/charged_u,
           ar = charged_u/attempts_u,
           ap = attempts_u/redirects_u) %>%
    
    #fixes
    mutate(c2l_fixed = (charged2logins + lag(charged2logins, n = 1L, order_by = teh_period))/2,
           a2l_fixed = (attempts2logins + lag(attempts2logins, n = 1L, order_by = teh_period))/2,
           r2l_fixed = (red2logins + lag(red2logins, n = 1L, order_by = teh_period))/2,
           
           ac_fixed = (ac + lag(ac, n = 1L, order_by = teh_period))/2,
           ar_fixed = (ar+lag(ar, n = 1L, order_by = teh_period))/2,
           ap_fixed = (ap + lag(ap, n = 1L, order_by = teh_period))/2) %>%
    
    #absolute differences in metrics
    mutate(c2l_effect_abs = charged2logins * ac_fixed - lag(charged2logins, n = 1L, order_by = teh_period) * ac_fixed,
           ac_effect_abs = ac * c2l_fixed - lag(ac, n = 1L, order_by = teh_period) * c2l_fixed,
           
           a2l_effect_abs = attempts2logins * ar_fixed - lag(attempts2logins, n = 1L, order_by = teh_period) * ar_fixed,
           ar_effect_abs = ar * a2l_fixed - lag(ar, n = 1L, order_by = teh_period) * a2l_fixed,
           
           r2l_effect_abs = red2logins * ap_fixed - lag(red2logins, n = 1L, order_by = teh_period) * ap_fixed,
           ap_effect_abs = ap * r2l_fixed - lag(ap, n = 1L, order_by = teh_period) * r2l_fixed) %>%
    
    #%change decomposition
    mutate(c2l_effect = c2l_effect_abs/lag(rev2logins, n = 1L, order_by = teh_period),
           ac_effect = ac_effect_abs/lag(rev2logins, n = 1L, order_by = teh_period),
           
           a2l_effect = a2l_effect_abs/lag(charged2logins, n = 1L, order_by = teh_period),
           ar_effect = ar_effect_abs/lag(charged2logins, n = 1L, order_by = teh_period),
           
           r2l_effect = r2l_effect_abs/lag(attempts2logins, n = 1L, order_by = teh_period),
           ap_effect = ap_effect_abs/lag(attempts2logins, n = 1L, order_by = teh_period)) %>%  
    
    as.data.frame()
  return (y_funnel)
}


#VIA NAME
funnel_via <- function(x) {
  temp_tt <- x %>%
    group_by(teh_period) %>%
    summarize(logins = sum(logins),
                redirects_u = sum(redirects_u),
                attempts_u = sum(attempts_u),
                charged_u = sum(charged_u),
                rev = sum(AMOUNT_USD)) %>%
    as.data.frame()
  
  #BY VIA
  temp_via <- x %>%
      group_by(teh_period, via_name) %>%
      summarize(logins = sum(logins),
                redirects_u = sum(redirects_u),
                attempts_u = sum(attempts_u),
                charged_u = sum(charged_u),
                rev = sum(AMOUNT_USD)) %>%
      as.data.frame()
  
  #top N
  tv <- temp_via %>% 
    filter(teh_period == "m_week_1") %>%
    top_n(10, redirects_u) %>%
    select(via_name)
  
  temp_via_10 <- filter(temp_via, via_name %in% tv[[1]])
  temp_via_other <- temp_via %>%
    filter(! via_name %in% tv[[1]]) %>%
    group_by(teh_period) %>%
    summarize(redirects_u = sum(redirects_u), attempts_u = sum(attempts_u), charged_u = sum(charged_u), rev = sum(rev), logins = sum(logins)) %>%
    mutate(via_name = "other") %>%
    as.data.frame
  
  temp_top  <- rbind(temp_via_10, temp_via_other)
  
  #with totals
  temp_top1 <- left_join(x = temp_top, y = temp_tt, by = c("teh_period" = "teh_period"))
  temp_top1 <- temp_top1 %>%
    select(teh_period, via_name, redirects_u = redirects_u.x, redirects_t = redirects_u.y, rev = rev.x, rev_t = rev.y, logins_t = logins.y) %>%
    arrange(via_name)
  
  #KPIs
  temp_top2 <- temp_top1 %>%
    mutate(rev2log = rev/logins_t, redSh = redirects_u/redirects_t, revSh = rev/rev_t) %>%
    mutate(redShChange = (redSh - lag(redSh, n = 1L, order_by = via_name)), revShChange = (revSh - lag(revSh, n = 1L, order_by = via_name)),
      rev2logCh = (rev2log / lag(rev2log, n = 1L, order_by = via_name) - 1),
      POIrev2log = ((rev2log-lag(rev2log, n = 1L, order_by = via_name))/(lag(rev_t, n = 1L, order_by = via_name)/lag(logins_t, n = 1L, order_by = via_name)))) %>%
    
    filter(teh_period == "m_week_1") %>%
    select(via_name, redirects_u, rev, rev2log, redSh, redShChange, revSh, revShChange, rev2logCh, POIrev2log)
  return(temp_top2)
  }

#PRODUCT
funnel_prod <- function(x) {
  temp_tt <- x %>%
    group_by(teh_period) %>%
    summarize(logins = sum(logins),
                redirects_u = sum(redirects_u),
                attempts_u = sum(attempts_u),
                charged_u = sum(charged_u),
                rev = sum(AMOUNT_USD)) %>%
    as.data.frame()
  
  #BY PROD
  temp_prod <- x %>%
      group_by(teh_period, product) %>%
      summarize(logins = sum(logins),
                redirects_u = sum(redirects_u),
                attempts_u = sum(attempts_u),
                charged_u = sum(charged_u),
                rev = sum(AMOUNT_USD)) %>%
      as.data.frame()
  
  #top N
  tp <- temp_prod %>% 
    filter(teh_period == "m_week_1") %>%
    top_n(5, attempts_u) %>%
    select(product)
  
  temp_prod_5 <- filter(temp_prod, product %in% tp[[1]])
  temp_prod_other <- temp_prod %>%
    filter(! product %in% tp[[1]]) %>%
    group_by(teh_period) %>%
    summarize(redirects_u = sum(redirects_u), attempts_u = sum(attempts_u), charged_u = sum(charged_u), rev = sum(rev), logins = sum(logins)) %>%
    mutate(product = "other") %>%
    as.data.frame
  
  temp_top  <- rbind(temp_prod_5, temp_prod_other)
  
  #with totals
  temp_top1 <- left_join(x = temp_top, y = temp_tt, by = c("teh_period" = "teh_period"))
  temp_top1 <- temp_top1 %>%
    select(teh_period, product,  charged_t = charged_u.y, charged_u = charged_u.x, rev = rev.x, rev_t = rev.y, logins_t = logins.y) %>%
    arrange(product)
  
  #KPIs # # # # #  # AZAZA TUT NIHUIA NE SDELAL
  temp_top2 <- temp_top1 %>%
    mutate(rev2log = rev/logins_t, log2paid = charged_u/logins_t, revSh = rev/rev_t, chargedSh = charged_u/charged_t) %>%
  
    mutate(revShChange = (revSh - lag(revSh, n = 1L, order_by = product)),
      paidShChange = (chargedSh - lag(chargedSh, n = 1L, order_by = product)),
      rev2logCh = (rev2log / lag(rev2log, n = 1L, order_by = product) - 1),
      log2paidCh = (log2paid / lag(log2paid, n = 1L, order_by = product) - 1),
      POIlog2paid = ((log2paid-lag(log2paid, n = 1L, order_by = product))/(lag(charged_t, n = 1L, order_by = product)/lag(logins_t, n = 1L, order_by = product))),
      POIrev2log = ((rev2log-lag(rev2log, n = 1L, order_by = product))/(lag(rev_t, n = 1L, order_by = product)/lag(logins_t, n = 1L, order_by = product)))) %>%
    
    filter(teh_period == "m_week_1") %>%
    select(product, rev, rev2log, charged_u, log2paid, revSh, chargedSh, revShChange, paidShChange, rev2logCh, log2paidCh, POIrev2log, POIlog2paid)
  return(temp_top2)
}



