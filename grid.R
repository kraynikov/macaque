#write queries for given segmentation
bf  <- function(x) {
  x_temp  <-  x
  x_temp  <-  x_temp %>%  mutate(query = " ") 
  colnames(x_temp)[3] <- paste("zquery", colnames(x_temp)[1], sep = "_")
  
  #WRITE QUERIES
  for (i in 1:nrow(x_temp)) {
    x_temp[i, 3] = paste("and", colnames(x_temp[1]), "in (", as.character(x_temp[i,1]), ")", sep = " ")
  }
  
  #ADD OTHERS
  x_temp <- rbind(x_temp, NA)  
  x_temp[nrow(x_temp), 2] = 'other'
  
  x_temp[nrow(x_temp), 3]  = paste("and", colnames(x[1]), "not in (", sep = " ")
  for (i in 1:(nrow(x_temp) -1)) {
    if (i != (nrow(x_temp) - 1)) {
      x_temp[nrow(x_temp), 3] = paste(x_temp[nrow(x_temp), 3], as.character(x_temp[i,1]), ",", sep = " ")
    }
    else {x_temp[nrow(x_temp), 3] = paste(x_temp[nrow(x_temp), 3], as.character(x_temp[i,1]), ")", sep = " ")
    }
  }
  
  x_temp <- rbind(x_temp, c(NA, 'all', ' '))
  x_temp <- x_temp[, 2:3]
  return (x_temp)
}

#small motherfucker of a little use
picker <- function(x, y) {
  x <- filter(x, x[,2] %in% y)
  return(x)
  }

#-- THE GRID -------------------------------------------------------------------------------------------------------
#intersects given segments, combining queries for every case
grider <- function(k) {
  the_grid <- k[[1]]
  for (i in 1:(length(k)-1)) {
    the_grid  <- merge(the_grid, k[[i+1]], all = TRUE)
  }
  the_grid <- the_grid[ , order(names(the_grid))]
  pick  <- c(names(the_grid)[(length(names(the_grid))/2+1):(length(names(the_grid)))])
  the_grid <- unite(the_grid, "teh_query", pick, sep = " ", remove = TRUE)
  return(the_grid)
}


#-- LOOPER ---------------------------------------------------------------------------------------------------------
#simple loop, assembling query from SQL operator blocks, doesn't give any shit about auth_oks, simply takes all charges 
#in given transaction_date, the fastest one, the least precise
looper <- function(the_grid, y) {
  fuck <- data.frame()
  for (i in 1:nrow(the_grid)) {
    temp_d  <- sqlQuery(r, paste(c(Select, from, sprintf(where, the_grid[i, ncol(the_grid)]),  groupby), sep = " ", collapse = " "))
    # if (nrow(temp_d) == 0 ) {
    #     temp_d <- rbind(temp_d, 0)
    #    }
    temp_d <- wtl(funnel(y(temp_d)))
    temp_d <- cbind(the_grid[i, 1:(ncol(the_grid) - 1)], temp_d)
    fuck <- rbind(fuck, temp_d)
  }
  return(fuck)
}

#loops with preassembled query, consisting of 4 queries, inner joining by order_source_key, precise but slow as fuck (~20 min)
looper_heavy <- function(the_grid, y) {
  fuck <- data.frame()
  for (i in 1:nrow(the_grid)) {
    temp_d  <- sqlQuery(r, paste(c(sprintf(AUTH_OK, the_grid[i, ncol(the_grid)],  the_grid[i, ncol(the_grid)], the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "))
    print("loaded data for new segment")
    if (nrow(temp_d) == 0 ) {
      temp_d <- rbind(temp_d, 0)
    }
    temp_d <- wtl(funnel(y(temp_d)))
    print("funnel passed")
    temp_d <- cbind(the_grid[i, 1:(ncol(the_grid) - 1)], temp_d)
    fuck <- rbind(fuck, temp_d)
    print(i)
    print("binded new segment KPIs")
    
    
  }
  return(fuck)
}

#loops with preassembled query, that deals with AUTH_OKs via lagging transaction date for charges w/ non-null auth_order_key 
#almost as fast as simple looper, but less precise than heavy_looper (estimanted 0.45% error)
looper_lazy <- function(the_grid, y) {
  fuck <- data.frame()
  temp_d <- data.frame()
  for (i in 1:nrow(the_grid)) {
    print(i)
    temp_data  <- sqlQuery(r, paste(c(sprintf(auth_ok, the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "))
    print("loaded data for new segment")
    if (nrow(temp_data) == 0 ) {
      fuck  <-  rbind(fuck, cbind(the_grid[i, 1:(ncol(the_grid) - 1)], as.data.frame(lapply(temp_d, function(x){replace(x, 1,0)})))) 
    } else {
    temp_d <- wtl(funnel(y(temp_data)))
    
    temp_bind <- cbind(the_grid[i, 1:(ncol(the_grid) - 1)], temp_d)
    fuck <- rbind(fuck, temp_bind)
    print("binded new segment KPIs")
    }
  }
  return(fuck)
}

looper_data <- function(the_grid) {
  fuck <- list()
  temp_d <- data.frame()
  for (i in 1:nrow(the_grid)) {
    print(i)
    temp_data  <- sqlQuery(r, paste(c(sprintf(auth_ok_w, the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "))
    print("loaded data for new segment")
    temp_d <- temp_data %>% mutate(
      SN = the_grid[i, 1], RP = the_grid[i, 2], CG = the_grid[i, 3])
    if (ncol(the_grid) == 4) {
      temp_d <- temp_d %>% mutate(SG = the_grid[i,4])
    }
    fuck[[i]] <- temp_d
    print("binded new dataframe")
    
  }
  return(fuck)
}

looper_via <- function(the_grid) {
  fuck <- list()
  temp_d <- data.frame()
  for (i in 1:nrow(the_grid)) {
    print(i)
    #temp_data  <- sqlQuery(r, paste(c(sprintf(auth_ok_w, the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "))
    temp_data  <- sqlQuery(r, paste(c(sprintf(range_dates, rdate0-2, rdate1+2, the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "), stringsAsFactors = FALSE)
    print("loaded data for new segment")
    temp_d <- temp_data %>% mutate(
      SN = the_grid[i, 1], RP = the_grid[i, 2], CG = the_grid[i, 3])
    if (ncol(the_grid) == 4) {
      temp_d <- temp_d %>% mutate(SG = the_grid[i,4])
    }
    fuck[[i]] <- temp_d
    print("binded new dataframe")
    
  }
  return(fuck)
}

looper_prod <- function(the_grid) {
  fuck <- list()
  temp_d <- data.frame()
  for (i in 1:nrow(the_grid)) {
    print(i)
    
    temp_data  <- sqlQuery(r, paste(c(sprintf(prange_dates, rdate0-2, rdate1+2, the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "), stringsAsFactors = FALSE)
    print("loaded data for new segment")
    temp_d <- temp_data %>% mutate(
      SN = the_grid[i, 1], RP = the_grid[i, 2], CG = the_grid[i, 3])
    if (ncol(the_grid) == 4) {
      temp_d <- temp_d %>% mutate(SG = the_grid[i,4])
    }
    fuck[[i]] <- temp_d
    print("binded new dataframe")
    
  }
  return(fuck)
}

looper_split <- function(the_grid, y) {
  fuck <- data.frame()
  temp_d <- data.frame()
  for (i in 1:nrow(the_grid)) {
    print(i)
    temp_data  <- sqlQuery(r, paste(c(sprintf(ref_date,  as.character(split_date), as.character(qWindow+2), the_grid[i, ncol(the_grid)])), sep = " ", collapse = " "))
    print("loaded data for new segment")
    if (nrow(temp_data) == 0 ) {
      fuck  <-  rbind(fuck, cbind(the_grid[i, 1:(ncol(the_grid) - 1)], as.data.frame(lapply(temp_d, function(x){replace(x, 1,0)})))) 
    } else {
    temp_d <- wtl(funnel(y(temp_data)))
    
    temp_bind <- cbind(the_grid[i, 1:(ncol(the_grid) - 1)], temp_d)
    fuck <- rbind(fuck, temp_bind)
    print("binded new segment KPIs")
    }
  }
  return(fuck)
}

#-- DEEPER LIST ----------------------------------------------------------------------------------------------------
#for given grid with some LOD segmentation, e.g. SITE x PLATFORM x COUNTRY he adds intersections with additional LODs 
#provided in the input list. Output is a list of intersections for looping over with deep_loop function.
deeper <- function(x, y) {
  out_list <- list()
  for (i in 1:length(y)) {
    temp_grid <- data.frame()
    temp_grid <- merge(y[[i]], x, all = TRUE)
    temp_grid <- mutate(temp_grid, teh_query1 = paste(temp_grid[[ncol(temp_grid)]], temp_grid[[2]], sep = " "))
    temp_grid <- temp_grid[c(3:(ncol(temp_grid)-2), 1, ncol(temp_grid))]
    #temp_grid <- arrange(temp_grid, temp_grid[,1], temp_grid[,2], temp_grid[,3], temp_grid[,4])
    out_list[[i]] <-  temp_grid
  }
  return(out_list)  
}

#-- DEEP LOOP ------------------------------------------------------------------------------------------------------
#with given list of grids he loops over each, output is a list of KPI dataframes, for each deeper LOD
deep_loop <- function(deep_grid, y, z) {
  
  fucking_list <- list()
  for (j in 1:length(deep_grid)) {
    fucking_list[[j]]  <- z(deep_grid[[j]], y)
    print(j)
  }
  return(fucking_list)
}

deep_data <- function(deep_grid, z) {
  
  fucking_list <- list()
  for (j in 1:length(deep_grid)) {
    fucking_list[[j]]  <- z(deep_grid[[j]])
    print(j)
  }
  return(fucking_list)
}

#-- DIGGER ---------------------------------------------------------------------------------------------------------
#takes deep_loop output and rearranges list elements to have an initial segment across various LODs, not LOD
#across initial segments, as looper and thus deep_loop provide
digger <- function(fuck) {    #fucker is a list
  fuck <- lapply(fuck, function(x) split(x, x[1:2])) #make fucker list of lists
  motherfucker <- list()      #output list
  for (j in 1:length(fuck[[1]])) {
    motherfucker[[j]] <- list() #container for SN
    for (i in 1:length(fuck)) { #inside LT, 4 sreza
      motherfucker[[j]][[i]] <- fuck[[i]][[j]] 
    }
  }
  return(motherfucker)
}

