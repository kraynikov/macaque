#count rows in bloody list
row_counter <- function(digged_fuck) {
  cb_row <- 0
  for (i in 1:length(digged_fuck)) {
    for (j in 1:length(digged_fuck[[i]])) {
      cb_row <- cb_row + nrow(digged_fuck[[i]][[j]])
    }
  }
  return(cb_row)
}

#--- TO EXCEL --------------------------------------------------------------------------------------------
#pizdec, rabotaet cherez pen'-kolodu
writer <- function(digged_fuck) {
  wb <- createWorkbook()
  cs_dd <- CellStyle(wb) + DataFormat("0.00;-0.00")
  blocks <- list()
  blocks[[1]] <- list()
  blocks[[2]] <- list()
  
  for (i in 1:length(digged_fuck)) {
    teh_name <- if (nrow(digged_fuck[[i]][[1]]) != 0) {paste(c(sprintf(unlist(digged_fuck[[i]][[1]][1,1:3]))), collapse = ".")} else {paste("CellBlock", as.character(i), sep = ".")}
    blocks[[1]][[i]] <- createSheet(wb, sheetName=teh_name)
    blocks[[2]][[i]] <- CellBlock(blocks[[1]][[i]], 1, 1, 595, 19)
    
    rs <- 2
    CB.setRowData(blocks[[2]][[i]], colnames(digged_fuck[[i]][[1]]), rowIndex = 1, colOffset=0, showNA=TRUE, rowStyle=NULL)
    
    for (j in 1:length(digged_fuck[[i]])) {
      if (nrow(digged_fuck[[i]][[j]]) != 0) {
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][1:4]), startRow=rs, startColumn=1, showNA=TRUE, cellStyle=NULL)
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][5:19]), startRow=rs, startColumn=5, showNA=TRUE, cellStyle=cs_dd)
        rs <- rs+nrow(digged_fuck[[i]][[j]])+1
      }
    }
    
  }
  deep_name  <- paste("segments_top_kpi_weekly_TD_", as.character(Sys.Date()), ".xlsx", sep = "")
  saveWorkbook(wb, file = deep_name)
}

#--- WITH FORMATTING ----------------------------------------------------------------------------------------
nice_writer <- function(digged_fuck) {
  wb <- createWorkbook()
  
  cs_bucks <- CellStyle(wb) + DataFormat("0$")
  cs_int <- CellStyle(wb) + DataFormat("0;-0;0;@")     #absolutes   
  cs_zbucks <- CellStyle(wb) + DataFormat("0.00$;-0.00$")   #money
  cs_ddperc <- CellStyle(wb) + DataFormat("0.00%;-0.00%;-;@")  # percent double digit
  cs_zperc <- CellStyle(wb) + DataFormat("0%;-0%;-;@")    #percentage
  cs_dd <- CellStyle(wb) + DataFormat("0.00;-0.00")
  
  a <- c(colnames(digged_fuck[[1]][[1]]))
  a <- append(a, " ", after=4)
  a <- append(a, " ", after=7)
  a <- append(a, " ", after=12)
  
  blocks <- list()
  blocks[[1]] <- list()
  blocks[[2]] <- list()
  
  for (i in 1:length(digged_fuck)) {
    teh_name <- if (nrow(digged_fuck[[i]][[1]]) != 0) {paste(c(sprintf(unlist(digged_fuck[[i]][[1]][1,1:3]))), collapse = ".")} else {paste("C", as.character(i), sep = ".")}
    blocks[[1]][[i]] <- createSheet(wb, sheetName=teh_name)
    blocks[[2]][[i]] <- CellBlock(blocks[[1]][[i]], 1, 1, 595, 30)
    
    rs <- 2
    CB.setRowData(blocks[[2]][[i]], a, rowIndex = 1, colOffset=0, showNA=TRUE, rowStyle=NULL)
    
    for (j in 1:length(digged_fuck[[i]])) {
      if (nrow(digged_fuck[[i]][[j]]) != 0) {
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][1:4]), startRow=rs, startColumn=1, showNA=TRUE, cellStyle=NULL)
        
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][5]), startRow=rs, startColumn=6, showNA=TRUE, cellStyle=cs_bucks)
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(sapply((digged_fuck[[1]][[j]][6]), as.double)), startRow=rs, startColumn=7, showNA=TRUE, cellStyle=NULL)
        
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][7]), startRow=rs, startColumn=9, showNA=TRUE, cellStyle=cs_zbucks)
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][8:10]), startRow=rs, startColumn=10, showNA=TRUE, cellStyle=cs_ddperc)
        
        CB.setMatrixData(blocks[[2]][[i]], as.matrix(digged_fuck[[i]][[j]][11:19]), startRow=rs, startColumn=14, showNA=TRUE, cellStyle=cs_zperc)
        rs <- rs+nrow(digged_fuck[[1]][[j]])+1
      }
    }
    
  }
  deep_name  <- paste("segments_top_kpi_weekly_TD_", as.character(Sys.Date()), ".xlsx", sep = "")
  saveWorkbook(wb, file = deep_name)
}

#---- DUMB WRITER ----------------------------------------------------------------------------------------------
dumb_writer <- function(td_deep) {
  td_long <- data.frame()
  for (i in 1:length(td_deep)) {
    names(td_deep[[i]])[4] <- "SEGMENT"
    td_long <- rbind(td_long, td_deep[[i]])
  }
  return(td_long)
}




