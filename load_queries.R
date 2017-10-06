Select <- readtext(paste0("queries/", "Select.txt"))[[2]]
from  <- readtext(paste0("queries/", "from.txt"))[[2]]
where_raw  <- readtext(paste0("queries/", "where_raw.txt"))[[2]]
where_marina  <- readtext(paste0("queries/", "where_marina.txt"))[[2]]
where  <- readtext(paste0("queries/", "where.txt"))[[2]]
groupby  <- readtext(paste0("queries/", "groupby.txt"))[[2]]

auth_ok <- readtext(paste0("queries/", "auth_ok.txt"))[[2]]

auth_ok_w <- readtext(paste0("queries/", "auth_ok_wednesday_run.txt"))[[2]]
ref_date <- readtext(paste0("queries/", "ref_date.txt"))[[2]]

w_via <- readtext(paste0("queries/", "w_via.txt"))[[2]]

range_dates <- readtext(paste0("queries/", "range_dates.txt"))[[2]]
trange_dates <- readtext(paste0("queries/", "trange_dates.txt"))[[2]]
prange_dates <- readtext(paste0("queries/", "prange_dates.txt"))[[2]]