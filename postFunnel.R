wtl  <- function(z) {
  z[nrow(z), c("rev", "logins", "rev2logins", "charged2logins", "attempts2logins", "red2logins", "rv2l_diff",
               "c2l_effect", "ac_effect", "c2l_diff", "a2l_effect",  "ar_effect", "a2l_diff",  "r2l_effect",  "ap_effect")]
}