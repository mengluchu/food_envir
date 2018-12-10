#' calculate the entropy score (by brainstorm)
#' @param home_res the take-home restaurants environement
#' @param ready_res the ready to eat restaurants environment
#' @param df_tot the df_tot as in the entr function.
#' @example entr_score(Home_res_100m,ready_res_100m,d_res_100m)
#' @export

entr_score = function(home_res, ready_res, df_tot)
{
  homeent = apply(Home_res, 1, entr, df_tot  )
  sumhome=apply(Home_res, 1, sum)
  home1 =homeent * sumhome

  readyent = apply(ready_res, 1, entr, df_tot )
  sumready=apply(ready_res,1,sum)
  ready1 = readyent*sumready

  entroinde=home1/ready1
  entroinde[entroinde<0] = NA
  entroinde
}
