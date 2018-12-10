#' Entropy of the food environment
#' @param df_res the restaurant dataframe
#' @param df_tot the restaurant dataframe for the "scale", can be restaurant of all resturants, or the same as only e.g. take-home, by default is the same as df_res
#' @export

entr= function(df_res, df_tot = df_res)
{
  NC_ff = sum(df_tot!=0)     # number of restaurant category
  NA_ff = sum(df_tot)          # number of restaurants
  prob = sapply(X=df_res, FUN = function(x) x/NA_ff )  # probability of a certain restaurant
  prob[prob==0]=NA       # 0 is assigned to NA to avoid log(0)
  prlog=  prob*log(prob)
  su = -sum(prlog[which(!is.na(prlog))])    # sum of all the probabilities*log(probabilities)
  su/NC_ff              # devide by number of categories
}
