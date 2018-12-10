#' Calculate healthiness Score
#' @param df_res the dataframe of the restaurant environment(count within a buffer)
#' @param  df_tot  dataframe of the restaurant environment for normalisation. By default same as the df_res
#' @param  score the healthiniess score
#' @param normalise if normalise is Ncati, normalise by the number of restaurant categotris, e.g. if there are 3 fastfood and 5 cafe, the number of categories is 2. If the normalise is Nall, then normalise by the total number of resturant, in the previous example: 8.
#' @param method method to calculate healthiness score, can be from c("simple", "expFun", “threshold")
#' @details all the 0 restaurants records are NA.  if method is "simple", healthScore = sum(NumberRestau \* score). The method "expFun" calculate healthiness score with an exponential function of the number of restaurants sum(log(numberOfrestaurant+1) \* score). The "threshold" method treats more than a certain number (i.e. threshold) restaurants as the certain number of resturants
#' @example healthScore(d_res_100m, d_res_100m,score_res, "Ncati", "expFun")
#' @export
#'


healthScore = function(df_res, df_tot = df_res, score, normalise = c("Ncati", "Nall"), method = c("simple", "expFun", "threshold"), threshold =2  ){
  repl = function(df, threshold)
  {
    df[df >= threshold] = threshold
    df
  }

  resN=  switch(method,
    "simple" = df_res,
    "expFun" = log(df_res + 1), #natual base, all the bases should result in same correlation.
    "threshold" = repl(df_res, threshold)
   )

  mul = sweep(resN, 2, score, "*")
  S_ff = apply(mul,1,sum)

  # normalise
  if (normalise%in% c("Ncati","Nall"))
  {
    NC_ff = apply(df_tot, 1, function(x) sum(x!=0)) # category, plus 0.01 to avoid 0
    NA_ff = apply(df_tot, 1, sum)  # total number, plus 0.01 to avoid 0.

    NC_ff[which(NC_ff == 0)] = NA
    NA_ff[which(NA_ff == 0)] = NA # if the row sum is 0, return NA.

    S_ff = switch(normalise,
         "Ncati" = S_ff/NC_ff,
         "Nall" =  S_ff/NA_ff)
  }
  S_ff = zero2NA(S_ff, df_res)
  S_ff
}
