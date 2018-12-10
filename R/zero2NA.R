#' Replace rowsum of zero to NA
#' @details  if there is no resturant for an agent, replace the number of resturant (i,e, 0) to NA
#' @param df the dataframe which needs to be processed
#' @param baseon the dataframe which the calcualtion is based on. It could be the same as the df to be processed.
#' @export
#'
zero2NA = function(df, baseon)
{
  df[which(apply(baseon, 1, sum)==0)] = NA
  df
}
