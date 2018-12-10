#' Replace the element of a vector which based on another dataframe has the rowsum of zero to NA
#' @details  if there is no resturant for an agent, replace the number of resturant (i,e, 0) to NA
#' @param vec the vector which needs to be processed
#' @param baseon the dataframe which the calcualtion is based on.
#' @export
#'
zero2NA = function(vec, baseon)
{
  df[which(apply(baseon, 1, sum)==0)] = NA
  df
}
