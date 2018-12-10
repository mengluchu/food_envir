#' healthiness score1
#' @param df_res the dataframe of the resturant environment(count within a buffer)
#' @param  score the healthiniess score
#' @details calculated as sum(score * foodenv_count)
#' @export
#'
S1 = function(df_res,  score)
{
  mul = sweep(df_res,2, score, "*")
  apply(mul,1,sum)
}
