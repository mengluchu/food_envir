#' Replace names of a dataset
#'  @param dataset the dataset
#' @param name2replace greplstyle name to replace, e.g. "loc"
#' @param replacenames the names to replace to, a list match the length of names to be replaced
#' @export
#'
renaming = function(dataset, names2replace="loc", replacenames)
{
  namesd2 =names(dataset)[which(grepl(names2replace, names(dataset)))]
  rp = rep(sapply(replacenames, function(x) substr(x, start = 15, stop =nchar(x)-4)), each = 4)
  trp =  rep(sapply(replacenames, function(x) substr(x, start = 1, stop =14)),each = 4)

  fn = 0
  le = length(namesd2)
  for ( i in 1: le)
  {
    fn = rbind(fn, gsub(trp[i], rp[i],namesd2[i] ))
  }
  fn = fn[-1]

  names(dataset)[which(grepl(names2replace, names(dataset)))] = fn
  return(dataset)
}
