#'generate table to compare healthscore and AHEI, MDS, DAS under linear regression
#'@param healthscore1 healthiness score
<<<<<<< HEAD
#'@param AHEI dietary score AHEI
#'@param MDS dietary score MDS
#'@param DAS dietary score DAS
#'@param xlabplot the name of the xlab
#'@export
#'
LMtable = function(healthscore1,AHEI,MDS,DAS, xlabplot="sum of healthiness score" ){
=======
#'@param xlabplot the name of the xlab
#'@export
#'
LMtable = function(healthscore1, xlabplot="sum of healthiness score" ){
>>>>>>> 81cd23fb8140a1a92d24e61dfc4bc2fe559820be

  datadf=na.omit(data.frame (healthscore=healthscore1, AHEI,MDS,DAS))


  fit = lm(healthscore~AHEI,data= datadf)
<<<<<<< HEAD
 # plot(fit)
 # fit = lm( log(healthscore + 1 - min(healthscore))~AHEI, data = datadf)
 # plot(fit)
=======
  plot(fit)
  fit = lm( log(healthscore + 1 - min(healthscore))~AHEI, data = datadf)
  plot(fit)
>>>>>>> 81cd23fb8140a1a92d24e61dfc4bc2fe559820be
  p_v = summary(fit)$coefficients[2,4]
  slope = summary(fit)$coefficients[2,1]
  con95 =  confint(fit, "AHEI", level = 0.95)
  df = data.frame(pvalue = p_v, slope = slope, con95)

  fit = lm(healthscore~MDS, data= datadf)

  p_v = summary(fit)$coefficients[2,4]
  slope = summary(fit)$coefficients[2,1]
  con95 =  confint(fit, "AHEI", level = 0.95)
  df = rbind(df, c( p_v,  slope,con95))

  fit = lm(healthscore~DAS, data= datadf)

  p_v = summary(fit)$coefficients[2,4]
  slope = summary(fit)$coefficients[2,1]
  df = rbind(df, c( p_v,  slope,con95))
  row.names(df) = c("AHEI","MDS","DAS")

  plot(healthscore1, AHEI, xlab= xlabplot, ylab = "AHEI")
  plot(healthscore1, MDS, xlab= xlabplot, ylab = "MDS")
  plot(healthscore1, DAS, xlab= xlabplot, ylab = "DASH")

  return(df)
}
