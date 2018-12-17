#‘ t-test to compare dietary indices of different score categories. Higher than -1, (-1, -19), lower than -19
#' @param  df the dataframe
#' @param nationality
#' @example Complete = read.csv("combine_healthscore.csv"); levels(Complete,$H1_etniciteit);ttesthscore(Complete, "Turks"); ttesthscore(Complete, "Nederlands")
ttesthscore = function(df, Nationality){
  scoreC1= df%>%filter(H1_etniciteit==Nationality&heath_score100 >(-1))
  scoreC2= df%>%filter(H1_etniciteit==Nationality&heath_score100 <(-1)&heath_score100>(-19))
  scoreC3= df%>%filter(H1_etniciteit==Nationality&heath_score100<(-19) )


  print(c(
    t.test(na.omit(scoreC1$Total_AHEI_Score),na.omit(scoreC2$Total_AHEI_Score))$p.value,
    t.test(scoreC1$Total_AHEI_Score,scoreC3$Total_AHEI_Score)$p.value,
    t.test(scoreC2$Total_AHEI_Score,scoreC3$Total_AHEI_Score)$p.value,

    t.test(scoreC1$Total_MDS,scoreC2$Total_MDS )$p.value,
    t.test(scoreC1$Total_MDS ,scoreC3$Total_MDS)$p.value,
    t.test(scoreC2$Total_MDS ,scoreC3$Total_MDS)$p.value,


    t.test(scoreC1$Total_DASH_Score,scoreC2$Total_DASH_Score)$p.value,
    t.test(scoreC1$Total_DASH_Score,scoreC3$Total_DASH_Score)$p.value,
    t.test(scoreC2$Total_DASH_Score ,scoreC3$Total_DASH_Score)$p.value))

}
