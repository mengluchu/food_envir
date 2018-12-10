library(devtools)
install_github("mengluchu/food_envir")
library(foreign)
dataset = read.spss("C:/Users/lu000012/Documents/files/foodenv/170303_HELIUS data Maartje Poelman.sav", to.data.frame=TRUE)

dothers =  dataset[,-which(grepl("loc|Total", names(dataset)))]
load("C:/Users/Lu000012/Documents/files/foodenv/categorynames.Rdata")

# rename the dataset from loc to categorynames
d1 = renaming(dataset, "loc", categorynames )

# remove resturant that are not related to health (suggested by Maartje)
d2_omit = d1[, -which(grepl( "Seks|Tabak|Ziekenh|Levensmid|Koffiehuis|Coffeeshop|Disco|Hotelb|Partycentrum|Horeca" ,names(d1)))]


#subset all the resturant
dres = subset_grep(d2_omit, "0m")

#subset 100m resturant

d_res_100m = subset_grep(d_res, "100m")

#subset 500m and 1000m
d_res_500m =  subset_grep(d_res, "500m")
d_res_1000m =  subset_grep(d_res, "1000m")

#separate take-home and ready to eat.
homeindex = "Groente_Fr|Bakker|Toko|Koffie_Thee|Delicatessen|Kaas|Minisuper|Notenb|Poelier|Reform|Slagerij|Supermark|Visb"
Home_res = subset_grep(d_res, homeindex )

Home_res_100m = subset_grep(Home_res, "100m")
Home_res_500m = subset_grep(Home_res, "500m")


ready_res = d_res[,-which(grepl(home_index,names(d_res)))]


ready_res_100m =subset_grep(ready_res, "100m")
ready_res_500m =subset_grep(ready_res, "500m")

# separate fast food
fastfoodindex = "Fastfood|Grill|Bezorg"
fastfood_res =  subset_grep(ready_res, fastfoodindex )
fastfood_50m =  subset_grep(fastfood_res, "50m")
fastfood_100m =  subset_grep(fastfood_res, "100m")

#scores
score_fastfood = c(-4.9,-3.7,-4.9)
score_res = c(4.8, 1.2, -4.4, 1.5, -4.3, 1.3, 0.2, -1.3, 1.4, 2.2, 0.6, 1.0,-0.4, -4.6, 1.8,
               2.8, -4.7, -2.3, -4.9, -3.7, -4.9, -0.9, -3.9, -1.5, -3.2, -1.9, -0.9)

score_home = c(4.8, 1.2, 1.5, 1.3, 0.2, -1.3, 1.4, 2.2, 0.6, 1.0, -0.4, 1.8, 2.8)
score_ready = c(-4.4, -4.3, -4.6, -4.7, -2.3, -4.9, -3.7, -4.9, -0.9, -3.9, -1.5, -3.2, -1.9, -0.9)

#NR: number of a certain type of restaurant
#NA: number of all the restaurant
#NC: number of categories

####Direct sum of score
# S1:   sum(NR \* score)

###Normalise with all kinds of restaurants
##### 100m, 500m, 1000m buffer

dfs100m = S1(d_res_100m ,  score_res)
dfs500m = S1(d_res_500m ,  score_res)
dfs1000m = S1(d_res_1000m ,  score_res)

# records with no resturant are replaced with NA
dfs100m = zero2NA(dfs100m, d_res_100m)
dfs500m = zero2NA(dfs500m, d_res_500m)
dfs1000m = zero2NA(dfs1000m, d_res_1000m)

summary(dfs1) #23494, 13541 NA
Complete = cbind(dothers, d2_omit, heath_score100 = dfs1, heath_score500 = dfs2, health_score1000 = dfs3)
#summary(Complete[, 268:270])

#write.csv(Complete, file = "combine_healthscore.csv")

# paired t-test
ss =  dfs100m
scoreC1= d2_omit[which(ss >(-1)),]
scoreC2= d2_omit[which(ss >(-10)&ss <(-2)), ]
scoreC3= d2_omit[which(ss >(-11)&ss <(-19)), ]
scoreC4= d2_omit[which(ss <(-19)),]


t.test(na.omit(scoreC1$Total_AHEI_Score),na.omit(scoreC2$Total_AHEI_Score))
t.test(scoreC1$Total_AHEI_Score,scoreC4$Total_AHEI_Score)
t.test(scoreC2$Total_AHEI_Score,scoreC4$Total_AHEI_Score)

t.test(scoreC1$Total_MDS,scoreC2$Total_MDS )
t.test(scoreC1$Total_MDS ,scoreC4$Total_MDS)
t.test(scoreC2$Total_MDS ,scoreC4$Total_MDS)


t.test(scoreC1$Total_DASH_Score,scoreC2$Total_DASH_Score)
t.test(scoreC1$Total_DASH_Score,scoreC4$Total_DASH_Score)
t.test(scoreC2$Total_DASH_Score ,scoreC4$Total_DASH_Score)




#Mean Differences between category 2 and 1 (Category 2- category 1) . The ordered variables are printed (descending order). It shows the lower score have the highest energy intake.
# plot differences
plot(dif21, typ = "h" )
idxorder = order(dif21,decreasing = T)
names(d3)[idxorder]
dfs1_home = S1(Home_res[,hind100m], d_res, score_home)

#test(dfs1)
#writeLines("td, th { padding : 6px } th { background-color : brown ; color : white; border :  1px solid white; } td { color : brown ; border : 1px solid brown }", con = "mystyle.css")










