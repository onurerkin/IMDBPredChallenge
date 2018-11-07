###### converting factors into integers #####
 # please run the code until for loop part to get the dataset we will be using.
 # the dataframe "data2" is the dataframe we get at the end. 
data_final <- read.csv("~/Desktop/McGill/Fall/Multivariate Statistical Analysis/Midterm/data_final.csv", stringsAsFactors=FALSE)

attach(data_final)
data_final$user_votes_number=as.numeric(data_final$user_votes_number)
data_final$critic_reviews_number=as.numeric(data_final$critic_reviews_number)
data_final$user_reviews_number = as.numeric(data_final$user_reviews_number)
data_final$actor_3_facebook_likes=as.numeric(data_final$actor_3_facebook_likes)
data_final$actor_2_star_meter=as.numeric(data_final$actor_2_star_meter)
data_final$actor_3_star_meter=as.numeric(data_final$actor_3_star_meter)
detach(data_final)
attach(data_final)


# Dealing with max(facebook) and min(star meter) ####

# we realized those are not very useful predictors :(
data_final$movie_star_facebook_like = apply(subset(data_final,select = c(actor_1_facebook_likes,actor_2_facebook_likes,actor_3_facebook_likes)),1,max)
data_final$max_star_rating = (apply(subset(data_final,select = c(actor_1_star_meter,actor_2_star_meter,actor_3_star_meter)),1,min))
detach(data_final)
attach(data_final)

# removing unnecssary columns ####
# for sake of simplicity
data_final = subset(data_final,select = -c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7,X.8,X.9,X.10,X.11))
data_final = subset(data_final,select = -c(release_day,release_year,language,
                                           country,title,movie_id,aspect_ratio,
                                           actor_1_known_for,actor_2_known_for,
                                           actor_3_known_for,movie_imdb_link,color,genres,
                                           plot_keywords,number_of_faces_in_movie_poster,
                                           user_votes_number,
                                           user_reviews_number))
detach(data_final)
attach(data_final)




#### best subset linear ####
# just to see best model for linear case, we don't use this part in practice
library(leaps)
sel=regsubsets(imdb_score~movie_star_facebook_like+budget+
                 number_news_articles+critic_reviews_number+director_facebook_likes
                 +max_star_rating+ratio_movie_cast_likes+duration_mins,data_final,nvmax = 20)
summary(sel)
summary.sel=summary(sel)
which.max(summary.sel$adjr2)


bestlinear=lm(imdb_score~movie_star_facebook_like+budget+
                number_news_articles+critic_reviews_number+director_facebook_likes
              +max_star_rating+ratio_movie_cast_likes+duration_mins)
summary(bestlinear)

#### removing missing values ####
# this code removes all rows with missing values
# we might use some other strategies for missing values (like replacing with mean etc.)
data2=data_final[complete.cases(data_final), ]

detach(data_final)
detach(data2)
attach(data2)


#### MODEL ####


#outliers ####
library(car)
outlierTest(bestlinear,n.max = 100)
outs<-outlierTest(bestlinear,n.max = 100)
out_index<-names(outs$bonf.p)
data2<-data2[!(as.integer(rownames(data2)) %in% out_index),]
detach(data2)
attach(data2)



library(dplyr)


# data2=data2 %>% group_by(director) %>% mutate(avg_imdb_director = mean(imdb_score))
# data2=data2 %>% group_by(production_company) %>% mutate(avg_imdb_production = mean(imdb_score))
# data2=data2 %>% group_by(director) %>% mutate(director_count=ifelse(count()>3,avg_imdb_director,0))
# detach(data2)
# attach(data2)




# RUN IT TILL HERE !!!!!!!!!!



#####  FOR LOOP FOR BEST POLY AND SPLINES ######
# It didn't work. we should debug it ( we can try a simpler loop instead of a comlicated one)
library(caTools)
library(splines)
library(boot)
# 
# 
# meanMSE = c()
# 
# 
# degrees_1 = c()
# degrees_2 = c()
# degrees_3 = c()
# degrees_4 = c()
# degrees_5 = c()
# 
# 
# k=0
# 
# for (a1 in 1:3){
#     for (a2 in 1:3){
#        for (a3 in 1:3){
#            for (a4 in 1:3){
#                for (a5 in 1:3){
#     
#                   print(k)
#                   k=k+1
#                   
#                   q3reg= glm(imdb_score~poly(budget,a1)+
#                                   poly(number_news_articles,a2)
#                                 +content_rating+drama+action+thriller+adventure+horror+poly(duration_mins,a3)
#                                 +poly(movie_star_facebook_like,a4)+poly(director_facebook_likes,a5),data = data2)
#                   MSE_inside=cv.glm(data2,q3reg,K=2)$delta[1]
#                 
#                 #Storing MSE and knots
#                 meanMSE = append(meanMSE, MSE_inside)
#                 
#                 budget_loop = append(budget_loop,a1)
#                 number_of_votes_loop = append(number_of_votes_loop,a2)
#                 number_news_articles_loop = append(number_news_articles_loop,a3)
#                 critic_reviews_number_loop = append(critic_reviews_number_loop,a4)
#                 user_reviews_number_loop = append(user_reviews_number_loop,a5)
# 
#                 
#                  }
#                }
#              }
#            }
# }



#Storing all results of the for loop in a dataframe
# results = data.frame(meanMSE)
# results$meanMSE = meanMSE
# results$a1 = budget_loop
# results$a2 = number_of_votes_loop
# results$a3 = number_news_articles_loop
# results$a4 = critic_reviews_number_loop
# results$a5 = user_reviews_number_loop
# 
# results[which.min(results$meanMSE),]


##########  MSE CALCULATION ####



data2=data2 %>% group_by(production_company) %>% mutate(avg_imdb_production=(mean(imdb_score)))

data2=data2 %>% group_by(distributor) %>% mutate(avg_imdb_dist=(mean(imdb_score)))


data2=data2 %>% group_by(actor_1_name) %>% mutate(avg_imdb_act1=(mean(imdb_score)))


data2 = data2 %>% group_by(director) %>% mutate(avg_imdb_director=(mean(imdb_score)))

detach(data2)
attach(data2)


mse =c()
for (i in 1:200) {
  
  library(caTools)
  sample=sample.split(data2$imdb_score,SplitRatio = 0.700)
  train=subset(data2,sample==TRUE)
  test=subset(data2,sample==FALSE)
  
  
  bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
                 +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                   max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
  summary(bestcase2)
  onur = mean((test$imdb_score-(predict(bestcase2,test)))^2)
  mse = append(mse,onur)
}
print(mean(mse))


#### MOVIES ####


grinch=data.frame(budget=75000000,number_news_articles=150,content_rating='PG',
                  drama=0,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=573000,
                  duration_mins=90,movie_star_facebook_like=1000000,movie_meter_IMDB_pro=93,
                  director_facebook_likes=1500,max_star_rating=226,
                  production_company='Universal Pictures',director='Yarrow Cheney',
                  distributor='Universal Pictures',actor_1_name='Benedict Cumberbatch')


london=data.frame(number_news_articles=5,content_rating='PG',
                  drama=1,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=1000,
                  duration_mins=88,movie_star_facebook_like=2360,movie_meter_IMDB_pro=7370,
                  director_facebook_likes=100,max_star_rating=2074, release_month='Nov',
                  production_company='Diablo Films',director='Steve McLean',
                  distributor='Strand Releasing',actor_1_name='Harris Dickinson')


long_dumb_road=data.frame(budget=75000000,number_news_articles=41,content_rating='R',
                          drama=0,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=113,
                          duration_mins=88,movie_star_facebook_like=100000,movie_meter_IMDB_pro=9374,
                          director_facebook_likes=0,max_star_rating=119,
                          production_company='Gamechanger Films',director='Hannah Fidell',
                          distributor='The Film Arcade',actor_1_name='Taissa Farmiga')

grindelwald=data.frame(number_news_articles=640,content_rating='PG-13',
                       drama=0,action=0,thriller=0,adventure=1,horror=0,movie_facebook_likes=34000,
                       duration_mins=134,movie_star_facebook_like=1000000,movie_meter_IMDB_pro=32,
                       director_facebook_likes=1000,max_star_rating=41,
                       production_company='Heyday Films',director='David Yates',
                       distributor='Warner Bros.',actor_1_name='Johnny Depp')

Instant_Family=data.frame(number_news_articles=65,content_rating='PG',
                          drama=0,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=36743,
                          duration_mins=119 ,movie_star_facebook_like=17319613,movie_meter_IMDB_pro=304,
                          director_facebook_likes=1000,max_star_rating=131, release_month='Nov',
                          production_company='Paramount Pictures',director='Sean Andres',
                          distributor='Paramount Pictures',actor_1_name='Mark Wahlberg')

Robin_Hood =data.frame(number_news_articles=289,content_rating='PG',
                       drama=0,action=1,thriller=0,adventure=116,horror=0,movie_facebook_likes=45930,
                       duration_mins=119,movie_star_facebook_like=42733,movie_meter_IMDB_pro=144,
                       director_facebook_likes=1000,max_star_rating=71, release_month='Nov',
                       production_company='Appian Way',director='Otto Bathurst',
                       distributor='Lionsgate',actor_1_name='Taron Egerton')

Creed2 =data.frame(number_news_articles=218,content_rating='PG',
                   drama=1,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=1000000,
                   duration_mins=117,movie_star_facebook_like=1000000,movie_meter_IMDB_pro=98,
                   director_facebook_likes=1000,max_star_rating=160, release_month='Nov',
                   production_company='Metro-Goldwyn-Mayer Studios',director='Steven Caple',
                   distributor='Annapurna Pictures',actor_1_name='Tessa Thompson')


Killer=data.frame(number_news_articles=19,content_rating='PG',
                  drama=1,action=0,thriller=0,adventure=0,horror=0,
                  duration_mins=110,movie_meter_IMDB_pro=8629,
                  max_star_rating=502,
                  production_company='End Cue',director='Steven Caple',
                  distributor='IFC Midnight',actor_1_name='Tessa Thompson')

women_of_marwen=data.frame(number_news_articles=758,content_rating='PG',
                           drama=1,action=0,thriller=0,adventure=0,horror=0,movie_facebook_likes=1000000,
                           duration_mins=117,movie_star_facebook_like=9669942,movie_meter_IMDB_pro=98,
                           director_facebook_likes=6901,max_star_rating=264, release_month='Nov',
                           production_company='DreamWorks',director='Robert Zemeckis',
                           distributor='Universal Pictures',actor_1_name='Steve Carell')

ralph_breaks=data.frame(number_news_articles=153,content_rating='PG',
                        drama=1,action=0,thriller=0,adventure=1,horror=0,movie_facebook_likes=1000000,
                        duration_mins=112,movie_star_facebook_like=9669942,movie_meter_IMDB_pro=153,
                        director_facebook_likes=6901,max_star_rating=136, release_month='Nov',
                        production_company='Walt Disney Animation Studios',director='Robert Zemeckis',
                        distributor='Walt Disney Studios Motion Pictures',actor_1_name='John C. Reilly')

becoming_astrid=data.frame(number_news_articles=8,content_rating='PG',
                        drama=1,action=0,thriller=0,adventure=1,horror=0,
                        duration_mins=123,movie_meter_IMDB_pro=	4291,
                        max_star_rating=7631, release_month='Nov',
                        production_company='Nordisk Film Production Sverige AB',director='Pernille Fischer Christensen',
                        distributor='Music Box Films',actor_1_name='Alba August')

second_act=data.frame(number_news_articles=96,content_rating='PG',
                      drama=0,action=0,thriller=0,adventure=1,horror=0,movie_facebook_likes=1000000,
                      duration_mins=120,movie_star_facebook_like=9669942,movie_meter_IMDB_pro=1389,
                      director_facebook_likes=6901,max_star_rating=237, release_month='Nov',
                      production_company='STX Entertainment',director='Robert Zemeckis',
                      distributor='STX Entertainment',actor_1_name='Milo Ventimiglia')

#### PREDICTIONS ######

#predicting grinch
prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=grinch, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

predict(bestcase2,test)

#predictiong grindelwald

prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=grindelwald, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

predict(bestcase2,test)


#predicting london

prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=london, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )


predict(bestcase2,test)


#long dumb road


prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=long_dumb_road, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )
predict(bestcase2,test)


#predicting instant family

prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=Instant_Family, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )
predict(bestcase2,test)

# Prdicting Robin Hood


prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=Robin_Hood, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )
predict(bestcase2,test)

#Predicting Creed 2

prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=Creed2, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)

#predicting killer

prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=Killer, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)


#women of marwen predict


prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=women_of_marwen, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)

#predicting ralph breaks



prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=ralph_breaks, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)




#predicting becoming astrid



prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=becoming_astrid, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)


#predicting second act


prod_avg=data2 %>% group_by(production_company) %>% summarise(avg_imdb_production=mean(imdb_score))
test=merge(x=second_act, y=prod_avg, by = 'production_company',all.x= TRUE)
na_idxs_prod=which(is.na(test$avg_imdb_production))
test[(as.integer(rownames(test)) %in% na_idxs_prod),"avg_imdb_production"]=mean(prod_avg$avg_imdb_production)

dist_avg=data2 %>% group_by(distributor) %>% summarise(avg_imdb_dist=mean(imdb_score))
test=merge(x=test, y=dist_avg, by = 'distributor',all.x=TRUE)
na_idxs_dist=which(is.na(test$avg_imdb_dist))
test[(as.integer(rownames(test)) %in% na_idxs_dist),"avg_imdb_dist"]=mean(dist_avg$avg_imdb_dist)

act1_avg=data2 %>% group_by(actor_1_name) %>% summarise(avg_imdb_act1=mean(imdb_score))
test=merge(x = test, y = act1_avg, by = 'actor_1_name', all.x = TRUE)
na_idxs_act1=which(is.na(test$avg_imdb_act1))
test[(as.integer(rownames(test)) %in% na_idxs_act1),"avg_imdb_act1"]=mean(act1_avg$avg_imdb_act1)

dir_avg=data2 %>% group_by(director) %>% summarise(avg_imdb_director=mean(imdb_score))
test=merge(x=test, y=dir_avg, by = 'director',all.x=TRUE)
na_idxs_dir=which(is.na(test$avg_imdb_director))
test[(as.integer(rownames(test)) %in% na_idxs_dir),"avg_imdb_director"]=mean(dir_avg$avg_imdb_director)


bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =data2 )

bestcase2= glm(imdb_score~poly(number_news_articles,2)+action
               +duration_mins+poly(avg_imdb_production,3)+drama+poly(movie_meter_IMDB_pro,3)+
                 max_star_rating+poly(avg_imdb_dist,3)+poly(avg_imdb_act1,2)+poly(avg_imdb_director,2),data =train )
predict(bestcase2,test)