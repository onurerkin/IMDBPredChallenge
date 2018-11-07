## PLOTTING PREDICTORS #####
attach(data_final)
detach(data_final)
#genre plots should be done laterrr

par(mfrow = c(1,1))

###### converting factors into integers #####
data_final$user_votes_number=as.numeric(data_final$user_votes_number)
data_final$critic_reviews_number=as.numeric(data_final$critic_reviews_number)
data_final$user_reviews_number = as.numeric(data_final$user_reviews_number)
data_final$actor_3_facebook_likes=as.numeric(data_final$actor_3_facebook_likes)
data_final$actor_2_star_meter=as.numeric(data_final$actor_2_star_meter)
data_final$actor_3_star_meter=as.numeric(data_final$actor_3_star_meter)
class(data_final)
myGlobals = objects(data_final)
for (i in myGlobals) {
  print(i)
  print(typeof(get(i)))
  
}

plot(number_news_articles,imdb_score,xlim = c(0,35000))

lreg1=lm(imdb_score~actor_1_facebook_likes+budget+number_of_votes+number_news_articles+critic_reviews_number+user_reviews_number+user_votes_number)
summary(lreg1)

residualPlots(lreg1)
ployreg=glm(imdb_score~poly(number_news_articles,2))
plot(number_news_articles,imdb_score)
lines(sort(number_news_articles),predict(ployreg)[order(number_news_articles)])

# budget,
#content_rating,
# number_news_articles,
# irector_facebook_likes,
# color,
# cast_total_facebook_likes
# movie_facebook_likes
# movie_budget
# sum_total_likes  = cast total facebook likes same
# ratio_movie_cast_likes maybe


library(easyGgplot2)
ggplot2.scatterplot(data=data_final, xName='critic_reviews_number',yName='imdb_score',
                    groupName='action', size=3,backgroundColor="white",
                    setColorByGroupName=TRUE,xlim=c(0,1000),addRegLine = TRUE)


install.packages("devtools")
library(devtools)
install_github("kassambara/easyGgplot2",force = TRUE)

plot(actor_1_facebook_likes,actor_1_star_meter,ylim = c(0,10000),xlim = c(0,10000))
