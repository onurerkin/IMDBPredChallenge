## PLOTTING PREDICTORS #####

data_final <- read.csv("~/Downloads/data_final.csv")
attach(data_final)
par(mfrow=c(1,1))
plot(duration_mins,imdb_score) # maybe
plot(release_year,imdb_score) # bs
plot(budget,imdb_score) # USE IT funnel test
plot(content_rating,imdb_score)  # useful, categorical, outliers
plot(aspect_ratio,imdb_score) #useless
plot(director,imdb_score) #leave it
plot(number_news_articles,imdb_score) #important funnel
plot(director_facebook_likes,imdb_score) #use it as categrical
plot(sum_total_likes,imdb_score,xlim = c(0,100000)) # we sum actors' facebook likes
plot(actor_1_star_meter,imdb_score,xlim = c(0,100000)) # we sum actors' star likes
plot(movie_meter_IMDB_pro,imdb_score)

plot(user_vote_number,imdb_score) #type is factor

plot(critic_reviews_number,imdb_score)#type is factor
plot(user_reviews_number,imdb_score) #same
plot(color,imdb_score) #use it
plot(genres,imdb_score)
plot(cast_total_facebook_likes,imdb_score,xlim = c(0,50000)) #useful
plot(number_of_faces_in_movie_poster,imdb_score,xlim=c(0,20)) #maybeee useful or not

plot(plot_keywords,imdb_score) #useless
plot(movie_facebook_likes,imdb_score,xlim = c(1000,15000)) #niceee
#genre plots should be done laterrr

plot(sum_total_likes,imdb_score,xlim=c(0,100000)) #useful
plot(ratio_movie_cast_likes,imdb_score,xlim=c(0,300)) #useful

plot(movie_meter_IMDB_pro,imdb_score,xlim = c(0,50000)) #maybe
plot(number_of_votes,imdb_score)  #awesomeeeee 
plot(plot_summary,imdb_score) #disgusting

plot(movie_facebook_likes,imdb_score, xlim = c(0,1000)) #use it
plot(movie_budget,imdb_score,xlim = c(0,100000000))



###### converting factors into integers #####
data_final$user_votes_number=as.numeric(data_final$user_votes_number)
attach(data_final)
plot(user_vote_number,imdb_score) #type is factor

data_final$critic_reviews_number=as.numeric(data_final$critic_reviews_number)
attach(data_final)
plot(critic_reviews_number,imdb_score)#type is factor

data_final$user_reviews_number = as.numeric(data_final$user_reviews_number)
attach(data_final)
plot(user_reviews_number,imdb_score) #same


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


library(ggplot2)
# ggplot2.scatterplot(data=data_final, xName='budget',yName='imdb_score',
#                     groupName='genres', size=3,backgroundColor="white", 
#                     setColorByGroupName=TRUE,xlim=c(0,100000000),addRegLine = TRUE)


install.packages("devtools")
library(devtools)
install_github("kassambara/easyGgplot2")
