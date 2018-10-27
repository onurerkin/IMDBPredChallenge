useful_data = subset(data_final,select = -c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7,X.8,X.9,X.10,X.11))
useful_data = subset(useful_data,select = -c(release_day,release_month,release_year,language,country,plot_keywords,plot_summary,))

attach(useful_data)

useful_data$actor_2_star_meter=as.numeric(useful_data$actor_2_star_meter)
useful_data$actor_3_star_meter=as.numeric(useful_data$actor_3_star_meter)

useful_data$star_meter_inverse_sum=(actor_1_star_meter^-1+actor_2_star_meter^-1+actor_3_star_meter^-1)


useful_data$log_inverse_star_meter_1=log(actor_1_star_meter^-2)
useful_data$log_inverse_star_meter_2=log(actor_2_star_meter^-2)
useful_data$log_inverse_star_meter_3=log(actor_3_star_meter^-2)
useful_data$star_meter_sum_log = log_inverse_star_meter_1+log_inverse_star_meter_2+log_inverse_star_meter_3

attach(useful_data)
plot(log(star_meter_inverse_sum),imdb_score)
plot(star_meter_sum_log,imdb_score)

