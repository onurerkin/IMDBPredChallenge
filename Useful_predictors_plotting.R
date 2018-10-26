useful_data = subset(data_final,select = -c(X.1,X.2,X.3,X.4,X.5,X.6,X.7,X.8,X.9,X.10,X.11))
useful_data = subset(useful_data,select = -c(release_day,release_month,release_year,language,country,plot_keywords,plot_summary))

