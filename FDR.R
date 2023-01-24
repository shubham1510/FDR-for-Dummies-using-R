#1
set.seed(5715)
#generating 10000 random numbers following normal distribution and replicating randomly 1001 times with mean = 0 and sd = 1
df <- data.frame(replicate(1001,rnorm(10000)))

#2
colnames(df)[1] = 'Y'

#3
model1 <- lm(Y ~ . + 0,data = df)
summary(model1)
print('Intercept is not needed because the model is on standard normalized data and passes through the origin.')

#4
p_values <- summary(model1)$coefficients[,4]
hist(p_values)
print('histogram shows p-values follow uniform distribution almost perfectly')

#5
count_significant <- sum(ifelse(p_values < 0.01,1,0))
count_significant
print('We observe 12 significant variables. Considering the data was generated randomly, we expected 0 significant variables however considering significance level of 0.01 and knowing that p-values have a uniform distribution, we expected 0.01*1000 i.e. ~10 significant variables.')

#6
p_values <- sort(p_values,decreasing = FALSE)
Rank <- c(1:1000)
ordered_p_values <- data.frame(Rank)
ordered_p_values$p_values <- p_values
#a data frame with ordered p-value including the intercept and the rank
ordered_p_values$calculation <- (ordered_p_values$Rank/1000)*0.1
#calculating p* for each variable at q = 0.1
ordered_p_values$comparision <- ifelse(ordered_p_values$p_values <= ordered_p_values$calculation,1,0)
#since we have run the significance test 1001 times (1000 for 1000 variables and once for intercept)
plot(ordered_p_values$Rank,ordered_p_values$p_values,lines(1:1000, 0.1*(1:1000)/1000))
print('After applying BH procedure we get 0 significant variable and conclude we have 12 false discoveries.')

#7
df <- read.csv('C:\\Users\\shubh\\Desktop\\MSBA\\Machine Learning\\Assignment 1\\autos.csv')
#only taking integer variables
df_integer <- df[,sapply(df,is.numeric) | sapply(df,is.integer)]
#using fastdummies library to create dummies for categorical variables
library(fastDummies)
df_categorical <- df[,sapply(df, is.character)]
df_dummies <- dummy_columns(df_categorical,remove_first_dummy = TRUE,select_columns = NULL)
#removing original variables from the data frame
df_dummies <- within(df_dummies,rm("make","fuel_type","aspiration","num_of_doors","body_style","drive_wheels","engine_location","engine_type","fuel_system"))
#merging the dummies and integer variables for running linear regression
df_new <- merge(df_dummies,df_integer,by = 'row.names')
df_new <- df_new[,-1]
plot(price ~ ., data = df_integer)
cor(df_integer)
lapply(1:ncol(df_categorical), function(i) boxplot(df$price ~ df_categorical[, i],xlab = colnames(df_categorical[i])))
aggregate(df$price,list(df$make),mean)
print('jaguar and porsche have significantly higher avg prices as compared to other makes.')
aggregate(df$price,list(df$fuel_type),mean)
aggregate(df$price,list(df$aspiration),mean)
aggregate(df$price,list(df$num_of_doors),mean)
print('No significant difference in the avgs of the two types based on number of doors.')
aggregate(df$price,list(df$body_style),mean)
print('Convertible and hardtop has significantly higher avg prices.')
aggregate(df$price,list(df$drive_wheels),mean)
print('rwd has significantly higher avg price.')
aggregate(df$price,list(df$engine_location),mean)
print('Rear engine type cars have significantly higher avg prices')
aggregate(df$price,list(df$engine_type),mean)
print('ohcv seems to have higher avg price as compared to other ')
aggregate(df$price,list(df$fuel_system),mean)
print('1bbl and 2bbl seem to have very low avg price as compared to other fuel systems.')

#8
model2 <- lm(price ~ .,data = df_new)
summary(model2)
print('The model with all the dummies gives decent results (Global F-statistic is significant).')
print('Since the dummies for engine type ohcf and idi are NA, it indicates that these variables have perfect linear relationship with another variable or a combination of other variables. Hence, dropping them.')
df_new_1 <- within(df_new,rm("engine_type_ohcf","fuel_system_idi"))
model3 <- lm(price ~ .,data = df_new_1)
summary(model3)  
print('The model with all the dummies and continuous variables gives decent results (Global F-statistic is significant). And the point of the exercise is to check for False Discoveries, hence I am working with the given model with maximum possible variables')

#9
#at alpha = 0.01, probability of atleast one false discovery (1 - prob of 0 false discoveries)
prob_0.01 <- 1 - (choose(53,0)*0.01^0*0.99^53)
#at alpha = 0.05
prob_0.05 <- 1 - (choose(53,0)*0.05^0*0.95^53)
#at alpha = 0.05
prob_0.1 <- 1 - (choose(53,0)*0.1^0*0.9^53)
print('We observe that the probability of at least one false discovery when running the significance test for 52 variables and 1 intercept is between 0.41 and 0.99 for the smallest and largest significance levels.')
print('Hence we are almost certain to have false discoveries.')

#10
#replicating the BH procedure from the 1st half of the exercise
table_model3 <- data.frame(summary(model3)$coefficients)
library(tibble)
table_model3 <- tibble::rownames_to_column(table_model3, "Variable")
table_model3 <- table_model3[order(table_model3$Pr...t..,decreasing = FALSE),]
table_model3$rank <- c(1:53)
table_model3$calculation <- (table_model3$rank/nrow(table_model3))*0.1
table_model3$comparision <- ifelse(table_model3$Pr...t.. <= table_model3$calculation,1,0)
plot(table_model3$rank,table_model3$Pr...t..,lines(1:53, 0.1*(1:53)/53))
table_model3 <- table_model3[,-c(2:4)]
print('After running the BH procedure, at significance level of 0.01 we observe that false discoveries will not have been an issue and we would have got 13 significant variables.')
print('We observe 23 significant variables at significance level of 0.05 but conclude 4 are false discoveries after applying the BH procedure.')
