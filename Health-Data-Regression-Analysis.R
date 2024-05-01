#Load the data into R
summary(heart_data)


#Make sure your data meet the assumptions
cor(heart_data$biking, heart_data$smoking)


hist(heart_data$heart.disease)


plot(heart.disease ~ biking, data=heart_data)

plot(heart.disease ~ smoking, data=heart_data)

#Perform the linear regression analysis
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart_data)

summary(heart.disease.lm)

#Check for homoscedasticity
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))


#Visualize the results with a graph
plotting.data<-expand.grid(
  biking = seq(min(heart_data$biking), max(heart_data$biking), length.out=30),
  smoking=c(min(heart_data$smoking), mean(heart_data$smoking), max(heart_data$smoking)))


#Predict the values of heart disease based on your linear model
plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata=plotting.data)


#Round the smoking numbers to two decimals
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)


#Change the ‘smoking’ variable into a factor
plotting.data$smoking <- as.factor(plotting.data$smoking)


install.packages("ggplot2")
library(ggplot2)

#Plot the original data
heart.plot <- ggplot(heart_data, aes(x=biking, y=heart.disease)) +
  geom_point()

heart.plot


#Add the regression lines
heart.plot <- heart.plot +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)

heart.plot



# Make the graph ready for publication
heart.plot <-
  heart.plot +
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")

heart.plot



heart.plot + annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")
