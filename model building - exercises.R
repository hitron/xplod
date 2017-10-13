# EXERCISES DATA SCIENCE BOOTCAMP
# ADAPTED FROM DATACAMP.COM

# DATA DISCOVERY - MODEL BUILDING AND VALIDATION
# LOAD LIBRARIES
# Don't forget to install them first!
library(tidyverse)
library(rpart)
library(titanic)

# LOAD DATASETS
titanic <- titanic::titanic_train %>%
  select(c(2,3,5,6))



# The Confusion Matrix----
# Have you ever wondered if you would have survived the Titanic disaster in 1912? Our friends from Kaggle have some historical data on this event. The titanic dataset is already available in your workspace.
# 
# In this exercise, a decision tree is learned on this dataset. The tree aims to predict whether a person would have survived the accident based on the variables Age, Sex and Pclass (travel class). The decision the tree makes can be deemed correct or incorrect if we know what the person's true outcome was. That is, if it's a supervised learning problem.
# 
# Since the true fate of the passengers, Survived, is also provided in titanic, you can compare it to the prediction made by the tree. As you've seen in the video, the results can be summarized in a confusion matrix. In R, you can use the table() function for this.
# 
# 

# Instructions
# Have a look at the structure of titanic. Can you infer the number of observations and variables?

# Inspect the code that build the decision tree, tree. Don't worry if you do not fully understand it yet.
tree <- rpart(Survived ~ ., data = titanic, method = "class")
# Use tree to predict() who survived in the titanic dataset. Use tree as the first argument and titanic as the second argument. Make sure to set the type parameter to "class". Assign the result to pred.
pred<-predict(tree, newdata=titanic, type = "class")
# Build the confusion matrix with the table() function, call it conf and print it. This function builds a contingency table. The first argument corresponds to the rows in the matrix and should be the Survived column of titanic: the true labels from the data. The second argument, corresponding to the columns, should be pred: the tree's predicted labels.
(conf<-table(titanic$Survived,pred))


# Deriving ratios from the Confusion Matrix------------------------
# The confusion matrix from the last exercise provides you with the raw performance of the decision tree:
#   
# The survivors correctly predicted to have survived: true positives (TP)
# The deceased who were wrongly predicted to have survived: false positives (FP)
# The survivors who were wrongly predicted to have perished: false negatives (FN)
# The deceased who were correctly predicted to have perished: true negatives (TN)
# 

# In the slides, you saw that these values can be used to estimate comprehensive ratios to asses the performance of a classification algorithm. An example is the accuracy, which in this case represents the percentage of correctly predicted fates of the passengers.
# 
# Accuracy=(TP+TN) / (TP+FN+FP+TN)

# Apart from accuracy, precision and recall are also key metrics to assess the results of a classification algorithm:
#   
# Precision= TP / (TP+FP)
# Recall= TP / (TP+FN)
# The confusion matrix you've calculated in the previous exercise is available in your workspace as conf.
# 
# Instructions
# Assign the correct values of the confusion matrix to FP and TN. Fill in the ___.
TP <- conf[1, 1] 
FN <- conf[1, 2] 
FP <- conf[2, 1] 
TN <- conf[2, 2] 
# Calculate the accuracy as acc and print it out.
(Accuracy=(TP+TN) / (TP+FN+FP+TN))

# Finally, also calculate the precision and the recall, as prec and rec. Print out both of them.
(Precision= TP / (TP+FP))
(Recall= TP / (TP+FN))

# The quality of a regression------------
# Imagine this: you're working at NASA and your team measured the sound pressure produced by an airplane's wing under different settings. These settings are the frequency of the wind, the angle of the wing, and several more. The results of this experiment are listed in the air dataset (Source: UCIMLR).
air = read_csv("https://assets.datacamp.com/production/course_682/datasets/air.csv")

# Your team wants to build a model that's able to predict the sound pressure based on these settings, instead of having to do those tedious experiments every time.
# 
# A colleague has prepared a multivariable linear regression model, fit. It takes as input the predictors: wind frequency (freq), wing's angle (angle), and chord's length (ch_length). The response is the sound pressure (dec). All these variables can be found in air.
# 
# Now, your job is to assess the quality of your colleague's model by calculating the RMSE (see slides)
#   
#  
# Instructions
# Take a look at the structure of air. What does it tell you?

# Inspect your colleague's code that builds a multivariable linear regression model based on air. 
fit <- lm(dec ~ freq + angle + ch_length, data = air)
# Use the predict() function to make predictions for the observations in the air dataset. Simply pass fit to predict(); R will know what to do. Assign the result to pred.
pred<-predict(fit,newdata=air)
# Use air$dec and pred to calculate the RMSE # Print out rmse. 
rmse=sqrt(mean((pred-air$dec)^2))
print(rmse)
# # Adding complexity to increase quality-------------------------
# In the last exercise, your team's model had 3 predictors (input variables), but what if you included more predictors? You have the measurements on free-stream velocity, velocity and suction side displacement thickness, thickness available for use in the air dataset as well!
# 
# Adding the new variables will definitely increase the complexity of your model, but will it increase the performance? To find out, we'll take the RMSE from the new, more complex model and compare it to that of the original model.
# 
# A colleague took your code from the previous exercise and added code that builds a new extended model, fit2! It's your job to once again assess the performance by calculating the RMSE.
# 
# Instructions
# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the predict() function to make predictions using fit2, for all values in the air dataset. Assign the resulting vector to pred2.
pred2<-predict(fit2,newdata=air)

# Calculate the RMSE, assign this value to rmse2.
# Print rmse2 and compare it with the earlier rmse. What do you conclude?
rmse2=sqrt(mean((pred2-air$dec)^2))
print(rmse2-rmse)


# R-squared
# Remember the formula for calculating R-squared? It is defined as 1 minus the residual sum of squares over the total sum of squares. Time to put this theory into practice!

# Instructions
# First, calculate the residuals of your model, fit2. They can be found within the model object. Store it as res.
res=summary(fit2)$r.squared
# Calculate the residual sum of squares: ss_res
ss_res=sum((rmse2-res)^2)
# Determine the total sum of squares. To compute it, find the difference between the sound pressures (dec) and the average, square those values, and then sum() them. Assign the result to ss_tot
ss_tot=sum((air$dec-mean(air$dec))^2)
# Calculate R-squared and assign it to r_sq. Also print it.
r_sq=1-ss_res/ss_tot

# Apply summary() to air, to compare your manually calculated r_sq and the one from the summary.
summary(air)
# Let's do some clustering!-------------------------
seeds <- read.csv("https://assets.datacamp.com/production/course_682/datasets/seeds.csv")
# In the dataset seeds you can find various metrics such as area, perimeter and compactness for 210 seeds. (Source: UCIMLR). However, the seeds' labels were lost. Hence, we don't know which metrics belong to which type of seed. What we do know, is that there were three types of seeds.
# 
# The code below groups the seeds into three clusters (km_seeds), but is it likely that these three clusters represent our seed types? Let's find out.
# 
# There are two initial steps you could take:
#   
#   Visualize the distribution of cluster assignments among two variables, for example length and compactness.
# Verify if the clusters are well separated and compact. To do this, you can calculate the between and within cluster sum of squares respectively.

# Instructions
# Take a look at the structure of the seeds dataset.

# Group the seeds in three clusters. Use the kmeans() function, with arguments seeds and the number of clusters (3)

# Extend the plot() command by coloring the observations based on their cluster. Do this by setting the col argument equal to the cluster element of km_seeds.
plot(length ~ compactness, data = seeds)

# Print out the ratio of the within sum of squares to the between cluster sum of squares, so WSS/BSS. These measures can be found in the cluster object km_seeds as tot.withinss and betweenss. Is the within sum of squares substantially lower than the between sum of squares?



# By now, you should have a good understanding of the different techniques and their properties. Can you tell which one of the following statements is true? Consult with your neighbours.
# 1) Defining performance metrics for unsupervised learning (cf. clustering) is easy and straightforward.
# 2) You can use the RMSE to determine if a classification was good.
# 3) If you want to build a system that can automatically categorize email as spam or not, a confusion matrix can help you assess its quality.
# 4) The classification model you learned on a dataset shows an error rate of 0.1. You're sure this model will be useful.


# Split the sets-------------------------
# Let's return to the titanic dataset for which we set up a decision tree. In the first exercise you calculated a confusion matrix to assess the tree's performance. However, the tree was built using the entire set of observations. Therefore, the confusion matrix doesn't assess the predictive power of the tree. The training set and the test set were one and the same thing: this can be improved!
# 
# First, you'll want to split the dataset into train and test sets. You'll need to first shuffle the dataset in order to have a fair distribution of the output variable in each set.
# 
# For example, you could use the following commands to shuffle a data frame df and divide it into training and test sets with a 60/40 split between the two.
# 
# n <- nrow(df)
# shuffled_df <- df[sample(n), ]
# train_indices <- 1:round(0.6 * n)
# train <- shuffled_df[train_indices, ]
# test_indices <- (round(0.6 * n) + 1):n
# test <- shuffled_df[test_indices, ]
# Watch out, this is an example of how to do a 60/40 split! In the exercise you have to do a 70/30 split. However, you can use the same commands, just change the numbers!
#   
#   Instructions
# Shuffle the dataset, call the result shuffled


# Split the dataset into a train set, and a test set. Use a 70/30 split. The train set should contain the rows in 1:round(0.7 * n) and the test set in (round(0.7 * n) + 1):n. The example in the exercise description can help you!


#   Print out the structure of both train and test with str(). Does your result make sense?



# First you train, then you test---------------

# Time to redo the model training from before. This time, however, you'll want to build a decision tree on the training set, and next assess its predictive power on a set that has not been used for training: the test set.
# 

# 
# Instructions
# Recreate the model 'tree', but now only on the train part of your titanic dataset


# Use the predict() function with the tree model as the first argument and the correct dataset as the second argument. Set type to "class". Call the predicted vector pred. Check that you should do the predictions on the train or test set.


# Use the table() function to calculate the confusion matrix. Assign this table to conf. Construct the table with the test set's actual values (test$Survived) as the rows and the test set's model predicted values (pred) as columns.
# Finally, print out conf.

# If you're ahead of your peers, you can view this video on on cross validation here: https://www.youtube.com/watch?v=TIgfjmp-4BA (Tip: it may be of use during the business case)


# Classification: Creating an ROC curve ------------
# In this exercise you will work with a medium sized dataset about the income of people given a set of features like education, race, sex, and so on. Each observation is labeled with 1 or 0: 1 means the observation has annual income equal or above $50,000, 0 means the observation has an annual income lower than $50,000 (Source: UCIMLR). This label information is stored in the income variable.

data1=load(url("https://assets.datacamp.com/production/course_682/datasets/income.RData"))
# 

# In previous exercises, you used this tree to make class predictions, by setting the type argument in predict() to "class".
# 
# To build an ROC curve, however, you need the probabilities that the observations are positive. In this case, you'll want to to predict the probability of each observation in the test set (already available) having an annual income equal to or above $50,000. Now, you'll have to set the type argument of predict() to "prob".
# 
# Instructions
# Build a tree on the training set: tree. Use method = 'class'
tree <- rpart(income ~ ., train, method = "class")
# Predict the probabilities of the test set observations using predict(). It takes three arguments:
# The first argument should be the tree model that is built, tree
# The second argument should be the test set, on which you want to predict
# Finally, don't forget to set type to "prob".
# Assign the result to all_probs.
all_probs = predict(tree, test, type = 'prob')


# Print out all_probs. Ask yourself the question; what kind of data structure is it?
all_probs
# Select the second column of all_probs, corresponding to the probabilities of the observations belonging to class 1. Assign to probs.
probs = all_probs[,2]

# Now that you have the probabilities of every observation in the test set belonging to the positive class (annual income equal or above $50,000), you can build the ROC curve.
# 
# You'll use the ROCR package for this. First, you have to build a prediction object with prediction(). Next, you can use performance() with the appropriate arguments to build the actual ROC data and plot it.

# Instructions 
# Load the ROCR package. Don't forget to install it first.
library(ROCR)

# Use prediction() with probs and the true labels of the test set (in the income column of test) to get a prediction object. Assign the result to pred.
pred = prediction(probs, test$income)

# Use performance() on pred to get the ROC curve. The second and third argument of this function should be "tpr" and "fpr". These stand for true positive rate and false positive rate, respectively. Assign to result to perf.
perf = performance(pred, 'tpr', 'fpr')

# Plot perf with plot().


# Next up: the area under the curve
#  
# Again using the ROCR package, you can calculate the AUC. The use of prediction() is identical to before. However, the performance() function needs some tweaking.
# 
# Instructions
# # Use performance() with this prediction object to get the ROC curve. The second argument of this function should be "auc". This stands for area under curve. Assign to perf.
perf = performance(pred, 'auc')

# Print out the AUC. Use google to find where this value can be found in perf. Is your classification model good? Consult with your neighbours!
perf@y.values[[1]]

