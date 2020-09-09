#WEEK 2 In Class Acitivity
#examples
pairs(iris)
pairs(iris[, c("Petal.Width", "Sepal.Width", "Sepal.Length")])

#data downloaded from course link to R working files
#set working directory
setwd("~/GitHub/ECo_602_2020/in_class_assignments")

#load habitat data
hab.sta<-read.csv("hab.sta.csv")
#load bird species data
bird.sta<-read.csv("bird.sta.csv")

#dat_birds <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
#dat_habitat <- read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")


#highest value in vector, determine number of bins for x-axis
max(bird.sta$CBCH) #6
#plot bird abundance with histogram, course example
hist(bird.sta$CBCH, #column to be counted
     xlab = "Number of birds counted", #x-axis label
     breaks = 0:7 - 0.5, #7 columns/columns at 0-6, breaks every 0.5 shifts column labels to center of bar
     main = "Histogram of Chestnut Beaked Chickadee Abundance") #plot title

#max count
max(bird.sta$OCWA) #4
#plot species OCWA abundance with histogram
hist(bird.sta$OCWA, #column to be counted
     xlab = "Number of birds counted", #x-axis label
     breaks = 0:5 - 0.5, #5 columns/columns at 0-4, breaks every 0.5 shifts column labels to center of bar
     main = "Histogram of Bird Abundance") #plot title

#PAIR PLOT
#pairs(mydata[, c("a","b","c")])
#In the first line you see a scatter plot of a and b, then one of a and c
#In the second row b and a (symmetric to the first), b and c, and so on

#look for numeric variables
lapply(hab.sta, class)
#pair plot to explore habitat data relationships between differnt size snags
pairs(hab.sta[, c("snag.sml","snag.ml","snag.l")]) #calls on 3 types of habitat

