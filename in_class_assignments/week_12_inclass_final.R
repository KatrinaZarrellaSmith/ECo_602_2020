# Week 12 In-class Final

require(here)
dat_delomys = read.csv(here("data", "delomys.csv"))

#explore continuous vs categorical data
head(dat_delomys)
class(dat_delomys$X) #integer/categorical

#Plot a histogram of body mass
hist(dat_delomys$body_mass)

#Plot a histogram of body length
hist(dat_delomys$body_length)

#what species
unique(dat_delomys$binomial)

#Plot a scatterplot of body length (on the y-axis) and body mass (on the x-axis)
plot(dat_delomys$body_mass, 
     dat_delomys$body_length,
     xlab = "Body Mass (g)",
     ylab = "Body Length (mm)",
     main = "Scatterplot of Body Mass and Length of Delomys spp.")

#Conditional boxplot of body mass grouped by sex
plot(dat_delomys$sex,
     dat_delomys$body_mass, 
     xlab = "Sex",
     ylab = "Body Mass (g)",
     main = "Body Mass by Sex of Delomys spp.")

#Conditional boxplot of body mass grouped by species
plot(dat_delomys$binomial,
     dat_delomys$body_mass, 
     xlab = "Species",
     ylab = "Body Mass (g)",
     main = "Body Mass by Species of Delomys spp.")

#Conditional boxplot of body mass grouped by species and sex
boxplot(body_mass ~ binomial * sex, 
     data = dat_delomys,
     names = c("Female D. dorsalis", "Male D. dorsalis",
               "Female D. sublineatus", "Male D. sublineatus"),
     xlab = "Species",
     ylab = "Body Mass (g)",
     main = "Boxplot of Body Mass by Species and Sex",
     col = "grey")
