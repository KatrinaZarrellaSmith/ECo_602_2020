#add data
install.packages("palmerpenguins")
install.packages("here")

#load to the environment
require(palmerpenguins)
require(here)

#what kind of object is data?
class(penguins) #table

#convert to data frame
penguins = data.frame(penguins)

#mean bill length
mean(penguins$bill_length_mm) #doesn't run with NA in data

#preview data
head(penguins)

#mean bill length with no NA
mean(penguins$bill_length_mm, na.rm = TRUE)

#summary stats for each column
summary(penguins)

#pair plot
pairs (penguins[,3:6], pch = 19, cex = 0.5,
       col = penguins$species,lower.panel = NULL)

### Correlation panel
###panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs (penguins[,3:6], pch = 19, cex = 0.5,col = penguins$species,lower.panel = panel.cor)

#scatterplot
plot(x = penguins$body_mass_g, 
     y = penguins$flipper_length_mm,
     bg = colors(penguins$species),
     cex = 3,
     pch=21)

#histogram
hist(penguins$body_mass_g)

#boxplot
plot(x = penguins$species,
     y = penguins$body_mass_g,
     ylab = "Body Mass (g)",
     xlab = "Species",
     main = "Body Mass by Penguin Species")

#same
boxplot(penguins$body_mass_g ~ penguins$species,
     ylab = "Body Mass (g)",
     xlab = "Species",
     main = "Body Mass by Penguin Species",
     medcol="red")
