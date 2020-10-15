#Use "=" to assign a variable name to mirror other computer languages

#naming a vector
names(mydata)<- c("","","")

#naming a matrix
rownames(my_matrix) <- row_names_vector
colnames(my_matrix) <- col_names_vector

#calculate the totals for each row of a matrix
rowSums(my_matrix)

#contents of workspace
ls()

#matrix selection
my_matrix[1,2] #selects the element at the first row and second column.
my_matrix[1:3,2:4] #results in a matrix with the data on the rows 1, 2, 3 and columns 2, 3, 4.
my_matrix[,1] #selects all elements of the first column.
my_matrix[1,] #selects all elements of the first row.

#Comparing data without visual inspection
#Assign a variable name based on a condition
subset_mydata <- value>0
result <- my_data[subset_mydata]
#use math to review data 
mean(result)

#factor ordering 
factor_temperature_vector <- factor(temperature_vector, order = TRUE, 
                                    levels = c("Low", "Medium", "High"))

#renaming factors
levels(factor_survey_vector) <-c("Female","Male")
#Watch out: the order with which you assign the levels is important. 
#If you type levels(factor_survey_vector), you'll see that it outputs [1] "F" "M". 
#If you don't specify the levels of the factor when creating the vector, R will automatically assign them alphabetically. To correctly map "F" to "Female" and "M" to "Male", the levels should be set to c("Female", "Male"), in this order.

#indexing dataframes
# The planets_df data frame from the previous exercise is pre-loaded
# Select first 5 values of diameter column
planets_df[1:5,"diameter"]

#If you type rings_vector in the console, you get:
#  [1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
# select all columns for planets with rings
planets_df[rings_vector,]
#subset
subset(mydata, subset=condition statement ex<1)

#order function
a<-c(3,1,2)
order(a)
a[order(a)]

# Use order() to create positions
positions <-  order(mydata$diameter)

# Use positions to sort planets_df
mydata[positions,]

#assign a value to workspace
df=data.frame(
  x=1,
  y=1)#does not create workspace variable

df=data.frame(
  x<-1,
  y<-1)#does create workspace variable



#DATACAMP Quiz
#assign a character to a variable
a = "Katrina Zarrella Smith"
#class of variable
class(a)
#assign numeric to variable
b1 = 45.6
#class of variable
class(b1)
#assign character to variable
b2 = "45.6"
#class of variable
class(b2)
#calculate
b1 + b2
#assign vector to variable
c = 0:3
#class of element from vector
class(c)
#calculate
r = b1 + c
#class of calculated element from vector
class(r)
#assign vector to variable
v1 = -2:2
#multiply the vector by 3
v2 = v1*3
#add the elements of vector
sum(v2)

#create a list
my_list_1 = list("two" = 5.2, "one" = "five point two", "three" = 0:5)
#index the third element
my_list_1[[3]]
#index the element with name "one"
my_list_1$one

#create vectot of integers 1 through 3, repeated 5 times
my_vec = rep(1:3, 5)
#print vector
my_vec #[1] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3
#total number of elements in vector
length(my_vec)

#create vector of just 3's from my_vec
my_bool_vec = my_vec==3
#total number of elements in vector
length(my_bool_vec)
#create a data frame of two vectors
data.frame(my_vec, my_bool_vec)
#self-check for just 3's
#my_vec[my_vec==3] same as
my_vec[my_bool_vec]