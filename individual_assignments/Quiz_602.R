#ECo 634 fall 2020


# WEEK 1 QUIZ -------------------------------------------------------------

# ---- Question 1 ----
# Run the following two lines of code in the console and explain the differences in output:

c(1, 2, 3)
"c(1, 2, 3)"

#c(1, 2, 3) is a vector of whole numbers
#"c(1, 2, 3)" is a string of the character input

# ---- Question 2 ----
# Run the following two lines of code in the console and consider the differences:

c_1 = c(1, 2, 3)
c_2 = "c(1, 2, 3)"

# Q1. Is c_1 a variable, or a function?
# Q2. Is c_2 a variable, or a function?
# Q3. If c_1 and c_2 have different values, why?

#c_1 is a variable
#c_2 is a variable
#they are different because c_1 is assigned a numeric vector whereas c_2 is assigned a character string

# ---- Question 3: Matrices 1 ----
# Create a numeric vector of length 3 called my_vec. It should contain the integers from 1 to 3.
my_vec = 1:3
my_vec
  
# Build a matrix using the following code:
mat_1 = matrix(my_vec)
mat_1

# Q1 (1pt.): What are the dimensions of the matrix (i.e. how many rows and columns)?
# Q2 (2pts.): Write R code to retrieve the element of mat_1 that has a value of 3.

#3 rows, 1 column
mat_1[3]

# ---- Question 4: Matrices 2 ----
# You will use my_vec from the previous question again.
# Create a matrix mat_2 that has two rows and three columns using my_vec. Do not use the c() or rep() functions.
mat_2 = matrix(my_vec, nrow = 2, ncol = 3)
mat_2

# Create a matrix mat_3 that has three rows and two columns using my_vec. Do not use the c() or rep() functions.
mat_3 = matrix(my_vec, nrow = 3, ncol = 2)
mat_3

# Q1 (1pt.): Paste the code you used to create mat_2.
# Q2 (1pt.): Paste the code you used to create mat_3.
# Q3 (1pt.): Did R use rows or columns to recycle the values in my_vec?
#columns
# Q4 (1pt.): Create a matrix, mat_4, with a number of elements that is not a multiple of 3 and paste the code into the editor.
mat_4 = matrix(my_vec, nrow = 8, ncol = 4)
mat_4
# Q5 (1pt.): How did R handle the recycling of values of my_vec in mat_4?
#columns

# ---- Question 5: List subsetting challenge question ----
# Create a list, named my_list_1 with following three elements:
# first element is numeric: 5.2
# second element is a string "five point two"
# third element is a vector of all integers from 0 to 5. Do recall how to do this from the DataCamp course?

my_list_1 = list("two" = 5.2, "1" = "five point two", "three" = 0:5)
my_list_1

# Name the elements in my_list_1:
# "two"
# "one"
# "three"
# Run the following lines of code.

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"

# Q1 (8 pts): for each subsetting operation, explain why R produced the corresponding output.
# Identify the type of subsetting operation.
# Explain how the operation chose which element (1, 2, or 3) to return.

# Q2 (2 pts): Hypothesize why some of the lines may have produced errors or NULL outputs.
# Q3 (2 pts): Identify which lines produced the same output and explain why.

my_list_1[[1]] 
#Q1: indexing a list by position; looks for the first element of the list

my_list_1[[as.numeric("1")]] 
#Q1: indexing a list by position; looks for the first element of the list
#Q3: returns the first element in the list, same as my_list_1[[1]] because the character string "1" is converted to a numeric value 1 which tells R to look for the first element of the list

my_list_1[["1"]] 
#Q1: indexing a list (attempt); cannot return element
#Q2 no returned element/NULL as the character string "1" has no value or order so R doesn't know where to look

my_list_1[["one"]]
#Q1: indexing a list by element name; looks for an element of the list with the name one

my_list_1$one 
#Q1: indexing a list by element name; looks for an element of the list with the name one
#Q3: returns the same element as my_list_1[["one"]] because they are referencing the same element name

my_list_1$"one"
#Q1: indexing a list by element name; looks for an element of the list with the name one
#Q3: returns the same element as my_list_1[["one"]] and my_list_1$one because they are referencing the same element name one and in this case, the name is a character string so putting it in quotes does not change the ability of R to look for a matching character name

my_list_1$1
#Q1: indexing a list by element name (attempt); cannot return element
#Q2: element name must be character string, in this case 1 is a numeric and there is no named element 1 in the list

my_list_1$"1"
#Q1: indexing a list by element name (attempt); cannot return element
#Q2: there is no element name "1" in the list


#WEEK 1 class activity

#load data stored in R
data("iris")
#view metadata
?iris
#view column headers and 1st few rows
head(iris)
#view vector
iris$Sepal.Width
#calculate mean
mean(iris$Sepal.Length)
#calculate SD
sd(iris$Sepal.Width)
#plot
plot(x=iris$Sepal.Width,y=iris$Sepal.Length)
#assign mean
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)
#plot
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
#add mean point
points(x = data_center_x, y = data_center_y, col = "red")
#add fitted line
lines(iris$Sepal.Width, fitted(fit), col ="blue")
#connect the dots
#lines(iris$Sepal.Width, iris$Sepal.Length)
#clear plot area
dev.off()
#Mike built function for fitted line
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    6), 
  add = TRUE)





#WEEK 2 Lab

#Create a variable to store the randomly-generated number.
int_rnd = sample(100, 1)
#Create text of a sentence that stated the value of the number.
int_rnd_sentence = paste0("The value of the randomly-generated number is: ", int_rnd)
#print sentence
print(int_rnd_sentence)

#or nest
print(
  paste0(
    "The value of the randomly-generated number is: ", 
    sample(100, 1))
)

#example for-loop
#execute the loop 10 times, using an index variable called i
for(i in 1:10){print(i)}

#example function
#R expects the arguments to be supplied in the order specified by the function definition.
#Unless you can input the arguments in any order if you specify their names
print_number = function(n)
{
  print(paste0("The value of the number is ", n))
}
print_number(145)

#The normal distributions
?rnorm
#random number generator
rnorm(10)
rnorm(n = 10, sd = 1)
rnorm(sd = 1, mean = 0, n = 10)
#these are the same calls

#starting point
#can set same starting point for random number generator, example with 5
set.seed(5)


# WEEK 2 QUIZ -------------------------------------------------------------

#generate a large vector
n = 12345
vec_1 = sample(12, n, replace = TRUE)
head(vec_1)

#Use a logical test operator to create a Boolean vector (called vec_2) whose entries are TRUE if the corresponding entry in vec_1 is 3 and FALSE otherwise
vec_2 = vec_1==3

#Self test: you can use vec_2 to retrieve all of the 3 elements of vec_1 using the following to generate a vector of all 3's
vec_1[vec_2]

#Q1: vec_2 = vec_1==3

#Q2: you have a vector with 12345 units and to select the units that were=3, you would have to look at each of those units' positions and record it in the call to index


#determine how many elements are in vec_1
length(vec_1)

#check how many entries have the value 3, logic test
sum(vec_1 == 3)

n = 10
vec_1 = sample(12, n, replace = TRUE)
paste0("Sum of elements with value 3: ", sum(vec_1 == 3))

#Q1: By randomly sampling the large vector, different quantities of the value 3 are selected every time the sample is taken, changing the result value. A logic test can read the data faster and generate a result more reliably than a person can visually inspecting the data each time the sample is taken.

#Q2: Manual calculations or inspections of data do not take advantage of the capabilities of R--including accuracy and time to compute. R is more reliable at count data and faster than a person. If you were sharing code with a collaborator, it would not be desirable to require collaborators to take on manual calculations in the middle of your code. Similarly, if you were re-running your code, you could have issues where scripting does not continuously build upon the code before it--if you had to manually enter a result, especially a changing result, that you visually collected it will stop you from being able to run the code as one complete block. Additionally, visual inspection cannot be scaled up to even larger data sets and would be further time-consuming and unreliable.

for(i in 1:10){print(paste("This is loop iteration:", i))}

n=25
for (i in 1:n){print(i)}

#create a vector called vec_1 of length n. vec_1 should contain [pseudo]randomly generated integers between 1 and 10.
n=17
vec_1 = sample(10, n, replace = TRUE)
vec_1
for(i in 1:n){
  print(vec_1[i])
}

for(i in 1:n){
  print(paste("The element of vec_1 at index", i, 
              "is", vec_1[i]))
}

n  
min = 1
max = 10

for(i in 1:max){
  print(sample(max, min))
}

create_and_print_vec = 
  function(n, min = 1, max = 10){
    for(i in 1:max){
      print(paste("The element at index", i, 
                  "is", sample(max,min)))}}
create_and_print_vec()