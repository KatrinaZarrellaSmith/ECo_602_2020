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