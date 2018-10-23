# Vector: logical, integer, numeric (double), and character.

# Use TRUE and FALSE (or T and F) to create logical vectors
log_var <- c(TRUE, FALSE, T, F)
int_var <- c(1L, 6L, 10L)
dbl_var <- c(1, 2.5, 4.5)
# With the L suffix, you get an integer rather than a double
chr_var <- c("these are", "some strings")

# Vectors are always flat, even if you nest c()
c(1, c(2, c(3, 4)))
c(1, 2, 3, 4)

# missing value is NA
t <- c(1, NA, 2)
is.na(t)

# Given a vector, you can determine its type with typeof(), 
# or check if it’s a specific type with an “is” function: 
# is.logical(), is.integer(), is.double(), is.character().
# or, more generally, is.atomic().
length(int_var)
typeof(int_var)
is.integer(int_var)
is.atomic(int_var)

# is.numeric() is a general test for the “numberliness” of a vector 
# and returns TRUE for both integer and double vectors. 
is.numeric(int_var)
is.numeric(dbl_var)

# Coercion: 
# All elements of a vector must be the same type, 
# so when you attempt to combine different types,
# they will be coerced to the most flexible type. 
# Types from least to most flexible are: logical, integer, double, and character.

str(c("a", 1))

x <- c(FALSE, FALSE, TRUE)
as.numeric(x)

# List
# Lists are different from vectors, called recursive list
# because their elements can be of any type and different length, so list can contain other lists.

x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)

# The typeof() a list is list. 
# You can test for a list with is.list() and coerce to a list with as.list(). 
typeof(x)
is.list(x)
as.list(dbl_var)

x[1] # returns the 1st element of the list in a list form
str(x[1])
x[[1]] # returns the 1st elememt of the list but not in a list form
str(x[[1]])
# You can turn a list into a vector with unlist(). 
# If the elements of a list have different types, unlist() uses the same coercion rules as c().

unlist(x)

# Lists are used to build up many of the more complicated data structures in R
# e.g., linear models objects (as produced by lm()) are lists.

# Matrix
a <- matrix(1:6, ncol = 3, nrow = 2)

# You can also modify an object in place by setting dim()
c <- 1:6
dim(c) <- c(2, 3)

# length() generalises to nrow() and ncol() for matrices.
# names() generalises to rownames() and colnames() for matrices, and dimnames(), a list of character vectors, for arrays.

length(a)
nrow(a)
ncol(a)

rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
a

# Data frame: the most common way of storing data in R
df <- data.frame(x = 1:3, y = c("a", "b", "c"))
str(df)
# why we don't like factor, see this post:http://www.win-vector.com/blog/2014/09/factors-are-not-first-class-citizens-in-r/
# f = as.factor(c('a', 'b', 'c'))
# f = c(f, 'd')
df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
str(df)

is.data.frame(df)

# Coercion
# You can coerce an object to a data frame with as.data.frame():
# A vector will create a one-column data frame.
# A list will create one column for each element; 
# it’s an error if they’re not all the same length. 
x
as.data.frame(x)
# A matrix will create a data frame with the same number of columns and rows as the matrix.
a
as.data.frame(a)

