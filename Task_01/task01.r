f# 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
# install.packages("swirl")
library(swirl)
Anie
1
5+7
x <- 5+7
x
y <- x-3
y
z <- c(1.1, 9, 3.14)
?c
z
c(z, 555, z)
z * 2 + 100
my_sqrt <- sqrt(z-1)
my_div <- z/my_sqrt
2
3
c(1, 2, 3, 4)+c(0, 10, 100)
1000
my
bye()
No
swirl()
getwd()
ls()
x <- 9
dir()
?list.files
args(list.files)
old.dir <- getwd()
dir.create("testdir")
skip()
file.create("mytest.R")
dir()
file.exists("mytest.R")
file.info("mytest.R")
?file.rename
file.rename("mytest.R", "mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
skip()
?dir.create
skip()
setwd(old.dir)
1:20
pi:10
15:1
?':'
seq(1,20)
?seq
seq(0, 10, by=0.5)
my_seq <- seq(5, 10, length=30)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0, times = 40)
rep(c(0, 1, 2), each = 10)
4
num_vect <- c(0.5, 55, -10, 6)
tf <- num_vect < 1
1
tf
num_vect >= 6
(3 > 5) & (4 == 4)
1
my_char <- c("My", "name", "is")
my_char
paste(my_char, collapse = " ")
my_name <- c(my_char, "Angela Van Horn")
my_name
paste(my_name, collapse = " ")
paste("Hello", "world!", sep = " ")
paste(1:3, c("X", "Y", "Z"), sep = "")
LETTERS
paste(LETTERS, 1:4, sep = "-")
5
x <- c(44, NA,5, NA)
x * 3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y, z), 100)
my_na <- is.na(my_data)
my_na
my_data == NA
sum(my_na)
my_data
0/0
2/0
Inf - Inf
6
x
x[1:10]
3
x[is.na(x)]
y <- x[!is.na(x)]
y
5
y[y > 0]
x[x > 0]
x[!is.na(x) & x > 0]
x[c(3, 5, 7)]
x[3000]
x[c(-2, -10)]
x[-c(2, 10)]
vect <- c(foo = 11, bar = 2, norf = NA)
vect
names(vect)
vect2 <- c(11, 2, NA)
names(vect2) <- c("foo", "bar", "norf")
?identical 
identical(vect, vect2)
2
vect["bar"]
vect[c("foo", "bar")]
7
my_vector <- 1:20 
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4, 5)
dim(my_vector)
my_vector
attributes(my_vector)
my_vector
class(my_vector)
my_matrix <- my_vector
?matrix
identical(my_matrix, my_matrix2)
skip()
my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
2
1
8
TRUE == TRUE
(FALSE == TRUE) == FALSE
6 == 7
"sam" == "fool"
6 < 7
10 <= 10
4
1
5 != 7
!(5 == 7)
3
4
TRUE & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5 > 8 || 6 != 8 && 4 > 3.9
2
3
isTRUE(FALSE || TRUE && 6 !=4 || 9 > 4)
isTRUE(6 > 4)
4
identical('twins', 'twins')
2
xor(5 == 6, !FALSE)
4
ints <- sample(10)
ints > 5
which(ints > 7)
3
any(ints < 7)
any(ints < 0)
all(ints > 0)
2
1
9
Sys.Date()
10
head(flags)
dim(flags)
class(flags)
cls_list <- lapply(flags, class)
cls_list
class(cls_list)
as.character(cls_list)
?sapply
cls_list <- sapply(flags,class)
cls_vect <- sapply(flags, class)
class(cls_vect)
skip()
flag_colors <- flags[, 11:17]
skip()
head(flag_colors)
lapply(flag_colors, sum)
skip()
sapply(flag_colors, mean)
flag_shapes <- flags[, 19:23]
skip()
shape_mat <- sapply(flag_shapes, range)
shape_mat
class(shape_mat)
unique(c(3, 4, 5, 5, 5, 6, 6))
unique_vals <- lapply(flags, unique)
unique_vals
skip()
sapply(flags, unique)
lapply(unique_vals, function(elem) elem[2])
2