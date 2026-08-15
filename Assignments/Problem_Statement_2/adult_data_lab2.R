adult_data <- read.csv("C:/Users/91976/Desktop/Assignments Semester 7/Programming with R/adult_train.csv")
View(adult_data)
install.packages(c("dplyr", "naniar", "skimr"))
library(dplyr)
library(naniar)
library(skimr)
head(adult_data)
str(adult_data)
adult_dirty <- adult_data
names(adult_dirty)
set.seed(123)
adult_dirty$age[c(5, 10)] <- NA
adult_dirty$Age[c(5, 10)] <- NA
adult_dirty$Age[c(15, 20)] <- 999
adult_dirty$workclass[c(3, 8)] <- ""
adult_dirty$Workclass[c(3, 8)] <- ""
adult_dirty$Hours_per_week[2] <- NaN
adult_dirty[1:20, c("Age", "Workclass", "Hours_per_week")]
is.na(adult_dirty$Age)
sum(is.na(adult_dirty$Age))
is.nan(adult_dirty$Hours_per_week)
sum(is.nan(adult_dirty$Hours_per_week))
x <- NULL
is.null(x)
is.null(adult_dirty$Age)
adult_dirty$Workclass == ""
sum(adult_dirty$Workclass == "")
adult_dirty$Age == 999
sum(adult_dirty$Age == 999, na.rm = TRUE)
miss_var_summary(adult_dirty)
adult_dirty$Age[adult_dirty$Age == 999] <- NA
adult_dirty$Workclass[
  adult_dirty$Workclass == ""
] <- "Unknown"
adult_dirty <- adult_dirty[
  !is.nan(adult_dirty$Hours_per_week),
]
adult_dirty <- adult_dirty[
  !is.nan(adult_dirty$Hours_per_week),
]
median_impute <- function(x) {
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  return(x)
}
adult_dirty$age <- median_impute(adult_dirty$Age)
adult_dirty$hours.per.week <- median_impute(
  adult_dirty$Hours_per_week
)
sum(is.na(adult_dirty$Age))
sum(is.na(adult_dirty$Hours_per_week))
complete_rows <- complete.cases(adult_dirty)
table(complete_rows)
adult_dirty[!complete_rows, ]
sum(is.na(adult_data))
sum(is.na(adult_dirty))
sum(is.na(adult_data)) /
  prod(dim(adult_data)) * 100
sum(is.na(adult_dirty)) /
  prod(dim(adult_dirty)) * 100
vis_miss(adult_data)
vis_miss(adult_dirty)
skim(adult_dirty)
any(adult_dirty$age == 999)
sum(is.na(adult_dirty$Age))
sum(is.na(adult_dirty$Hours_per_week))
any(adult_dirty$workclass == "")
# Custom median imputation function
median_impute <- function(x) {
  med <- median(x, na.rm = TRUE)
  x[is.na(x)] <- med
  return(x)
}
# Apply the function to Age
adult_dirty$Age <- median_impute(adult_dirty$Age)
# Check again
sum(is.na(adult_dirty$Age))
write.csv(
  adult_dirty,
  "cleaned_adult_data.csv",
  row.names = FALSE
)