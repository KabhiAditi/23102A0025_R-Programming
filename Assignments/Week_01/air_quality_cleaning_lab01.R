version
2 + 2
data <- read.csv(""C:\Users\91976\Desktop\Assignments Semester 7\Programming with R\PRSA_Data_Aotizhongxin_20130301-20170228.csv"")
data <- read.csv("C:\Users\91976\Desktop\Assignments Semester 7\Programming with R\PRSA_Data_Aotizhongxin_20130301-20170228.csv)
library(readr)
PRSA_Data_Aotizhongxin_20130301_20170228 <- read_csv("C:/Users/91976/Downloads/beijing+multi+site+air+quality+data/PRSA2017_Data_20130301-20170228/PRSA_Data_20130301-20170228/PRSA_Data_Aotizhongxin_20130301-20170228.csv")
View(PRSA_Data_Aotizhongxin_20130301_20170228)
head(data)]
head(data)
data <- PRSA_Data_Aotizhongxin_20130301_20170228
head(data)
str(data)
dim(data)
nrow(data)
ncol(data)
any(is.na(data))
sum(is.na(data))
head(data)
str(data)
dim(data)
nrow(data)
ncol(data)
any(is.na(data))
sum(is.na(data))
data[is.na(data$PM2.5), c("No", "PM2.5")]
is.na(data$PM2.5)
sum(is.na(data$PM2.5))
colSums(is.na(data))
sum(is.na(data$PM10))
is.na(data$PM10)
is.na(data$TEMP)
data[is.na(data$PM10), c("No", "PM10")]
missing_object <- data$XYZ
missing_object
is.null(missing_object)
undefined_value <- data$PM2.5[1] / 0
undefined_value
is.nan(undefined_value)
undefined_value2 <- (data$PM2.5[1] - data$PM2.5[1]) / (data$PM2.5[1] - data$PM2.5[1])
undefined_value2
is.nan(undefined_value2)
is.na(undefined_value2)
missing_summary <- function(df) {
vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
total_records <- c()
missing_values <- c()
missing_percentage <- c()
for (v in vars) {
total <- nrow(df)
missing <- sum(is.na(df[[v]]))
percent <- (missing / total) * 100
total_records <- c(total_records, total)
missing_values <- c(missing_values, missing)
missing_percentage <- c(missing_percentage, round(percent, 2))
if (percent > 20) {
warning(paste(v, "contains more than 20% missing values"))
}
}
summary_table <- data.frame(
Variable = vars,
Total_Records = total_records,
Missing_Values = missing_values,
Missing_Percentage = missing_percentage
)
return(summary_table)
}
# Task 3: Missing-Value Summary Function
missing_summary <- function(df) {
# Selected variables
vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
# Create empty vectors
total_records <- c()
missing_values <- c()
missing_percentage <- c()
# Loop through each variable
for (v in vars) {
total <- nrow(df)
missing <- sum(is.na(df[[v]]))
percent <- (missing / total) * 100
total_records <- c(total_records, total)
missing_values <- c(missing_values, missing)
missing_percentage <- c(missing_percentage, round(percent, 2))
# Warning if missing percentage > 20%
if (percent > 20) {
warning(paste(v, "contains more than 20% missing values"))
}
}
# Create summary table
summary_table <- data.frame(
Variable = vars,
Total_Records = total_records,
Missing_Values = missing_values,
Missing_Percentage = missing_percentage
)
return(summary_table)
}
# Apply the function to your dataset
result <- missing_summary(data)
# Display the result
result
# Task 3: Missing-Value Summary Function
missing_summary <- function(df) {
# Selected variables
vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
# Create empty vectors
total_records <- c()
missing_values <- c()
missing_percentage <- c()
# Loop through each variable
for (v in vars) {
total <- nrow(df)
missing <- sum(is.na(df[[v]]))
percent <- (missing / total) * 100
total_records <- c(total_records, total)
missing_values <- c(missing_values, missing)
missing_percentage <- c(missing_percentage, round(percent, 2))
# Warning if missing percentage > 20%
if (percent > 20) {
warning(paste(v, "contains more than 20% missing values"))
}
}
# Create summary table
summary_table <- data.frame(
Variable = vars,
Total_Records = total_records,
Missing_Values = missing_values,
Missing_Percentage = missing_percentage
)
return(summary_table)
}
# Apply the function to your dataset
result <- missing_summary(data)
# Display the result
result
data$pollution_ratio <- data$PM2.5 / data$PM10
head(data$pollution_ratio)
sum(is.na(data$pollution_ratio))
sum(is.nan(data$pollution_ratio))
sum(is.infinite(data$pollution_ratio))
data[is.nan(data$pollution_ratio), c("PM2.5", "PM10", "pollution_ratio")]
data[is.infinite(data$pollution_ratio), c("PM2.5", "PM10", "pollution_ratio")]
data$pollution_ratio[is.nan(data$pollution_ratio)] <- NA
sum(is.nan(data$pollution_ratio))
sum(is.infinite(data$pollution_ratio))
sum(is.na(data$pollution_ratio))
data$pollution_ratio <- data$PM2.5 / data$PM10
head(data$pollution_ratio)
sum(is.na(data$pollution_ratio))
sum(is.nan(data$pollution_ratio))
sum(is.infinite(data$pollution_ratio))
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")
for (var in numeric_variables) {
if (var %in% names(data)) {
missing_before <- sum(is.na(data[[var]]))
median_value <- median(data[[var]], na.rm = TRUE)
data[[var]][is.na(data[[var]])] <- median_value
missing_after <- sum(is.na(data[[var]]))
cat("-----------------------------------\n")
cat("Variable:", var, "\n")
cat("Missing values before treatment:", missing_before, "\n")
cat("Median used for replacement:", median_value, "\n")
cat("Missing values after treatment:", missing_after, "\n")
}
else {
cat("Variable", var, "does not exist in the dataset.\n")
}
}
calculate_mode <- function(x) {
x <- x[!is.na(x)]              # Remove NA values
unique_values <- unique(x)     # Get unique categories
unique_values[which.max(tabulate(match(x, unique_values)))]
}
missing_before <- sum(is.na(data$wd))
mode_wd <- calculate_mode(data$wd)
data$wd[is.na(data$wd)] <- mode_wd
missing_after <- sum(is.na(data$wd))
cat("Mode of wd:", mode_wd, "\n")
cat("Missing values before replacement:", missing_before, "\n")
cat("Missing values after replacement:", missing_after, "\n")
clean_variable <- function(df, var_name) {
tryCatch({
if (!(var_name %in% names(df))) {
stop(paste("Error: Variable '", var_name, "' does not exist in the dataset.", sep=""))
}
variable <- df[[var_name]]
if (!is.numeric(variable)) {
stop(paste("Error: Variable '", var_name, "' is not numerical.", sep=""))
}
if (all(is.na(variable))) {
stop(paste("Error: Variable '", var_name, "' contains only missing values.", sep=""))
}
median_value <- median(variable, na.rm = TRUE)
if (is.na(median_value) || is.nan(median_value)) {
stop(paste("Error: Median cannot be calculated for '", var_name, "'.", sep=""))
}
variable[is.na(variable)] <- median_value
cat("Variable:", var_name, "cleaned successfully.\n")
cat("Median used:", median_value, "\n")
cat("Remaining missing values:", sum(is.na(variable)), "\n\n")
return(variable)
}, error = function(e) {
cat(e$message, "\n\n")
return(NULL)
})
}
clean_pm25 <- clean_variable(data, "PM2.5")
clean_variable(data, "XYZ")
clean_variable(data, "wd")
data$AllMissing <- NA
clean_variable(data, "AllMissing")
original_data <- read.csv(file.choose())
variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
missing_before <- sapply(variables, function(v) sum(is.na(original_data[[v]])))
missing_after <- sapply(variables, function(v) sum(is.na(data[[v]])))
values_replaced <- missing_before - missing_after
comparison_table <- data.frame(
Variable = variables,
Missing_Before = missing_before,
Missing_After = missing_after,
Values_Replaced = values_replaced
)
comparison_table
missing_matrix <- rbind(comparison_table$Missing_Before,
comparison_table$Missing_After)
barplot(
missing_matrix,
beside = TRUE,
names.arg = comparison_table$Variable,
col = c("red", "green"),
main = "Missing Values Before and After Data Cleaning",
xlab = "Variables",
ylab = "Number of Missing Values",
legend.text = c("Before Cleaning", "After Cleaning"),
args.legend = list(x = "topright")
)
write.csv(data,
file = "cleaned_air_quality_data.csv",
row.names = FALSE)
cat("Cleaned dataset exported successfully as 'cleaned_air_quality_data.csv'\n")
getwd()
colSums(is.na(original_data))
colSums(is.na(data))
sum(is.na(original_data))
sum(is.na(data))
original_data[205, c("PM10")]
data[205, c("PM10")]
summary(original_data$PM10)
summary(data$PM10)
