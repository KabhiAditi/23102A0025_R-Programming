install.packages(c(
  "data.table",
  "ggplot2",
  "foreach",
  "doParallel",
  "microbenchmark",
  "purrr",
  "lubridate",
  "arrow"
))

library(data.table)
library(ggplot2)
library(foreach)
library(doParallel)
library(microbenchmark)
library(purrr)
library(lubridate)
library(arrow)


# Import taxi trip data
taxi <- as.data.table(
  arrow::read_parquet(
    "C:\\Users\\91976\\Downloads\\yellow_tripdata_2026-01.parquet"
  )
)

# Import zone lookup data
zones <- fread("C:\\Users\\91976\\Downloads\\taxi_zone_lookup.csv")

# Inspect dataset
head(taxi)
str(taxi)
dim(taxi)
names(taxi)
summary(taxi)

# Check lookup table
head(zones)
str(zones)

# Dataset size
format(object.size(taxi), units = "MB")


# Identify required columns
required_cols <- c(
  "tpep_pickup_datetime",
  "tpep_dropoff_datetime",
  "PULocationID",
  "DOLocationID",
  "passenger_count",
  "trip_distance",
  "payment_type",
  "fare_amount",
  "total_amount"
)

missing_cols <- setdiff(required_cols, names(taxi))

if (length(missing_cols) > 0) {
  stop(
    "Required columns missing: ",
    paste(missing_cols, collapse = ", ")
  )
}

# Missing values in each column
missing_counts <- taxi[
  , lapply(.SD, function(x) sum(is.na(x)))
]

print(missing_counts)

# Remove exact duplicate rows
taxi <- unique(taxi)

# Convert timestamps to POSIXct if needed
taxi[, tpep_pickup_datetime :=
       as.POSIXct(tpep_pickup_datetime, tz = "UTC")]

taxi[, tpep_dropoff_datetime :=
       as.POSIXct(tpep_dropoff_datetime, tz = "UTC")]

# Keep records with valid timestamps and analytical values
taxi <- taxi[
  !is.na(tpep_pickup_datetime) &
    !is.na(tpep_dropoff_datetime) &
    !is.na(PULocationID) &
    !is.na(DOLocationID) &
    !is.na(trip_distance) &
    !is.na(fare_amount) &
    !is.na(total_amount) &
    trip_distance > 0 &
    fare_amount >= 0 &
    total_amount >= 0 &
    tpep_dropoff_datetime >= tpep_pickup_datetime
]

# Check cleaned dataset
dim(taxi)
summary(taxi)


# Extract temporal attributes
taxi[, pickup_hour :=
       hour(tpep_pickup_datetime)]

taxi[, pickup_day :=
       day(tpep_pickup_datetime)]

taxi[, pickup_weekday :=
       wday(
         tpep_pickup_datetime,
         label = TRUE,
         abbr = FALSE,
         week_start = 1
       )]

taxi[, pickup_month :=
       month(
         tpep_pickup_datetime,
         label = TRUE,
         abbr = FALSE
       )]

taxi[, pickup_month_num :=
       month(tpep_pickup_datetime)]

# Calculate trip duration in minutes
taxi[, trip_duration_min :=
       as.numeric(
         difftime(
           tpep_dropoff_datetime,
           tpep_pickup_datetime,
           units = "mins"
         )
       )]

library(lubridate)

taxi[, trip_duration_min := as.numeric(difftime(tpep_dropoff_datetime, tpep_pickup_datetime, units = "mins"))]


# Inspect the new attributes
head(
  taxi[, .(
    tpep_pickup_datetime,
    pickup_hour,
    pickup_day,
    pickup_weekday,
    pickup_month,
    trip_duration_min
  )]
)


# Inspect lookup columns
names(zones)

# Standardize column names for analysis
setnames(
  zones,
  old = c("LocationID", "Borough", "Zone"),
  new = c("LocationID", "Borough", "Zone"),
  skip_absent = TRUE
)

# Keep only required lookup attributes
zones <- unique(
  zones[, .(LocationID, Borough, Zone)]
)

# Build separate pickup and drop-off lookup tables
pickup_zones <- copy(zones)
setnames(
  pickup_zones,
  c("LocationID", "Borough", "Zone"),
  c("PULocationID", "pickup_borough", "pickup_zone")
)

dropoff_zones <- copy(zones)
setnames(
  dropoff_zones,
  c("LocationID", "Borough", "Zone"),
  c("DOLocationID", "dropoff_borough", "dropoff_zone")
)

# Efficient joins
taxi <- pickup_zones[
  taxi,
  on = "PULocationID"
]

taxi <- dropoff_zones[
  taxi,
  on = "DOLocationID"
]

# Check joined records
head(
  taxi[, .(
    pickup_zone,
    pickup_borough,
    dropoff_zone,
    dropoff_borough
  )]
)

hourly_demand <- taxi[
  , .(total_trips = .N),
  by = pickup_hour
][order(pickup_hour)]

print(hourly_demand)

weekday_demand <- taxi[
  , .(total_trips = .N),
  by = pickup_weekday
][order(pickup_weekday)]

print(weekday_demand)


monthly_demand <- taxi[
  , .(total_trips = .N),
  by = .(pickup_month_num, pickup_month)
][order(pickup_month_num)]

print(monthly_demand)


fare_analysis <- taxi[
  , .(
    total_trips = .N,
    average_fare = mean(fare_amount, na.rm = TRUE),
    total_revenue = sum(total_amount, na.rm = TRUE),
    average_trip_distance = mean(
      trip_distance,
      na.rm = TRUE
    )
  ),
  by = pickup_hour
][order(pickup_hour)]

print(fare_analysis)


top_routes <- taxi[
  !is.na(pickup_zone) & !is.na(dropoff_zone),
  .(
    total_trips = .N
  ),
  by = .(
    pickup_zone,
    dropoff_zone
  )
][order(-total_trips)]

print(head(top_routes, 10))


top_revenue_routes <- taxi[
  !is.na(pickup_zone) & !is.na(dropoff_zone),
  .(
    total_trips = .N,
    total_revenue = sum(total_amount, na.rm = TRUE),
    average_revenue_per_trip = mean(
      total_amount,
      na.rm = TRUE
    )
  ),
  by = .(
    pickup_zone,
    dropoff_zone
  )
][order(-total_revenue)]

print(head(top_revenue_routes, 10))


distance_fare <- taxi[
  trip_distance > 0 &
    fare_amount >= 0,
  .(
    average_fare = mean(fare_amount, na.rm = TRUE),
    total_trips = .N
  ),
  by = .(
    distance_group = cut(
      trip_distance,
      breaks = c(0, 1, 3, 5, 10, 20, Inf),
      labels = c(
        "0-1 miles",
        "1-3 miles",
        "3-5 miles",
        "5-10 miles",
        "10-20 miles",
        "20+ miles"
      ),
      include.lowest = TRUE
    )
  )
]

print(distance_fare)

# Correlation between distance and fare
cor(
  taxi$trip_distance,
  taxi$fare_amount,
  use = "complete.obs"
)

payment_analysis <- taxi[
  , .(
    total_trips = .N,
    total_revenue = sum(total_amount, na.rm = TRUE)
  ),
  by = payment_type
][order(-total_trips)]

print(payment_analysis)


borough_demand <- taxi[
  !is.na(pickup_borough),
  .(
    total_trips = .N,
    average_fare = mean(fare_amount, na.rm = TRUE),
    total_revenue = sum(total_amount, na.rm = TRUE)
  ),
  by = pickup_borough
][order(-total_trips)]

print(borough_demand)

set.seed(123)

sample_n <- min(100000L, nrow(taxi))

taxi_sample <- taxi[
  sample.int(nrow(taxi), sample_n)
]

hour_groups <- split(
  taxi_sample$fare_amount,
  taxi_sample$pickup_hour
)

functional_result <- lapply(
  hour_groups,
  function(x) mean(x, na.rm = TRUE)
)

print(functional_result)

map_result <- purrr::map_dbl(
  hour_groups,
  ~ mean(.x, na.rm = TRUE)
)

print(map_result)

vectorized_result <- tapply(
  taxi_sample$fare_amount,
  taxi_sample$pickup_hour,
  mean,
  na.rm = TRUE
)

print(vectorized_result)

datatable_result <- taxi_sample[
  , .(average_fare = mean(fare_amount, na.rm = TRUE)),
  by = pickup_hour
]

print(datatable_result)


functional_numeric <- unlist(functional_result)

stopifnot(
  isTRUE(
    all.equal(
      as.numeric(functional_numeric),
      as.numeric(vectorized_result),
      tolerance = 1e-8,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      as.numeric(datatable_result$average_fare),
      as.numeric(vectorized_result),
      tolerance = 1e-8,
      check.attributes = FALSE
    )
  )
)

cat("The implementations produced equivalent mean-fare results.\n")

# Divide the data into four partitions
number_of_partitions <- 4L

partition_id <- (
  seq_len(nrow(taxi)) - 1L
) %% number_of_partitions + 1L

taxi_parts <- lapply(
  seq_len(number_of_partitions),
  function(i) taxi[partition_id == i]
)

# Check partition sizes
sapply(taxi_parts, nrow)

analyze_partition <- function(dt) {
  dt[
    , .(total_trips = .N),
    by = pickup_hour
  ]
}

sequential_time <- system.time({
  
  sequential_results <- lapply(
    taxi_parts,
    analyze_partition
  )
  
  sequential_summary <- rbindlist(
    sequential_results
  )[
    , .(total_trips = sum(total_trips)),
    by = pickup_hour
  ][order(pickup_hour)]
  
})

print(sequential_time)
print(sequential_summary)

# Choose a suitable number of cores
available_cores <- parallel::detectCores(
  logical = FALSE
)

if (is.na(available_cores)) {
  available_cores <- 2L
}

workers <- max(
  1L,
  min(2L, available_cores - 1L)
)

# Register the parallel backend
cl <- parallel::makeCluster(workers)
doParallel::registerDoParallel(cl)

parallel_time <- system.time({
  
  parallel_results <- foreach(
    i = seq_along(taxi_parts),
    .combine = "rbind",
    .packages = "data.table"
  ) %dopar% {
    
    analyze_partition(taxi_parts[[i]])
  }
  
  parallel_summary <- as.data.table(
    parallel_results
  )[
    , .(total_trips = sum(total_trips)),
    by = pickup_hour
  ][order(pickup_hour)]
  
})

# Stop workers and release resources
parallel::stopCluster(cl)
foreach::registerDoSEQ()

print(parallel_time)
print(parallel_summary)


stopifnot(
  isTRUE(
    all.equal(
      sequential_summary,
      parallel_summary,
      check.attributes = FALSE
    )
  )
)

sequential_seconds <- unname(
  sequential_time["elapsed"]
)

parallel_seconds <- unname(
  parallel_time["elapsed"]
)

speedup <- sequential_seconds / parallel_seconds

cat("Sequential elapsed time:",
    sequential_seconds, "seconds\n")

cat("Parallel elapsed time:",
    parallel_seconds, "seconds\n")

cat("Speedup:",
    round(speedup, 3), "\n")

ggplot(hourly_demand,
       aes(x = pickup_hour, y = total_trips)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "NYC Taxi Demand by Hour",
    x = "Pickup hour (0–23)",
    y = "Number of trips"
  ) +
  theme_minimal()

ggplot(weekday_demand,
       aes(x = pickup_weekday, y = total_trips)) +
  geom_col() +
  labs(
    title = "Taxi Demand by Day of the Week",
    x = "Day of week",
    y = "Number of trips"
  ) +
  theme_minimal()

ggplot(monthly_demand,
       aes(x = pickup_month, y = total_trips)) +
  geom_col() +
  labs(
    title = "Monthly Taxi Demand",
    x = "Pickup month",
    y = "Number of trips"
  ) +
  theme_minimal()

ggplot(fare_analysis,
       aes(x = pickup_hour, y = average_fare)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Taxi Fare by Hour",
    x = "Pickup hour (0–23)",
    y = "Average fare (currency units)"
  ) +
  theme_minimal()

top10_routes <- head(top_routes, 10)

top10_routes[
  , route := paste(
    pickup_zone,
    "to",
    dropoff_zone
  )
]

ggplot(
  top10_routes,
  aes(
    x = reorder(route, total_trips),
    y = total_trips
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequently Travelled Routes",
    x = "Pickup-to-drop-off route",
    y = "Number of trips"
  ) +
  theme_minimal()

# Use a sample to keep plotting responsive
set.seed(123)

plot_n <- min(5000L, nrow(taxi))

plot_sample <- taxi[
  sample.int(nrow(taxi), plot_n)
]

ggplot(
  plot_sample,
  aes(x = trip_distance, y = fare_amount)
) +
  geom_point(alpha = 0.35) +
  coord_cartesian(
    xlim = c(0, 30),
    ylim = c(0, 200)
  ) +
  labs(
    title = "Trip Distance versus Fare Amount",
    x = "Trip distance (miles)",
    y = "Fare amount (currency units)"
  ) +
  theme_minimal()

payment_analysis[
  , payment_label := paste(
    "Payment type",
    payment_type
  )
]

ggplot(
  payment_analysis,
  aes(
    x = payment_label,
    y = total_trips
  )
) +
  geom_col() +
  labs(
    title = "Distribution of Taxi Payment Types",
    x = "Payment type code",
    y = "Number of trips"
  ) +
  theme_minimal()









