bikes_tbl <- read_excel("DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  separate(
    col = category,
    into = c("category.1", "category.2", "category.3"),
    sep = " - "
  ) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

bikes_tbl %>%
  select(bike_id, model, model_year)

bikes_tbl %>%
  select(1:3)

bikes_tbl %>%
  select(1, contains("model"))

bikes_tbl %>%
  select(model, price)

bikes_tbl %>%
  select(category_1:category_3, everything())

bikes_tbl %>%
  relocate(category_1:category_3)

bikes_tbl %>%
  select(starts_with("model"))

bikes_tbl %>%
  pull(price) %>%
  mean()

bikes_tbl %>%
  select(where(is.character))

bikes_tbl %>%
  select(where(is.numeric))

bikes_tbl %>%
  select(!where(is.numeric))

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>%
  rename(
    "Model" = model,
    "Bike Family" = category_1,
    "Ride Style" = category_2,
    "Bike Category" = category_3,
    "Price in Euro" = price
  )

bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>%
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

bikes_tbl %>% 
  select(model, category_1, category_2, category_3, price) %>%
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())


# Basic Row Operations

bikes_tbl %>%
  select(model, price) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > mean(price))

bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000)|(price < 1000)) %>%
  arrange(desc(price)) %>%
  View()

bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurance")) %>%
  View()

bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(category_2 %in% c("E-Mountain")) %>%
  View()

bikes_tbl %>%
  filter(category_2 == "E-Mountain")

bikes_tbl %>%
  filter(category_2 != "E-Mountain")

bikes_tbl %>%
  filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))

bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)

# Column Transformations

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

bike_orderlines_tbl %>%
  mutate(freight_costs = 2*weight) 

bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))

bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5) %>% View()

bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
   filter(is_strive)

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price,3)) %>%
  select(total_price, price_binned, everything()) # divides into groups based on how expensive or cheap, like percentile

bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low"
 )) %>%
  select(total_price, price_binned, price_binned2, everything()) %>%
  View()


bike_orderlines_tbl %>%
  mutate("Aeroad or Ultimate" = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate"
  )) %>%
  select("Aeroad or Ultimate", everything()) %>%
  View()


# Summary Calculations

bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))

bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(revenue))


bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(count = n(),
            avg = mean(total_price),
            med = median(total_price),
            sd = sd(total_price),
            min = min(total_price),
            max = max(total_price)) %>%
  ungroup() %>%
  arrange(desc(count))

bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA,4), total_price[5:nrow(.)]))

bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  View()

bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.))) %>%
  View()

bike_orderlines_missing %>%
  filter(!is.na(total_price))

# Reshaping/ Pivoting

bike_data_sizes_tbl %>%
  select(model, price_euro, colors, size, stock_availability) %>%
  pivot_wider(names_from = size,
              values_from = stock_availability)

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales)) 

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),                         
    Road = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    ) 

bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols = c(names(.)[2:6]),
               names_to = "category_1",
               values_to = "sales",
               values_drop_na = T) %>%
  mutate(sales = sales %>% str_remove_all("€|\\.") %>% as.double())


# Joining and Binding

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl <- bike_orderlines_tbl %>% select(1:2,4:8)

order_dates_tbl %>%
  left_join(y = order_items_tbl, by = c("order_id" = "order_id", "order_line" = "order_line"))

bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )

train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

train_tbl %>%
  bind_rows(test_tbl)

# Splitting and Combining

bike_orderlines_tbl %>%
  select(order_date) %>%
  mutate(order_date = as.character(order_date)) %>%
  separate(col = order_date,
           into = c("year", "month", "day"),
           sep = "-", remove = FALSE) %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  ) %>%
  
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))
           

# Data.table

library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
covid_data_dt 

class(covid_data_dt)

test_df <- data.frame(matrix(runif(10000000), nrow = 1000000))
write.csv(test_df, "test_df.csv", row.names = F)

system.time({test_df_base <- read.csv("test_df.csv")})
system.time({test_df_readr <- read_csv("test_df.csv")})
# Time taken by fread to import
system.time({test_dt <- fread("test_df.csv")})


test_dt <- data.table(ID = c("b", "b", "b", "a", "a", "c"),
                      a = 1:6,
                      b = 7:12,
                      c = 13:18)

covid_data_dt[year == 2019, sum(cases), by = continentExp]

# Basic row/column operations
covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]

covid_data_dt[1:2]

covid_data_dt[order(year, month, day, -countriesAndTerritories)]

# Select columns in j and rename columns

covid_data_dt[, geoId]

covid_data_dt[,c("geoId","countriesAndTerritories")]

covid_data_dt[,list(geoId)]

covid_data_dt[,.(geoId)]

covid_data_dt[,.(geoId, countriesAndTerritories)]

covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

select_cols = c("cases", "deaths")

covid_data_dt[,..select_cols]

colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

# Exercise

data()
data("airquality")
airquality_tbl <-  data.table(airquality) 
airquality_tbl[!is.na(Ozone), .(Solar.R, Wind, Temp)]

setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]

# Compute or do j

covid_data_dt[, sum(deaths>1000)]

covid_data_dt[deaths > 1000]

# Create new Columns

covid_data_dt[, deaths_per_capita := deaths / popData2019]

covid_data_dt[, `:=`(deaths_per_capita = deaths / popData2019,
                     cases_per_capita = cases / popData2019,
                     deaths_per_cases = deaths / cases)]
covid_data_dt[, deaths_per_cases := NULL]

covid_data_dt[, dateRep := lubridate::dmy(dateRep)] %>% View()


# Exercise
data("mtcars")
mtcars$carname <- rownames(mtcars)
mtcars_tbl <- data.table(mtcars) 

mtcars_tbl[, mileage_type := ifelse(mpg > 20,
                                    "high",
                                    "low")] %>% View()

# Subset in i and do in j

covid_data_dt[country == "Germany" & month == 4,
              .(m_cases = mean(cases),
                m_death = mean(deaths)
                )
              ]

covid_data_dt[country == "United_States_of_America" & month == 6 & deaths < 1000,
              length(day)]

covid_data_dt[country == "United_States_of_America" & month == 6 & deaths < 1000,
              .N]

# Aggregations / Grouping

covid_data_dt[deaths > 1000, .I, by = country]

covid_data_dt[, .I[deaths > 1000]]

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)]

# Exercise

mtcars_tbl[, 
           .( `Number of Cars` = .N, mileage = mean(mpg)),
           by = .(gear)]

# Advanced Operations
# Chaining

covid_cases_means <- covid_data_dt[, .(m_cases = mean(cases) %>% round(1),
                                      m_deaths = mean(deaths) %>% round(1)),
                                      by = .(country)]
covid_data_dt[, .(
  m_cases = round(mean(cases), digits = 1),
  m_deaths = round(mean(deaths), digits = 1)
),
  by = .(country)][order(-m_cases)]

covid_data_dt[, .N,
              .(
                death_gt_1k = deaths > 1000,
                cases_lt_1k = cases < 1000
              )]

covid_data_dt[, .N,
              .(
                death_gt_1k = deaths > 1000,
                cases_lt_1k = cases < 1000,
                year
              )]

# .SD

covid_data_dt[, print(.SD), by = year] %>% View()

covid_data_dt[, lapply(.SD, mean), by = year]

covid_data_dt[, lapply(.SD, mean), by = .(year, month), .SDcols = c("cases","deaths")]

# Keys

setkey(covid_data_dt, date, country) 

covid_data_EUR_dt <- covid_data_dt[ continent == "Europe",
                                    lapply(.SD, function(x){
                                      x %>% mean() %>%
                                        round(1)
                                    }
                                    ),
                                    by = .(country),
                                    .SDcols = c("cases", "deaths")]
setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)

cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country,deaths)]


setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)

cd_dt1[cd_dt2, on = "country"]

cd_dt1[cd_dt2, on = c(colA = "colB")]

merge(cd_dt1, cd_dt2, by = "country")
merge(cd_dt1, cd_dt2, by = "country", all.x = T)
merge(cd_dt1, cd_dt2, by = "country", all = T)

cd_dt1[cd_dt2, on = "country", deaths := i.deaths]

dt_list <- list(cd_dt1, cd_dt2)

merge_func <- function(...) merge(..., all = TRUE, by = "country")
dt_merges <- Reduce(merge_func, dt_list)

# set() function

m = matrix(1, nrow=100000, ncol = 100)

DF = as.data.frame(m)
DT = as.data.table(m)

system.time(for(i in 1:100000) DF[i,1] <- i)

system.time(for (i in 1:100000) DT[i,V1:=1])

system.time(for (i in 1:100000) set(DT,i,1L,i))

# Business Case
# Wrangling large data with data.table. The topic is Bank Loan Defaults

library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

# 2.0 DATA IMPORT ----

# 2.1 Loan Acquisitions Data ----

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))

acquisition_data <- vroom(
  file = "loan_data/Acquisition_2019Q1.txt",
  delim = "|",
  col_names = names(col_types_acq),
  col_types = col_types_acq,
  na = c("", "NA", "NULL")
)

acquisition_data %>% glimpse()

# 2.2 Performance Data ----
col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file = "loan_data/Performance_2019Q1.txt",
  delim = "|",
  col_names = names(col_types_perf),
  col_types = col_types_perf,
  na = c("", "NA", "NULL")
)

performance_data %>% glimpse()

# data.table & Wrangling
# Convert to data.table

class(acquisition_data)
setDT(acquisition_data)
class(acquisition_data)

acquisition_data %>% glimpse()

setDT(performance_data)
performance_data %>% glimpse()

# 4. Data Wrangling
# Merge the data via the loan_id

tic()
combined_data <- merge(x = acquisition_data, y = performance_data,
                       by = "loan_id",
                       all.x = TRUE,
                       all.y = FALSE)
toc()

combined_data %>% glimpse()

tic()
performance_data %>%
  left_join(acquisition_data, by = "loan_id")
toc()

# prepare the data
setkey(combined_data, "loan_id")
key(combined_data)

?setorder()
setorderv(combined_data, c("loan_id", "monthly_reporting_period"))

combined_data %>% dim()

keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")

combined_data <- combined_data[, ..keep_cols]
combined_data %>% dim()
combined_data %>% glimpse()

# 4.4 Grouped Mutations
combined_data$current_loan_delinquency_status %>% unique()

combined_data[,current_loan_delinquency_status] %>% unique()

tic()
temp <- combined_data %>%
  group_by(loan_id) %>%
  mutate(gt_1mo_behind_in_3mo_dplyr = lead(current_loan_delinquency_status, n = 3) >= 1) %>%
  ungroup()
toc()

tic()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
toc()

combined_data %>% dim()
rm(temp)

# Analysis & Findings
tic()
combined_data[!is.na(monthly_reporting_period) , .N, by = monthly_reporting_period]
toc()

tic()
combined_data %>%
  filter(!is.na(monthly_reporting_period)) %>%
  count(monthly_reporting_period)
toc()

tic()
combined_data[current_loan_delinquency_status >=1, 
              list(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)][
                , max(current_loan_delinquency_status), by = loan_id
              ][
                order(V1, decreasing = TRUE)
              ]
toc()

tic()
combined_data %>%
  group_by(loan_id) %>%
  summarise(total_delinq = max(current_loan_delinquency_status)) %>%
  ungroup() %>%
  arrange(desc(total_delinq))
toc()

tic()
combined_data[current_loan_delinquency_status >=1,
              list(current_loan_delinquency_status, monthly_reporting_period,current_upb),
              by = loan_id][
                order(current_loan_delinquency_status, decreasing = TRUE, by = loan_id)
              ][!is.na(current_upb)
                , .(currentupb = max(current_upb), maxdel = max(current_loan_delinquency_status)), by = loan_id]
toc()

tic()
test <- combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][
  !is.na(current_upb)][
  order(-current_upb), .(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)  
  ]
toc()


tic()
upb_by_company_dt <- combined_data[!is.na(current_upb),
                                   .SD[.N],
                                   by = loan_id][
                                   ,.(sum_current_upb = sum(current_upb, na.rm = TRUE), cnt_current_upb = .N), by = seller_name
                                   ][
                                     order(sum_current_upb, decreasing = TRUE)
                                   ]
toc()

upb_by_company_dt
