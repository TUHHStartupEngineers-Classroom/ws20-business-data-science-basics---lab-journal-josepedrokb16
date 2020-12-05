scales::dollar(100, prefix = "", suffix = " €",
               big.mark = ".", decimal.mark = ",")

library(tidyverse)
library(lubridate)

sales_by_year_tbl <- bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, prefix = "", suffix = " €",
                                     big.mark = ".", decimal.mark = ","))

sales_by_year_tbl

sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales, color = sales)) + 
  
  geom_line(size = 1, color = "green") +
  geom_point(color = "black") + 
  geom_smooth(method = "lm", se = FALSE)


order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price = sum(total_price)
  ) %>%
  ungroup()

order_value_tbl

order_value_tbl %>%
  ggplot(aes(x = total_quantity, y = total_price)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)


revenue_by_month_tbl <- bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_month_tbl

revenue_by_month_tbl %>%
  ggplot(aes(year_month, revenue)) + 
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)


revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  select(category_2, total_price) %>%
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_category_2_tbl

revenue_by_category_2_tbl %>% 
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  ggplot(aes(category_2, revenue)) + 
  geom_col(fill = "#2c3e50") + 
  coord_flip()

bike_orderlines_tbl %>% 
  distinct(model, price) %>%
  ggplot(aes(price)) +
  geom_histogram(bins = 25, fill = "orange", color = "white")

bike_orderlines_tbl %>%
  distinct(price, model, frame_material) %>%
  ggplot(aes(price, fill = frame_material)) +
  geom_histogram() +
  facet_wrap(~ frame_material, ncol = 1)

bike_orderlines_tbl %>%
  distinct(price, model, frame_material) %>%
  ggplot(aes(price, fill = frame_material)) +
  geom_density(alpha = 0.5) + 
  theme(legend.position = "bottom")


unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  select(category_2, model, price) %>%
  distinct() %>%
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

unit_price_by_cat_2_tbl

unit_price_by_cat_2_tbl %>%
  ggplot(aes(category_2, price)) + 
  geom_boxplot() + 
  coord_flip()

unit_price_by_cat_2_tbl %>%
  ggplot(aes(category_2, price)) + 
  geom_jitter(width = 0.15, color = "#2c3e50") + 
  geom_violin(alpha = 0.5) + 
  coord_flip()

revenue_by_year_tbl <- bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

revenue_by_year_tbl

revenue_by_year_tbl %>%
  ggplot(aes(year, revenue)) + 
  geom_col(fill = "purple") + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_text(aes(label = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "",
                                       suffix = "M")),
            vjust = 6, color = "white") + 
  geom_label(label = "Major Demand This Year",
             vjust = -3,
             size = 5,
             fill = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>% filter(year %in% c(2019))) +
  expand_limits(y = 2e7)


sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales, color  = sales)) + 
  geom_line(size = 1) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous() + 
  scale_y_continuous() +
  scale_colour_continuous()

sales_by_year_tbl %>%
  ggplot(aes(x = year, y = sales, color = sales)) + 
  geom_line(size = 1) + 
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") + 
  expand_limits(y = 0) +
  scale_color_continuous(low = "#95E1EA", high = "#2097A3",
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                       prefix = "", 
                                                       suffix = "M €")) + 
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) + 
labs(title = "Revenue",
       subtitle = "Sales are trending up and to the right!",
       x = "",
       y = "Sales (Millions",
       color = "Rev (M €)",
       caption = "What's happening?\nSales numbers showing year-over-year growth")

library(ggthemes)

sales_by_month_2015 <- bike_orderlines_tbl %>%
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  mutate(month = month(order_date)) %>%
  
  filter(year == "2015") %>%
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",  
                                     suffix          = " €"))

sales_by_month_2015

sales_by_month_2015 %>%
  ggplot(aes(x = month, y = sales, color = sales)) +
  
  geom_line(size = 1) + 
  geom_point(size = 5) + 
  geom_smooth(method = "lm", se = FALSE) +
  expand_limits(y = 0) + 
  scale_color_continuous(low = "red", high = "green",
                         labels = scales::dollar_format(scale = 1/1e6,
                                                         prefix = "",
                                                         suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month,
                     labels = month(sales_by_month_2015$month, label = T)) + 
  
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M")) + 
  
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's hapenning?\nSales numbers are dropping towards the end of the year."
  ) + 
  theme_economist() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))


sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_Date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)

colors()

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) + 
  geom_col(fill = "slateblue")

col2rgb("slateblue")

col2rgb("#2C3E50")

rgb(44,62,80, maxColorValue = 255)

RColorBrewer::display.brewer.all()

RColorBrewer::brewer.pal.info

RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

viridisLite::viridis(n = 20)

sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue)) + 
  geom_col(fill = viridisLite::viridis(n = 20)[10])


sales_by_year_category_1_tbl %>% 
  ggplot(aes(year, revenue, color = category_1)) + 
  geom_line(size = 1) +
  geom_point(color = "dodgerblue", size = 5)

sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue)) + 
  geom_col(aes(fill = category_1))

sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, size = revenue)) +
  geom_line(aes(color = category_1), size = 1) +
  geom_point()

# Faceting

sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, color = category_1)) + 
  geom_line(color = "black") + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ category_1, ncol = 3, scales = "free_y") +
  expand_limits(y = 0)


# position Adjustment (Stack and Dodge)

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_col(position = position_dodge(width = 0.9), color = "white")


sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, fill = category_1)) +
  geom_area(color = "black")

# Scales (Colors, Fills, Axis)

g_facet_continuous <- sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, color = revenue)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  
  facet_wrap(~ category_1, scales = "free_y") + 
  expand_limits(y = 0) + 
  theme_minimal()

g_facet_continuous

g_facet_discrete <- sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, color = category_1)) + 
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  
  facet_wrap(~ category_1, scales = "free_y") + 
  expand_limits(y = 0) + 
  theme_minimal()

g_facet_discrete


g_area_discrete <- sales_by_year_category_1_tbl %>%
  ggplot(aes(year, revenue, fill = category_1)) + 
  geom_area(color = "black") + 
  theme_minimal()

g_area_discrete


g_facet_continuous + 
  scale_color_viridis_c(option = "E", direction = -1) 

RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
RColorBrewer::brewer.pal(n = 8, name = "Blues")

g_facet_discrete +
  scale_color_brewer(palette = "Set3") + 
  theme_dark()

g_facet_discrete + 
  scale_color_viridis_d(option = "D") + 
  theme_dark()

g_area_discrete + 
  scale_fill_brewer(palette = "Set3")

g_area_discrete + 
  scale_fill_viridis_d()

g_facet_continuous + 
  scale_x_continuous(breaks = seq(2015, 2019, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    preix = "",
                                                    suffix = "M"))

# Labels

g_facet_continuous + 
  
  scale_x_continuous(breaks = seq(2011, 2015, by = 2)) + 
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    preix = "",
                                                    suffix = "M")) + 
  geom_smooth(method = "lm", se = FALSE) + 
  
  scale_color_viridis_c() + 
  theme_dark() + 
  
  labs(
    title = "Bike Sales",
    subtitle = "Sales are trending up",
    caption = "5-year sales trends\ncomes from our ERP Database",
    x = "Year",
    y = "Revenue (M €)",
    color = "Revenue"
  )

# Themes

View(g_facet_continuous)

g_facet_continuous + 
  
  theme_light() + 
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.background = element_rect(
      color = "black",
      fill = "cornflowerblue",
      size = 1
    ),
    strip.text = element_text(
      face = "bold",
      color = "white"
    )
  )

# Example Graph: Putting it All Together

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue, fill = category_1)) +
  
  geom_area(color = "black") + 
  
  scale_fill_brewer(palette = "Blues", direction = -1) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " €")) + 
  
  labs(
    title = "Sales Over Year by Category 1",
    subtitle = "Sales Trending Upward",
    x = "",
    y = "Revenue (M €)",
    fill = "2nd Category",
    caption = "Bike sales trends look strong heading into 2020"
  ) + 
  
  theme_light() + 
  theme(
    title = element_text(face = "bold", color = "#08306B")
  )

# Factors

library(tidyverse)

library(forcats)

starwars %>%
  filter (!is.na(species)) %>%
  count(species, sort = TRUE)


starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = as_factor(species) %>%
           fct_lump(n = 3)) %>%
  count(species)

f <- factor(c("a", "b", "c", "d"), levels = c("b", "c", "d", "a"))
f

fct_reorder(f, c(2,1,4,3))

fct_relevel(f, "a")

fct_relevel(f, "b", "a")

fct_relevel(f, "a", after = 2)

fct_relevel(f, "a", after = Inf)

fct_relevel(f, "a", after = 3)

# Business Case

library(tidyverse)
library(lubridate)

n <- 10

top_customers_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, total_price) %>%
  
  mutate(bikeshop = as_factor(bikeshop) %>% fct_lump(n = n, w = total_price)) %>%
  
  group_by(bikeshop) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  arrange(desc(bikeshop)) %>%
  
  mutate(revenue_text = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "", 
                                       suffix = "M €")) %>%
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%
  
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%
  
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))
  
  

top_customers_tbl

top_customers_tbl %>%
  ggplot(aes(revenue, bikeshop)) +
  geom_segment(aes(xend = 0, yend = bikeshop),
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size = 1) + 
  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) + 
  
  geom_label(aes(label = label_text),
             hjust = "inward",
             size = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) + 
  
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +
  
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
      End: {year(max(bike_orderlines_tbl$order_date))}"
    ),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute 52% of purchasing power.")
  ) + 
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )


# Case 2

pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain", "Road")) %>%
  
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  
  complete(bikeshop, nesting(category_1, category_2)) %>%
  mutate(across(total_qty, ~replace_na(.,0))) %>%
  
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  mutate(bikeshop_num = as.numeric(bikeshop))

pct_sales_by_customer_tbl


pct_sales_by_customer_tbl %>%
  
  ggplot(aes(category_2, bikeshop)) +
  
  geom_tile(aes(fill = pct)) + 
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)),
            size = 3) + 
  facet_wrap(~category_1, scales = "free_x") + 
  
  scale_fill_gradient(low = "white", high = "#2C3E50") + 
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road:
      to be discussed ... 
      
      Customers that prefer Mountain:
      To be discussed ..."
    )) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )

