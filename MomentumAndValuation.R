library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(quantmod)
library(data.table)

# Read historical CAPE data from Barclay's
capedata <- read.csv("historical_capes.csv",
                     fileEncoding = "UTF-8-BOM",
                     check.names = FALSE)

# Replace zeros with NAs and format
capedata <- capedata %>% 
  mutate_if(is.numeric, ~replace(., . == 0, NA)) %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
  filter(Date > "2011-06-30") %>% 
  select(-Europe) %>% 
  select(Date, noquote(order(colnames(.)))) %>% 
  as.data.table()

# Get needed countries and their tickers
countries <- data.frame(
  country = c("Russia", "Italy", "Israel", "Spain", "Brazil", "Turkey",
              "Singapore", "Poland", "China", "Netherlands", "UK", "France",
              "Taiwan", "Australia", "Korea", "Hong Kong", "Germany", "Sweden",
              "India", "Switzerland", "Canada", "Mexico", "South Africa",
              "Japan", "USA"), 
  ticker = c("RSX", "EWI", "EIS", "EWP", "EWZ", "TUR", "EWS", "EPOL", "GXC",
             "EWN", "EWU", "EWQ", "EWT", "EWA", "EWY", "EWH", "EWG", "EWD", 
             "INDY", "EWL", "EWC", "EWW", "EZA", "EWJ", "VTI"),
  stringsAsFactors = FALSE)

# Get historical returns in a data frame
data <- as.data.frame(NULL)
for(i in 1:nrow(countries)){
  tick <- as.character(countries$ticker[i])
  # Year before since 12-month momentum is used
  temp <- getSymbols(tick, from = "2010-06-30", auto.assign = FALSE)[, 6]
  temp <- rownames_to_column(as.data.frame(temp))
  assign(tick, temp)
  if (i != 1) {
    data <- suppressMessages(full_join(as.data.frame(data), temp))
  } else {
    data <- rbind(data, temp)
  }
}

# Add country names to columns
colnames(data)[-1] <- countries$country
colnames(data)[1] <- "Date"

# Format and filter for max date for each month, arrange, calculate momentum
data <- data %>% 
  mutate(Date = as.Date(Date)) %>%
  group_by(strftime(Date, "%Y-%m")) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  select(-`strftime(Date, "%Y-%m")`) %>% 
  mutate_if(is.numeric, ~lag(lead(., 12) / ., 12)) %>% 
  select(Date, noquote(order(colnames(.)))) %>% 
  as.data.table()

# Join by using nearest dates
combined <- data[capedata, on = "Date", roll = "nearest" ]

# Separate momentum and valuation data for gathering
momentum <- combined %>% 
  select(Date, Australia:USA) %>% 
  gather("Country", "Momentum", -Date)
valuation <- combined %>% 
  select(Date, i.Australia:i.USA) %>%
  rename_at(vars(colnames(.)), ~ c("Date", unique(momentum$Country))) %>% 
  gather("Country", "Valuation", -Date)

# Plot all countries ----
all_countries <- inner_join(momentum %>% filter(Date == "2019-04-30"),
                            valuation %>% filter(Date == "2019-04-30"))

ggplot(all_countries, aes(x = Valuation, y = Momentum,
                          color = Country, label = Country)) +
  geom_point(show.legend = FALSE) +
  geom_point(color = "black", stroke = 1, shape = 21, fill = "white") +
  geom_text_repel(show.legend = FALSE) +
  ggtitle("Valuation (CAPE) vs momentum (1-year return) of different countries") +
  labs(subtitle = "2019/04/30",
       caption = "Source: shiller.barclays.com/SM/12/en/indices/static/historic-ratios.app \n
Blog post at: databasedinvesting.blogspot.com") +
  theme(plot.caption = element_text(hjust = 0, lineheight = 0.5))

# Plot selected countries ----
selected_countries <- inner_join(valuation, momentum) %>% 
  filter(Date %in% as.Date(c("2019-04-30", "2018-04-30", "2017-04-28",
                             "2016-04-28", "2015-04-30", "2014-04-30",
                             "2013-04-30", "2012-04-27")),
                            Country %in%c("Israel", "Turkey", "Russia", "USA"))

ggplot(selected_countries, aes(x = Valuation, y = Momentum,
                               color = Country, group = Country,
                               label = Country)) +
  geom_path(show.legend = FALSE, size = 1) +
  geom_point(color = "black", stroke = 1, shape = 21, fill = "white") +
  geom_text_repel(show.legend = FALSE,
                  data = selected_countries %>% filter(Date == "2019-04-30")) +
  ggtitle("Valuation (CAPE) vs momentum (1-year return) paths of selected countries") +
  labs(subtitle = "2012/04/27 - 2019/04/30, yearly",
       caption = "Source: shiller.barclays.com/SM/12/en/indices/static/historic-ratios.app \n
Blog post at: databasedinvesting.blogspot.com") +
  theme(plot.caption = element_text(hjust = 0, lineheight = 0.5))

# Plot just USA ----
just_usa <- inner_join(valuation, momentum) %>% 
           filter(Country == "USA") %>% 
           filter(Date >= "2012-04-27")

ggplot(just_usa, aes(x = Valuation, y = Momentum,
                    color = Date, label = Country)) +
  geom_path(show.legend = FALSE, size = 1) +
  geom_text_repel(show.legend = FALSE,
                  data = just_usa %>% filter(Date == "2019-04-30"),
                  force = TRUE) +
  geom_point(color = "black", stroke = 1, shape = 21, fill = "white") +
  ggtitle("Valuation (CAPE) vs momentum (1-year return) path of S&P 500") +
  labs(subtitle = "2012/04/27 - 2019/04/30, monthly",
       caption = "Source: shiller.barclays.com/SM/12/en/indices/static/historic-ratios.app \n
Blog post at: databasedinvesting.blogspot.com") +
  theme(plot.caption = element_text(hjust = 0, lineheight = 0.5))
