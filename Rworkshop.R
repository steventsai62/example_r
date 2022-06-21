install.packages("tidyverse")
#function name if the same in many packages
#good habit that r would remind 
install.packages("conflicted")
#fights
install.packages("nycflights13")
library(tidyverse)
library(conflicted)
library(nycflights13)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
df <- nycflights13::flights
glimpse(df)
head(df,10)
filter(df, month==12, day==25, dest=="ATL")
filter(df, month==12 | day==25)
Christmas <- filter(df, month == 12, day == 25)
(Jan1 <- filter(df, month == 1, day == 1))

#month in 7 or 8
filter(df, month == 7 | month == 8)
filter(df, month %in% c(7, 8)) 

#delay <= 60
filter(df, !(arr_delay > 60 | dep_delay > 60))
filter(df, arr_delay <= 60, dep_delay <= 60)

arrange(df, year, month, day)
arrange(df, year, desc(month), day)
tail(arrange(df, year, desc(month), day))

result_arrange <- arrange(df, desc(arr_delay))
#put what i want to check at the first one 
select(result_arrange, arr_delay, everything())
head(select(result_arrange, arr_delay, everything()))
tail(select(result_arrange, arr_delay, everything()))


# select specific columns
select(df, year, month, day)
# select all columns between year and day
select(df, year:day) 
# not to use index df[4:7]
# select all columns except those from year and day
select(df, -(year:day))

#regarding "time"
select (df, contains("time"))
select(df, carrier, origin, dest, distance, everything())

# we start by creating a smaller dataset.
df1 <- select(df, year:day, ends_with("delay"), distance, air_time)
mutate(df1, 
       gain= arr_delay - dep_delay, 
       speed = distance / air_time * 60,
       hours = air_time / 60,
       gain_per_hour = gain / hours)
transmute(df1, 
          gain= arr_delay - dep_delay, 
          speed = distance / air_time * 60,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

summarize(df, delay = mean(dep_delay, na.rm=T))
group_by(df, year, month, day)
by_day <- group_by(df, year, month, day) 
summarize(by_day, 
          ave_dep_delay = mean(dep_delay, na.rm = T),
          ave_arr_delay = mean(arr_delay, na.rm = T)
)

# group the Flights Data by the destination and check
by_dest <- group_by(df, dest)
delay <- summarize(by_dest,
                   count = n(),
                   ave_dist = mean(distance, na.rm=T),
                   ave_arr_delay = mean(arr_delay, na.rm=T)
)
delay <- filter(delay, count > 20, dest != "HNL")

 delay <- df %>% 
  group_by(dest) %>%
  summarize(
    count = n(),
    ave_dist = mean(distance, na.rm=T),
    ave_arr_delay = mean(arr_delay, na.rm=T)
  ) %>%
  filter(count > 20, dest != "HNL")
delay

#use filter, no longer need na.rm
not_cancelled <- df %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(dest) %>%
  summarize(
    distance_mu = mean(distance),
    distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd)) %>% 
  head()

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first = min(dep_time), # the first flight departed each day
    last = max(dep_time) # the last flight departed each day
  ) %>% 
  head()

# finds the first and last departure for each day
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    first_dep = first(dep_time),
    last_dep = last(dep_time)
  ) %>% 
  head()

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers)) %>% 
  head()

#more column than table()
not_cancelled %>% 
  count(dest) %>% 
  head(5)
not_cancelled %>%
  count(tailnum, wt = distance) %>% 
  head(5)

#check how many flights left before 5AM
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(n_early = sum(dep_time < 500)) %>% 
  head()

#what proportion of flights are delayed by more than one hour
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(hour_perc = mean(arr_delay > 60)) %>% 
  head()

#how to group the data by multiple variables
per_day <- df %>% 
  group_by(year, month, day) %>%
  summarize(flights = n())

per_day %>% head()

per_month <- summarize(per_day, flights = sum(flights))
per_month %>% head()
  # easy wrong
per_year <- summarize(per_month, flights = sum(flights))
per_year
 #ungroup()
daily <- df %>% group_by(year, month, day)
daily %>% 
  ungroup() %>% # no longer grouped by date
  summarize(flights=n()) # all flights

# finds the worst members of each group
df1 %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10) %>% 
  head()
# all groups bigger than a threshold
popular_dests <- df %>%
  group_by(dest) %>% 
  filter(n()>365)
popular_dests %>% head()

#compute per group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, arr_delay, prop_delay) %>%
  
  head()
#visualization
library(tidyverse)
library(conflicted)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")
glimpse(mpg)

# need this package to create site-by-site plots
install.packages("patchwork")
library(patchwork)
# create canvas
p1 <- ggplot(mpg)
# variables of interest mapped
p2 <- ggplot(mpg, mapping = aes(x = displ, y = hwy))
p1+p2
#the relationship between engine displacement and highway mileage per gallon
# data plotted
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

mpg %>% ggplot(aes(x=displ, y=hwy)) + geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

# x and y mapping needed when creating a scatterplot
p1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()
p1 + p2

p1 <- ggplot(mpg, aes(x = class)) +
  geom_bar()  
p2 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram() 
p1 + p2


# no y mapping needed when creating a bar chart
p1 <- ggplot(mpg, aes(y = class)) +
  geom_bar(fill = , alpha = 0.2)  
p2 <- ggplot(mpg, aes(x = hwy)) +
  geom_histogram(aes(y = ..density..), binwidth = density(mpg$hwy)$bw) +
  geom_density(fill= daytonred, alpha = 0.2)
p1 + p2

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue") +
  geom_smooth(color = "red")


p1 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(se = FALSE)
# color aesthetic specified for only the geom_point layer
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE)
p1 + p2

mpg %>% ggplot(aes(x=class)) + 
  geom_bar()

barplot(table(mpg$class))

(class_count <- count(mpg, class))
ggplot(class_count, aes(x = class, y = n)) +
  geom_bar(stat = "identity")

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(color = "grey") + 
  stat_summary(fun.y = "mean", geom = "line", size = 1, linetype = "dashed")

ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar()

# position = "dodge": values next to each other
p1 <- ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")
# position = "fill": percentage chart
p2 <- ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill")
p1 + p2


# default color brewer
p1 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_color_brewer()
# specifying color palette
p2 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_color_brewer(palette = "Set3")

p1 + p2

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~ class, nrow=2)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(year ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  labs(title = "Fuel Efficiency by Engine Power",
       subtitle = "Fuel economy data from 1999 and 2008 for 38 popular models of cars",
       x = "Engine Displacement (liters)",
       y = "Fuel Efficiency (miles per gallon)",
       color = "Car Type")
install.packages("plotly")
library(plotly)
p1 <- ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point() 
ggplotly(p1, width = 400, height = 300)

install.packages("DataExplorer")
library(DataExplorer)
insurance <- read_csv("https://raw.githubusercontent.com/Ying-Ju/R_Data_Analytics_Series_NTPU/main/insurance.csv")
glimpse(insurance)

plot_intro(insurance)
plot_missing(insurance)
plot_bar(insurance)
?plot_bar
plot_bar(insurance, with="charges")
plot_histogram(insurance, ncol=2)
plot_histogram(select(insurance, -children), ncol=2)
insurance_Q <- insurance %>% 
  select(age, bmi, charges, region) %>% 
  drop_na()
plot_boxplot(insurance_Q, by = "region")
plot_scatterplot(insurance_Q %>% select(-region), by = "charges")
plot_scatterplot(insurance_Q %>% select(-region), by = "charges", sampled_rows=100)
plot_scatterplot(insurance_Q %>% 
                   filter(region=="northwest") %>% 
                   select(-region), 
                 by = "charges")
plot_correlation(insurance_Q %>% 
                   select(-region), 
                 cor_args = list( "use" = "complete.obs"))
#if didn't complete obs , it will note you it has dropped some missing value
?create_report
create_report(insurance, output_file = "report.html", 
              output_dir = "C:/Steven/Doc/Fourthgrade/Rprogram")
install.packages("xaringanthemer")
library("xaringanthemer")
