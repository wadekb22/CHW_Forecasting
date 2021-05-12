#Load libraries----
library(readxl)
library(tidyverse)
library(lubridate)
library(writexl)

#Data Importing ----
#import and subset data
full_df <- read_csv('CHW_Forecasting-main/DataCleaning/merged_data.csv')
power_temp_df <- full_df %>% select("DATE", max_power = "Total Power (max)", 
                                    avg_power = "Total Power (avg)",
                                    min_power = "Total Power (min)", 
                                    sampled_power = "Total Power (samp)",
                                    "HourlyDryBulbTemperature",
                                    "HourlyWetBulbTemperature")

#import uncleaned data  
uncleaned_df <- readxl::read_xlsx("Power_Data_1.xlsx")

#Data Manipulation ----

#change hourly scale to daily
daily_df <- power_temp_df
daily_df$DATE <- daily_df$DATE %>% as_date()
daily_df <- daily_df %>%
  group_by(DATE) %>%
  summarise(`Median Daily Total Power`=median(avg_power),
            `Median Daily Temperature`=median(HourlyDryBulbTemperature))

#change hourly scale to daily - maximum
max_daily_df <- power_temp_df
max_daily_df$DATE <- max_daily_df$DATE %>% as_date()
max_daily_df <- max_daily_df %>%
  group_by(DATE) %>%
  summarise(`Max Daily Total Power`=max(avg_power),
            `Max DryBulb Temperature`=max(HourlyDryBulbTemperature),
            `Max WetBulb Temperature`=max(HourlyWetBulbTemperature))

#change hourly scale to total daily - sum
sum_daily_df <- power_temp_df
sum_daily_df$DATE <- sum_daily_df$DATE %>% as_date()
sum_daily_df <- sum_daily_df %>%
  group_by(DATE) %>%
  summarise(`Sum Daily Total Power`=sum(avg_power),
            `Sum Daily Temperature`=sum(HourlyDryBulbTemperature))

#change hourly scale to weekly median
weekly_df <- power_temp_df
weekly_df$DATE <- weekly_df$DATE %>% as_date() %>% epiweek()
weekly_df <- weekly_df %>%
  group_by(DATE) %>%
  summarise(`Median Weekly Total Power`=median(`Total Power`),
            `Median Weekly Temperature`=median(HourlyDryBulbTemperature))

#change hourly scale to weekly max
max_weekly_df <- power_temp_df
max_weekly_df <- max_weekly_df %>%
  group_by(week(DATE), year(DATE)) %>%
  summarise(`Max Weekly Total Power`=max(avg_power),
            `Max Weekly DryBulb`=max(HourlyDryBulbTemperature),
            `Max Weekly WetBulb`=max(HourlyWetBulbTemperature))
max_weekly_df <- max_weekly_df %>%
  ungroup() %>%
  mutate(WEEK = paste0(`year(DATE)`, "-", str_pad(`week(DATE)`, 2, pad = "0"))) %>%
  arrange(WEEK)
max_weekly_df <- max_weekly_df %>%
  mutate(Week = ymd(paste0(max_weekly_df$`year(DATE)`, "-1-1")))
week(max_weekly_df$Week) <- max_weekly_df$`week(DATE)`


#change hourly to daily max
max_df <- power_temp_df
max_df$DATE <- max_df$DATE %>% as_date()
max_df <- max_df %>%
  group_by(DATE) %>%
  summarise(`Max Daily Total Power`=max(`Total Power`),
            `Max Daily Temperature`=max(HourlyDryBulbTemperature))

chiller_4_data <- uncleaned_df %>% select(date_time, `Chiller-4 Power`)
chiller_4_data <- chiller_4_data %>% mutate(Negative = `Chiller-4 Power` < 0, date_time = as.Date(date_time))
  
#export to csv
#daily_df %>% write_csv("daily_data.csv")

#Plots ----

#pivoting data
pivoted_daily_df <- daily_df %>%
  pivot_longer(!DATE, names_to="Category", values_to="Value")

#Median Daily Total Power Mar 2019-Mar 2020
ggplot(daily_df, aes(DATE, `Median Daily Total Power`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#808080")) + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
    ) +
  labs(y = "Median Total Power", x = "Date", title="Median Total Power by Day Mar 2019-Mar 2020")

ggplot(daily_df, aes(DATE, `Median Daily Total Power`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#808080")) + 
  theme_light() + 
  labs(y = "Median Total Power (tons)", x = "Date", title="Median Total Power by Day Mar 2019-Mar 2020")
  
#Median Weekly Total Power Mar 2019-Mar 2020
ggplot(weekly_df, aes(DATE, `Median Weekly Total Power`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#808080")) + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
    )  + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50), labels = c("Mar 1, 2019", 
                                                            "May 10, 2019", 
                                                            "July 17, 2019",  
                                                            "Sept 27, 2019", 
                                                            "Dec 6, 2019",
                                                            "Feb 14, 2020")) +
  labs(y = "Median Total Power (tons)", x = "Date", title="Median Total Power by Week Mar 2019-Mar 2020")

#Median Weekly Temperature Mar 2019-Mar 2020
ggplot(weekly_df, aes(DATE, `Median Weekly Temperature`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#808080")) + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
  ) + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50), labels = c("Mar 1, 2019", 
                                                            "May 10, 2019", 
                                                            "July 17, 2019",  
                                                            "Sept 27, 2019", 
                                                            "Dec 6, 2019",
                                                            "Feb 14, 2020")) +
  #scale_x_date(date_breaks = "1 month", date_labels="%b %y") + 
  labs(y = "Median Total Power (tons)", x = "Date", title="Median Weekly Temperature Mar 2019 - Mar 2020")

#Max Daily Total Power Mar 2019-Mar 2020
ggplot(max_df, aes(DATE, `Max Daily Total Power`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#808080")) + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
  ) +
  labs(y = "Daily Maximum Total Power (tons)", x = "Date", title="Max Daily Total Power Mar 2019 - Mar 2020")

#Chiller 4 Negative Dip
chiller_4_data %>%
  ggplot(aes(date_time, `Chiller-4 Power`, color = Negative)) + geom_point() +
  geom_line(data = chiller_4_data, aes(x = date_time, y = `Chiller-4 Power`), color = "black", group = 1)

#Chiller 4 Negative Dip One Day
chiller_4_data %>%
  ggplot(aes(date_time, `Chiller-4 Power`, color = Negative)) + geom_point(aes(alpha = as.numeric(Negative)), size = 3) +
  scale_color_manual(values = c("black","red")) +
  scale_alpha(range = c(0,1)) + 
  geom_line(data = chiller_4_data, aes(x = date_time, y = `Chiller-4 Power`), color = "black", group = 1, size = 1.05) +
  scale_x_date(limits=c(ymd("2019-7-1"), ymd("2019-7-31"))) + 
  theme(
    plot.title = element_text(size = 30, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    legend.position = "none",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_blank(),
    axis.title=element_text(size = 18)
  ) +
  labs(y = "Total Power (tons)", x = "Date", title="July 2019 Negative Total Power instances")

#Temperature Box Plots
power_temp_df %>% 
  ggplot(aes(as.factor(month(DATE)), HourlyDryBulbTemperature)) +
  geom_boxplot() + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
  ) + scale_x_discrete(labels = c("1" = month.abb[1],
                                  "2" = month.abb[2],
                                  "3" = month.abb[3],
                                  "4" = month.abb[4],
                                  "5" = month.abb[5],
                                  "6" = month.abb[6],
                                  "7" = month.abb[7],
                                  "8" = month.abb[8],
                                  "9" = month.abb[9],
                                  "10" = month.abb[10],
                                  "11" = month.abb[11],
                                  "12" = month.abb[12])) +
  labs(y = "Temperature (°F)", x = "Month", title="2019 Temperature Seasonality")

#Total Power Box Plots
power_temp_df %>% 
  ggplot(aes(as.factor(month(DATE)), `Total Power`)) +
  geom_boxplot() + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    #legend.position = "bottom",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title=element_text(size = 18)
  ) + scale_x_discrete(labels = c("1" = month.abb[1],
                                  "2" = month.abb[2],
                                  "3" = month.abb[3],
                                  "4" = month.abb[4],
                                  "5" = month.abb[5],
                                  "6" = month.abb[6],
                                  "7" = month.abb[7],
                                  "8" = month.abb[8],
                                  "9" = month.abb[9],
                                  "10" = month.abb[10],
                                  "11" = month.abb[11],
                                  "12" = month.abb[12])) +
  labs(y = "Total Power (tons)", x = "Month", title="2019 Total Power Seasonality")

#Chiller 5 Negative Dip
chiller_5_plot_df <-  uncleaned_df %>% mutate(Negative = `Chiller-5 Power` < 0)
chiller_5_plot_df %>%
  ggplot(aes(date_time, `Chiller-5 Power`, color = Negative)) + geom_point(aes(alpha = as.numeric(Negative)), size = 3) +
  scale_color_manual(values = c("black","red")) +
  scale_alpha(range = c(0,1)) + 
  geom_line(data = chiller_5_plot_df, aes(x = date_time, y = `Chiller-5 Power`), color = "black", alpha = 1, group = 1, size = 1.05) +
  scale_x_datetime(limits = c(ymd_hms("2019-5-21 6:0:0"), ymd_hms("2019-5-21 18:0:0"))) + 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    legend.position = "none",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_blank(),
    axis.title=element_text(size = 18)
  ) +
  labs(y = "Total Power (tons)", x = "Time", title="May 21, 2019 Rapid Fire Testing")
 
#Prophet Data ----
forecast_df <- read_csv("daily_median_forecast.csv")

#Weekly Seasonality
ggplot(forecast_df, aes(ds, weekly)) + geom_line(size = 1.2, color = "darkblue") + scale_x_date(breaks = c(ymd("2019-3-3"), 
                                                                             ymd("2019-3-4"), 
                                                                             ymd("2019-3-5"), 
                                                                             ymd("2019-3-6"), 
                                                                             ymd("2019-3-7"), 
                                                                             ymd("2019-3-8"), 
                                                                             ymd("2019-3-9")), 
                                                                  limits=c(ymd("2019-3-3"), ymd("2019-3-9")), 
                                                                  labels = c("Sunday",
                                                                             "Monday",
                                                                             "Tuesday",
                                                                             "Wednesday",
                                                                             "Thursday",
                                                                             "Friday",
                                                                             "Saturday"))+ 
  theme(
    plot.title = element_text(size = 35, 
                              face = "bold", 
                              #hjust = 0.5, 
                              vjust = 5,
                              color = "#383838"),
    legend.position = "none",
    #legend.title = element_blank(),
    plot.margin=margin(50,50,50,50),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey"),
    panel.grid.minor.y = element_blank(),
    axis.text = element_text(size = 14),
    legend.text = element_blank(),
    axis.title=element_text(size = 18)
  ) +
  labs(y = "Weekly Component", x = "Day of Week", title="Weekly Seasonality")

#Yearly Seasonality
forecast_df %>%
  arrange(month(ds), day(ds)) %>%
  mutate(faux_date = paste0("2019 ", month(ds), " ", day(ds))) %>%
  mutate(faux_date = ymd(faux_date)) %>%
  ggplot(aes(faux_date, yearly)) + geom_line(size = 1.2, color = "darkblue")+ 
    theme(
      plot.title = element_text(size = 35, 
                                face = "bold", 
                                #hjust = 0.5, 
                                vjust = 5,
                                color = "#383838"),
      legend.position = "none",
      #legend.title = element_blank(),
      plot.margin=margin(50,50,50,50),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(colour = "grey"),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(size = 14),
      legend.text = element_blank(),
      axis.title=element_text(size = 18)
    ) +
    labs(y = "Yearly Component", x = "Month", title="Yearly Seasonality") +
  scale_x_date(date_breaks="1 month", date_labels="%b")

#Removing Seasonality----

power_temp_fit_df <- read_csv("power_temp_model_fit.csv")

glimpse(power_temp_fit_df)

power_temp_fit_df <- power_temp_fit_df %>%
  mutate(avg_power = power_temp_df$avg_power)

power_temp_fit_df <- power_temp_fit_df %>%
  mutate(seas_rm_avg_power = avg_power - daily - yearly - weekly)

power_temp_fit_df <- power_temp_fit_df %>%
  mutate(seas_trend_rm_avg_power = seas_rm_avg_power - trend)

power_temp_fit_df <- power_temp_fit_df %>%
  mutate(seas_trend_temp_rm_avg_power = seas_trend_rm_avg_power - extra_regressors_additive)

#Looking at older data----
older_power_df <- read_csv("power_data_5yr.csv")

old_and_new_df <- rbind(older_power_df %>% select(DATE, `Total Power (avg)`),
                        full_df %>% select(DATE, `Total Power (avg)`))

#Looking at all the years of data
old_and_new_df %>%
  ggplot(aes(DATE, `Total Power (avg)`)) + geom_point()

#Comparing years
old_and_new_df %>%
  filter(DATE %within% interval(ymd("2017-01-01"), ymd("2018-01-01"))) %>%
  ggplot(aes(DATE, `Total Power (avg)`)) + geom_line()

old_and_new_df %>%
  filter(DATE %within% interval(ymd("2018-01-01"), ymd("2019-01-01"))) %>%
  ggplot(aes(DATE, `Total Power (avg)`)) + geom_line()

old_and_new_df %>%
  filter(DATE %within% interval(ymd("2019-01-01"), ymd("2020-01-01"))) %>%
  ggplot(aes(DATE, `Total Power (avg)`)) + geom_line()

#new and older data - change hourly scale to daily
complete_daily_df <- old_and_new_df
complete_daily_df$DATE <- complete_daily_df$DATE %>% as_date()
complete_daily_df <- complete_daily_df %>%
  group_by(DATE) %>%
  summarise(`Average Daily Total Power`=mean(`Total Power (avg)`))
complete_daily_df <- complete_daily_df %>% mutate(old = DATE < ymd("2019-03-01"))
            
#same but using median
complete_daily_median_df <- old_and_new_df
complete_daily_median_df$DATE <- complete_daily_median_df$DATE %>% as_date()
complete_daily_median_df <- complete_daily_median_df %>%
  group_by(DATE) %>%
  summarise(`Median Daily Total Power`=median(`Total Power (avg)`))
complete_daily_median_df <- complete_daily_median_df %>% mutate(old = DATE < ymd("2019-03-01"))

complete_daily_df %>%
  ggplot(aes(DATE, `Average Daily Total Power`, color = old)) + geom_line()

complete_daily_median_df %>%
  ggplot(aes(DATE, `Median Daily Total Power`, color = old)) +
  geom_line() +
    theme(
      plot.title = element_text(size = 35, 
                                face = "bold", 
                                #hjust = 0.5, 
                                vjust = 0,
                                color = "#383838"),
      legend.position = "none",
      #legend.title = element_blank(),
      plot.margin=margin(0,10,10,10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(colour = "grey"),
      panel.grid.minor.y = element_blank(),
      axis.text = element_text(size = 14),
      legend.text = element_blank(),
      axis.title=element_text(size = 18)
    ) +
    labs(y = "Average Daily Total Power", x = "Date", title="Old vs New Data Comparison") +
  scale_x_date(date_breaks="1 year", date_labels="%Y")
