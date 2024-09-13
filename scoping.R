
# Connect to SQL Server.
library(tidyverse)
library(DBI)
library(patchwork)
library(scales)
library(zoo)
#library(officer)
#library(rvg)

con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=MLCSU-BI-SQL;database=EAT_Reporting_BSOL",
                 timeout = 10)

sql <- 
  "Select AppointmentDate as removal, ReferralRequestReceivedDate as referral, ProviderCode, ReferrerCode,
DATEDIFF(week, ReferralRequestReceivedDate, AppointmentDate) as waiting_time_weeks
FROM EAT_Reporting_BSOL.[SUS].[VwOutpatientSUS]
Where  TreatmentSpecialtyCode = 110 -- T&O
and CCGCode = '15E00'
and AttendanceStatusCode >4 and AttendanceStatusCode <7
and FirstAttendanceCode = 1 -- first attendance
and PriorityTypeCode = 1 -- 'Routine
and OPReferralSourceCode = '03'
and AppointmentDate >= '2020-04-01' and AppointmentDate < '2024-04-01'
and ReferrerCode not like 'R%'
and AppointmentDate is not null and ReferralRequestReceivedDate is not null"

dt <- dbGetQuery(con, sql)
#--18,556 without null catches 18,555 with NULL catch

theme_set(
  theme_minimal()+
  theme(plot.title = element_text(size=12),
        plot.subtitle = element_text(size=9, face="italic")
  )
)

outliers <-
  dt %>% 
  filter(waiting_time_weeks > 104) %>% 
  group_by(waiting_time_weeks) %>% 
  count()

# Percent outliers > 2 years 
sum(outliers$n) /
dt %>% 
  #filter(waiting_time_weeks > 104) %>% 
  #group_by(waiting_time_weeks) %>% 
  count() %>%  pull()

# 0.8%

a<- dt %>% 
  #filter(waiting_time_weeks < 120) %>% 
  ggplot(aes(x=waiting_time_weeks))+
  geom_histogram(alpha= 0.7, fill="dodgerblue2", binwidth=2)+
  scale_y_continuous(label=comma)+
  scale_x_continuous("Waiting time (weeks)")+
  labs(title = "BSOL TnO GP -> First Consultant OP",
       subtitle = "Unrestricted")

b<- dt %>% 
  filter(waiting_time_weeks < 104) %>% 
  ggplot(aes(x=waiting_time_weeks))+
  geom_histogram(alpha= 0.7, fill="darkolivegreen2", col="black", binwidth=2)+
  scale_y_continuous(label=comma)+
  scale_x_continuous("Waiting time (weeks)")+
  labs(title = "Restricted BSOL TnO GP -> First Consultant OP",
       subtitle = "Waiting times > 104 excluded")

a + b





##########################
# WL section
############################


wl_queue_size(dt)

# visualise queue
ggplot(wl_queue_size(dt), aes(dates, queue_size)) +
  geom_line() 


# visualise queue
dt %>% 
  select(removal, referral) %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(yearmon = as.yearmon(value)) %>% 
  group_by(name, yearmon) %>% 
  count() %>% 
  filter(year(yearmon) > 2021 ) %>% 
  ggplot(aes(x=yearmon, y=n, col=name)) +
  geom_line() +
  labs(
    title = "A growing waiting list"
  )


referral_stats <- wl_referral_stats(dt)
referral_stats


removal_stats <- wl_removal_stats(dt)
removal_stats
# If we want the average wait to be within a tar