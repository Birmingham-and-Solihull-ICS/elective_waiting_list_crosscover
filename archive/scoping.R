
# Connect to SQL Server.
library(tidyverse)
library(DBI)
library(patchwork)
library(scales)
library(zoo)
library(NHSRwaitinglist)
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
and AppointmentDate >= '2023-04-03' and AppointmentDate < '2024-04-01'
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


# Specialties

# TFC_WL_MAP 502 = Gynae,  
# 110 Tn0
# 101 Urology
# 120 ENT

sql_wl <-
  "SELECT	t2.StartOfWeekStartingSunDate, SUM(1) AS Waiters
  , ROW_NUMBER() OVER(PARTITION BY PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier ORDER BY Week_Ending_Date desc) as rowno
  
 
FROM	EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB t1
 
INNER JOIN Reference.dbo.DIM_tbDate t2
ON		t1.Week_Ending_Date = t2.Date
 
WHERE	Waiting_List_Type = 'ORTT'
AND		ACTIVITY_TREATMENT_FUNCTION_CODE in ('110', '101', '120', '502')
AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
)
AND		Week_Ending_Date >= '02-April-2023'
AND		Week_Ending_Date < '01-April-2024'
AND		SOURCE_OF_REFERRAL = '03'
 
GROUP BY t2.StartOfWeekStartingSunDate
 
ORDER BY 1 DESC"

wl <- dbGetQuery(con, sql_wl)

combine_dt <- data.frame(StartOfWeekStartingSunDate = wl$StartOfWeekStartingSunDate)

combine_dt <-
  dt %>% 
  group_by(week(removal))




# visualise

ggplot(wl, aes(x=StartOfWeekStartingSunDate, y=Waiters)) +
   geom_line() 
  # labs(
  #   title = "A growing waiting list"
  # )


# Combine 

wl_stats()


wl_stats



sql_wl2 <-
  "; with cte as
(
Select PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier,
REFERRAL_REQUEST_RECEIVED_DATE as referral, 
Outpatient_Appointment_Date as removal, 
Outcome_Of_Attendance_Description,
Last_DNA_Date,
ROW_NUMBER() OVER(PARTITION BY PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier ORDER BY Week_Ending_Date desc) as rowno

from EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
WHERE Waiting_List_Type = 'ORTT'
AND		ACTIVITY_TREATMENT_FUNCTION_CODE = '110'
AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
)
AND		SOURCE_OF_REFERRAL = '03'
)

Select *
from cte
Where rowno = 1"

wl2 <- dbGetQuery(con, sql_wl2)

wl2 <-
  wl2 %>% 
  select(referral, removal) %>% 
  mutate(referral = as.Date(referral),
         removal = as.Date(removal))

wl2_queue2 <- wl_queue_size(wl2) %>%  filter(dates > '2023-03-31')

ggplot(wl2_queue2, aes(dates, queue_size)) +
  geom_line(col="blue") 
