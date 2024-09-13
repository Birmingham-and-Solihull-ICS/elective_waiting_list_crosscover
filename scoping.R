
# Connect to SQL Server.
library(tidyverse)
library(DBI)
#library(officer)
#library(rvg)

con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};server=MLCSU-BI-SQL;database=EAT_Reporting_BSOL",
                 timeout = 10)

sql <- 
  "Select AppointmentDate, ReferralRequestReceivedDate, ProviderCode, ReferrerCode,
DATEDIFF(week, ReferralRequestReceivedDate, AppointmentDate) as waiting_time_weeks
FROM EAT_Reporting_BSOL.[SUS].[VwOutpatientSUS]
Where  TreatmentSpecialtyCode = 110 -- T&O
and CCGCode = '15E00'
and AttendanceStatusCode >4 and AttendanceStatusCode <7
and FirstAttendanceCode = 1 -- first attendance
and PriorityTypeCode = 1 -- 'Routine
and OPReferralSourceCode = '03'
and AppointmentDate >= '2023-04-01' and AppointmentDate < '2024-04-01'
and ReferrerCode not like 'R%'"

dt <- dbGetQuery(con, sql)

dt %>% 
  filter(waiting_time_weeks < 120) %>% 
  ggplot(aes(x=waiting_time_weeks))+
  geom_histogram(alpha= 0.7, fill="dodgerblue2", col="black", bins = 100)
