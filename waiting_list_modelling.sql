
drop table #CMtmp
SELECT	t2.StartOfWeekStartingSunDate, SUM(1) AS Waiters
into #CMtmp
FROM	EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB t1
 
INNER JOIN Reference.dbo.DIM_tbDate t2
ON		t1.Week_Ending_Date = t2.Date
 
WHERE	Waiting_List_Type = 'ORTT'
AND		ACTIVITY_TREATMENT_FUNCTION_CODE = '110'
AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
)
AND		Week_Ending_Date >= '02-October-2022'
AND		SOURCE_OF_REFERRAL in ('03', '3')
and Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'
 
GROUP BY t2.StartOfWeekStartingSunDate

order by 1 desc

 Select * from #CMtmp
 

 --Select * from #CMtmp
 --order by StartOfWeekStartingSunDate

 drop table #CMtmp2

SELECT	t2.StartOfWeekStartingSunDate,
t1.REFERRAL_REQUEST_RECEIVED_DATE as REFERRAL_REQUEST_RECEIVED_DATE2,
t1.Outpatient_Appointment_Date as Outpatient_Appointment_Date2,
t1.Outpatient_Future_Appointment_Date as Outpatient_Future_Appointment_Date2,
case when datediff(day, cast(REFERRAL_REQUEST_RECEIVED_DATE as datetime), cast(StartOfWeekStartingSunDate as datetime)) < 8 THEN 1 ELSE 0 end as added, -- snapshot on Sunday midnight, so 8 days Monday - Sunday inclusive, including reference date sunday.
datediff(day, cast(REFERRAL_REQUEST_RECEIVED_DATE as datetime), cast(StartOfWeekStartingSunDate as datetime)) datedif_add,
case when datediff(day, cast(StartOfWeekStartingSunDate as datetime), 
			cast(Outpatient_Appointment_Date as datetime)) 
			BETWEEN 1 and 8  THEN 1 ELSE 0 end as removed, 
case when datediff(day, cast(StartOfWeekStartingSunDate as datetime), 
			cast(coalesce(Outpatient_Appointment_Date, Outpatient_Future_appointment_Date) as datetime))
			BETWEEN 1 and 8 THEN 1 ELSE 0 end as removed_2, --doesn't seem well populated
datediff(day, cast(StartOfWeekStartingSunDate as datetime), cast(coalesce(Outpatient_Appointment_Date, Outpatient_Future_appointment_Date) as datetime)) as datedif_remove,
 ROW_NUMBER() OVER(PARTITION BY PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier ORDER BY Week_Ending_Date desc) as rowno,
 t1.*
 into #CMtmp2
FROM	EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB t1
 
INNER JOIN Reference.dbo.DIM_tbDate t2
ON		t1.Week_Ending_Date = t2.Date
 
WHERE	--PATIENT_PATHWAY_IDENTIFIER = '0004 0506 7087'
Waiting_List_Type = 'ORTT'
AND		ACTIVITY_TREATMENT_FUNCTION_CODE = '110'
AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
)
AND		Week_Ending_Date >= '02-October-2022'
AND		SOURCE_OF_REFERRAL in ('03', '3')
and Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'
--AND     TFC_WL_MAP = '502'

Select * 
from #CMtmp2
ORDER BY PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier, Week_Ending_Date desc



Select t2.StartOfWeekStartingSunDate,
t1.REFERRAL_REQUEST_RECEIVED_DATE as REFERRAL_REQUEST_RECEIVED_DATE2,
t1.Outpatient_Appointment_Date as Outpatient_Appointment_Date2,
t1.Outpatient_Future_Appointment_Date as Outpatient_Future_Appointment_Date2,* 

from EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB t1

INNER JOIN Reference.dbo.DIM_tbDate t2
ON		t1.Week_Ending_Date = t2.Date

order by t2.StartOfWeekStartingSunDate desc

--, cte2 as
--(
--SELECT	t2.StartOfWeekStartingSunDate, SUM(1) AS Waiters
 
--FROM	EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB t1
 
--INNER JOIN Reference.dbo.DIM_tbDate t2
--ON		t1.Week_Ending_Date = t2.Date
 
--WHERE	Waiting_List_Type = 'ORTT'
--AND		ACTIVITY_TREATMENT_FUNCTION_CODE = '110'
--AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
--'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
--)
--AND		Week_Ending_Date >= '02-October-2022'
--AND		SOURCE_OF_REFERRAL = '03'
 
--GROUP BY t2.StartOfWeekStartingSunDate
 
--ORDER BY 1 DESC


Select cte1.StartOfWeekStartingSunDate,
Waiters,
sum(added) as referrals,
sum(case when rowno = 1 THEN 1 ELSE 0 END ) as referrals_fistlist,
sum(removed_2) as removals

from #CMtmp2 cte1 inner join #CMtmp ON cte1.StartOfWeekStartingSunDate = #CMtmp.StartOfWeekStartingSunDate --and cte1.rowno = 1
--Where
group by cte1.StartOfWeekStartingSunDate, Waiters
order by StartOfWeekStartingSunDate


Select StartOfWeekStartingSunDate, sum(Waiters)
from #CMtmp group by StartOfWeekStartingSunDate



Select Week_Ending_Date, PATIENT_PATHWAY_IDENTIFIER, Referral_Identifier  added, removed, rowno, REFERRAL_REQUEST_RECEIVED_DATE, *
from #CMtmp2
