/*
PathwayID / key	 ||Date added	|| Date removed

WHERE	Waiting_List_Type = 'ORTT'
AND		ACTIVITY_TREATMENT_FUNCTION_CODE = '110'
AND		[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN (
'04X','04X00','05P','05P00','13P','13P00','15E','15E00','QHL','QHL00'
)
AND		Week_Ending_Date >= '02-October-2022'
AND		SOURCE_OF_REFERRAL in ('03', '3')
and Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'

*/
--Declarations for insertion
declare			@WeDate date
,				@TreatmentSpec varchar(3)

Set				@WeDate='02-October-2022'
Set				@TreatmentSpec='110'

--remove temp table if already in existence
drop table if exists ##BSOLtempChrisM;
--create table for analysis
create table ##BSOLtempChrisM (
							PathwayIdentifier		varchar(76)
,							ReferralIdentifier		varchar(76)
,							Date_Added				date
,							Date_Removed			date
,							[FirstWE_PreRemove]	date
							)

--Insert a couple of indexes to help on joins and insertion
create clustered index cl_idx_PatRefID on ##BSOLtempChrisM (PathwayIdentifier,ReferralIdentifier)
create nonclustered index ncl_add on ##BSOLtempChrisM(Date_Added asc)
create nonclustered index ncl_rem on ##BSOLtempChrisM(Date_Removed asc)

--Put Unique ID's in to the temp table....
Insert into ##BSOLtempChrisM
(
			PathwayIdentifier
,			ReferralIdentifier
)

(
select		PATIENT_PATHWAY_IDENTIFIER
,			Referral_Identifier
 from		EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
where		1=1
AND			Waiting_List_Type = 'ORTT'
AND			ACTIVITY_TREATMENT_FUNCTION_CODE = @TreatmentSpec
AND			[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN ( '04X'
,																  '04X00'
,																  '05P'
,																  '05P00'
,																  '13P'
,															      '13P00'
,																  '15E'
,																  '15E00'
,																  'QHL'
,																  'QHL00'
																)
AND		Week_Ending_Date >= @WeDate
AND		SOURCE_OF_REFERRAL in ('03', '3') --GP referral
and		Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'
group by 	PATIENT_PATHWAY_IDENTIFIER
,			Referral_Identifier
)

--puts in now the first week_ending date that this record exists in the pathway selected in the where clauses...
update		t1
set			t1.Date_Added=t2.Date_Added
from		##BSOLtempChrisM t1
left join (	select		min(week_ending_date) as Date_Added
,						Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER
			from 		EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
			where		1=1
			AND			Waiting_List_Type = 'ORTT'
			AND			ACTIVITY_TREATMENT_FUNCTION_CODE = @TreatmentSpec
			AND			[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN ( '04X'
,																			  '04X00'
,																			  '05P'
,																			  '05P00'
,																			  '13P'
,																		      '13P00'
,																			  '15E'
,																			  '15E00'
,																			  'QHL'
,																			  'QHL00'
																			)
AND						Week_Ending_Date >= @WeDate
AND						SOURCE_OF_REFERRAL in ('03', '3')
and						Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'

			group by	Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER
) t2

on			t1.PathwayIdentifier=t2.PATIENT_PATHWAY_IDENTIFIER
where		t1.ReferralIdentifier=t2.Referral_Identifier


--Update date removed 
update		t1
set			t1.Date_Removed=t2.Date_Removed
from		##BSOLtempChrisM t1
left join (	select		Date_Last_Attended as Date_Removed
,						Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER
			from 		EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
			where		1=1
			AND			Waiting_List_Type = 'ORTT'
			AND			ACTIVITY_TREATMENT_FUNCTION_CODE = @TreatmentSpec
			AND			[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN ( '04X'
,																			  '04X00'
,																			  '05P'
,																			  '05P00'
,																			  '13P'
,																		      '13P00'
,																			  '15E'
,																			  '15E00'
,																			  'QHL'
,																			  'QHL00'
																			)
			AND			Week_Ending_Date >= @WeDate
			AND			SOURCE_OF_REFERRAL in ('03', '3')
			and			Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'
			and			Date_Last_Attended is not null
			group by	Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER
,						Date_Last_Attended

) t2
on			t1.PathwayIdentifier=t2.PATIENT_PATHWAY_IDENTIFIER
where		t1.ReferralIdentifier=t2.Referral_Identifier


---------------------------------------------------------------------------------------------------------------------------------------------

update		t1
set			t1.FirstWE_PreRemove=t2.thedate
from		##BSOLtempChrisM t1
left join (	select		MAX(Week_Ending_Date) as thedate
,						Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER
			from 		EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
			where		1=1
			AND			Waiting_List_Type = 'ORTT'
			AND			ACTIVITY_TREATMENT_FUNCTION_CODE = @TreatmentSpec
			AND			[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN ( '04X'
,																			  '04X00'
,																			  '05P'
,																			  '05P00'
,																			  '13P'
,																		      '13P00'
,																			  '15E'
,																			  '15E00'
,																			  'QHL'
,																			  'QHL00'
																			)
			AND			Week_Ending_Date >= @WeDate
			AND			SOURCE_OF_REFERRAL in ('03', '3')
			and			Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'
			group by	Referral_Identifier
,						PATIENT_PATHWAY_IDENTIFIER

) t2
on			t1.PathwayIdentifier=t2.PATIENT_PATHWAY_IDENTIFIER
where		t1.ReferralIdentifier=t2.Referral_Identifier
and			t1.Date_Removed<=t2.thedate



drop table if exists ##BSOLtempChrisM_WeekEnding;
create table  ##BSOLtempChrisM_WeekEnding
(
Week_Ending_Date date
,Pathways_Quantity int
)

insert into  ##BSOLtempChrisM_WeekEnding (
Week_Ending_Date
,Pathways_Quantity 
)
(
select Week_ending_date
,COUNT(coalesce(PATIENT_PATHWAY_IDENTIFIER,Referral_Identifier)) as Pathways_Quantity
from EAT_Reporting_BSOL.Development.WEEKLYWAITINGLIST_JB
where		1=1
			AND			Waiting_List_Type = 'ORTT'
			AND			ACTIVITY_TREATMENT_FUNCTION_CODE =@TreatmentSpec
			AND			[ORGANISATION_IDENTIFIER_(CODE_OF_COMMISSIONER)] IN ( '04X'
,																			  '04X00'
,																			  '05P'
,																			  '05P00'
,																			  '13P'
,																		      '13P00'
,																			  '15E'
,																			  '15E00'
,																			  'QHL'
,																			  'QHL00'
																			)
			AND		Week_Ending_Date >= '02-oct-2022'--@WeDate
			AND		SOURCE_OF_REFERRAL in ('03', '3')
			and		Referral_To_Treatment_Period_Status_Description = 'First activity - first activity in a referral to treatment period'

group by Week_ending_date
)

select *, datename(weekday,Week_Ending_Date) as thedow from ##BSOLtempChrisM_WeekEnding
order by 1
