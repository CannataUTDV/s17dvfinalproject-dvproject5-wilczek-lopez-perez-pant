
----------------------------------------------------------------------------------------------------------------------------------------------
#crosstab1

#make into bins dominantRace.csv
--https://data.world/uscensusbureau/acs-2015-5-e-race
select ZCTA as ZipCode,
 	CASE
 	    WHEN B02008_001 >=  B02009_001 AND B02008_001 >=  B02010_001 AND B02008_001 >=  B02011_001 AND B02008_001 >=  B02012_001 AND B02008_001 >=  B02013_001 THEN "White"
 		WHEN B02009_001 >=  B02008_001 AND B02009_001 >=  B02010_001 AND B02009_001 >=  B02011_001 AND B02009_001 >=  B02012_001 AND B02009_001 >=  B02013_001 THEN "Black"
 		WHEN B02010_001 >=  B02008_001 AND B02010_001 >=  B02009_001 AND B02010_001 >=  B02011_001 AND B02010_001 >=  B02012_001 AND B02010_001 >=  B02013_001 THEN "American_Indian"
 		WHEN B02011_001 >=  B02008_001 AND B02011_001 >=  B02009_001 AND B02011_001 >=  B02010_001 AND B02011_001 >=  B02012_001 AND B02011_001 >=  B02013_001 THEN "Asian"
 		WHEN B02012_001 >=  B02008_001 AND B02012_001 >=  B02009_001 AND B02012_001 >=  B02010_001 AND B02012_001 >=  B02011_001 AND B02012_001 >=  B02013_001 THEN "Native_Hawaiian"
 		WHEN B02013_001 >=  B02008_001 AND B02013_001 >=  B02009_001 AND B02013_001 >=  B02010_001 AND B02013_001 >=  B02011_001 AND B02013_001 >=  B02012_001 THEN "Other"
 	end AS Dominant   
 from USA_ZCTA  
 where B02008_001 is not null AND B02009_001 is not null AND B02010_001 is not null AND B02011_001 is not null AND B02012_001 is not null AND B02013_001 is not null

#total: B02001_001
#white: B02008_001
#black: B02009_001
#american_indian: B02010_001
#asian: B02011_001
#native hawaiian: B02012_001
#other: B02013_001


# querying for export1.csv
select deaths_clean.`Causes of Death` as Cause_of_Death, deaths_clean.Count as Count, dominant_race.Dominant as Dominant_Race 
from deaths_clean inner join dominant_race
on deaths_clean.`ZIP Code` = dominant_race.ZipCode
where deaths_clean.Year = 2013



----------------------------------------------------------------------------------------------------------------------------------------------
#crosstab2


#make into bins median_income.csv
--https://data.world/uscensusbureau/acs-2015-5-e-income
select ZCTA as ZipCode,
	CASE
	    WHEN 0 < B19013_001 AND B19013_001 <= 20000 THEN "A: 0-20k"
	    WHEN 20000 < B19013_001 AND B19013_001 <= 40000 THEN "B: 20-40k"
	    WHEN 40000 < B19013_001 AND B19013_001 <= 60000 THEN "C: 40-60k"
	    WHEN 60000 < B19013_001 AND B19013_001 <= 80000 THEN "D: 60-80k"
	    WHEN 80000 < B19013_001 AND B19013_001 <= 100000 THEN "E: 80-100k"
	    WHEN 100000 < B19013_001 AND B19013_001 <= 120000 THEN "F: 100-120k"
	    WHEN 120000 < B19013_001 AND B19013_001 <= 140000 THEN "G: 120-140k"
	    WHEN 140000 < B19013_001 AND B19013_001 <= 160000 THEN "H: 140-160k"
	    WHEN 160000 < B19013_001 AND B19013_001 <= 180000 THEN "I: 160-180k"
	    WHEN 180000 < B19013_001 AND B19013_001 <= 200000 THEN "J: 180-200k"
	    WHEN 200000 < B19013_001 AND B19013_001 <= 220000 THEN "J: 200-220k"
	    WHEN 220000 < B19013_001 AND B19013_001 <= 240000 THEN "K: 220-240k"
	    WHEN 240000 < B19013_001 AND B19013_001 <= 260000 THEN "L: 240-260k"
	end AS MedianIncome
from USA_ZCTA
where B19013_001 is not null


# querying for export2.csv
--https://data.world/lnl445/s-17-dv-project-5
select deaths_clean.`Causes of Death` as Cause_of_Death, deaths_clean.Count as Count, median_income.MedianIncome as Median_Income 
from deaths_clean inner join median_income
on deaths_clean.`ZIP Code` = median_income.ZipCode
where deaths_clean.Year = 2013



----------------------------------------------------------------------------------------------------------------------------------------------
#crosstab3

#make into bins median_age.csv
--https://data.world/uscensusbureau/acs-2015-5-e-agesex
select ZCTA as ZipCode,
	CASE
	    WHEN 0 < B01002_001 AND B01002_001 <= 10 THEN "A: 0-10"
	    WHEN 10 < B01002_001 AND B01002_001 <= 20 THEN "B: 10-20"
	    WHEN 20 < B01002_001 AND B01002_001 <= 30 THEN "C: 20-30"
	    WHEN 30 < B01002_001 AND B01002_001 <= 40 THEN "D: 30-40"
	    WHEN 40 < B01002_001 AND B01002_001 <= 50 THEN "E: 40-50"
	    WHEN 50 < B01002_001 AND B01002_001 <= 60 THEN "F: 50-60"
	    WHEN 60 < B01002_001 AND B01002_001 <= 70 THEN "G: 60-70"
	    WHEN 70 < B01002_001 AND B01002_001 <= 80 THEN "H: 70-80"
	    WHEN 80 < B01002_001 AND B01002_001 <= 90 THEN "I: 80-90"
	    WHEN 90 < B01002_001 AND B01002_001 <= 100 THEN "J: 90-100"
	end AS MedianAge
from USA_ZCTA
where B01002_001 is not null


# querying for export3.csv
--https://data.world/lnl445/s-17-dv-project-5
select deaths_clean.`Causes of Death` as Cause_of_Death, deaths_clean.Count as Count, median_age.MedianAge as Median_Age 
from deaths_clean inner join median_age
on deaths_clean.`ZIP Code` = median_age.ZipCode
where deaths_clean.Year = 2013


----------------------------------------------------------------------------------------------------------------------------------------------
#for finding ranges in ui.r
select min(count) as min, avg(count) as avg, max(count) as max
from export1