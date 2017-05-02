require(readr)
require(lubridate)
require(dplyr)
require(data.world)

online0 = TRUE

if(online0) {
  globals = query(
    data.world(propsfile = "www/.data.world"),
    dataset="tommywilczek/s-17-dv-final-project", type="sql",
    query="select deaths_clean.`Causes of Death` as causeof, Median_Income_Raw.`Median Household Income` as median_income, sum(deaths_clean.Count) as sum_count, dominant_race.Dominant as dominant
        from deaths_clean inner join Median_Income_Raw on deaths_clean.`ZIP Code` = Median_Income_Raw.Zipcode inner join dominant_race on Median_Income_Raw.Zipcode = dominant_race.ZipCode
        group by deaths_clean.`Causes of Death`, Median_Income_Raw.`Median Household Income`, dominant_race.Dominant
        order by 1"
  ) 
}