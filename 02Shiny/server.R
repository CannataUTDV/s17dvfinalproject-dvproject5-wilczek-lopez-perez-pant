# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

# Server.R structure:
#   Queries that don’t need to be redone
#   shinyServer
#   widgets
#   tab specific queries and plotting

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
# if(online0) {
#   regions = query(
#     data.world(propsfile = "www/.data.world"),
#     dataset="tommywilczek/s-17-dv-final-project", type="sql",
#     query="select distinct dominant_race.Dominant as D, dominant_race.Dominant as R
#     from dominant_race
#     order by 1"
#   ) # %>% View()
# }
# region_list <- as.list(regions$D, regions$R)
# region_list <- append(list("All" = "All"), region_list)
# region_list5 <- region_list

# The following queries are for the Barcharts -> High Discount Orders tab data.
if(online0) {
# Step 1:
  highDiscounts <- query(
  data.world(propsfile = "www/.data.world"),
  dataset="cannata/superstoreorders", type="sql",
  query="
  SELECT distinct Order_Id, sum(Discount) as sumDiscount, sum(Sales) as
  sumSales
  FROM SuperStoreOrders
  where Region != 'International'
  group by Order_Id
  having sum(Discount) >= .3"
) # %>% View()
  # View(highDiscounts )

# Step 2
  highDiscountCustomers <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="
    SELECT distinct Customer_Name, City, State, Order_Id
    FROM SuperStoreOrders
    where Order_Id in
    (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    order by Order_Id",
    queryParameters = highDiscounts$Order_Id
  ) # %>% View()
    # View(highDiscountCustomers)
    
# Step 3
    stateAbreviations <- query(
      data.world(propsfile = "www/.data.world"),
      dataset="cannata/superstoreorders", type="sql",
      query="SELECT distinct name as State, abbreviation as Abbreviation
      FROM markmarkoh.`us-state-table`.`state_table.csv/state_table`
      where name in
      (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      order by name",
      queryParameters = highDiscountCustomers$State
    ) # %>% View()
    # View(stateAbreviations )
    
# Step 4
    highDiscountCustomers2 <- left_join(highDiscountCustomers,
                                        stateAbreviations, by="State") # %>% View()
    # View(highDiscountCustomers2)
    
# Step 5
    longLat <- query(
      data.world(propsfile = "www/.data.world"),
      dataset="cannata/superstoreorders", type="sql",
      query="SELECT distinct NAME as City, STATE as Abbreviation,
      LATITUDE AS Latitude,
      LONGITUDE AS Longitude
      FROM bryon.`dhs-city-location-example`.`towns.csv/towns`
      where NAME in
      (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      order by NAME",
      queryParameters = highDiscountCustomers$City
    ) # %>% View()
    # View(longLat)
    
# Step 6
    highDiscountCustomers2LongLat <- 
      inner_join(highDiscountCustomers2, longLat, by = c("City", "Abbreviation")) 
    # View(highDiscountCustomers2LongLat)
    
# Step 7
    discounts <- 
      inner_join(highDiscountCustomers2LongLat, highDiscounts, by="Order_Id") # %>% View()
    # View(discounts)

  OLDdiscounts <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="SELECT Customer_Name as CustomerName, s.City as City, states.abbreviation as State, 
    c.LATITUDE AS Latitude, 
    c.LONGITUDE AS Longitude, 
    Order_Id as OrderId, sum(Discount) as sumDiscount
    FROM SuperStoreOrders s join markmarkoh.`us-state-table`.`state_table.csv/state_table` states
    ON (s.State = states.name AND s.City = c.NAME) join
    bryon.`dhs-city-location-example`.`towns.csv/towns` c 
    ON (states.abbreviation = c.STATE)
    WHERE Region != 'International'
    group by Customer_Name, s.City, states.abbreviation, c.LATITUDE, c.LONGITUDE, Order_Id -- Note the absence of LATITUDE and LONGITUDE
    having sum(Discount) between .3 and .9"
  )  # %>% View()
} else {
  # Just faking one data point for now.
  Customer_Name = 'Wesley Tate'
  City = 'Chicago'
  State = 'Illinois'
  Order_Id = 48452
  Abbreviation = 'IL'
  Latitude =  41.85003
  Longitude = -87.65005
  sumDiscount = 0.34
  sumSales = 7124
  discounts <- data.frame(Customer_Name, City, State, Order_Id, Abbreviation, Latitude, Longitude, sumDiscount, sumSales)
}

# The following query is for the Barcharts -> High Sales Customers tab data.
if(online0) {
  # Step 1:
  highDiscounts <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="
    SELECT distinct Order_Id, sum(Discount) as sumDiscount
    FROM SuperStoreOrders
    group by Order_Id
    having sum(Discount) >= .3"
  ) # %>% View()
  # View(highDiscounts)
  
  # Step 2
  sales <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="cannata/superstoreorders", type="sql",
    query="
    select Customer_Id, sum(Profit) as sumProfit
    FROM SuperStoreOrders
    where Order_Id in 
      (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    group by Customer_Id",
    queryParameters = highDiscounts$Order_Id
    ) # %>% View()
  # View(sales)
} else {
  print("Getting discounts from csv")
  file_path = "www/SuperStoreOrders.csv"
  df <- readr::read_csv(file_path) 
  # Step 1
  highDiscounts <- df %>% dplyr::group_by(Order_Id) %>% dplyr::summarize(sumDiscount = sum(Discount)) %>% dplyr::filter(sumDiscount >= .3)
  # View(highDiscounts)
  # Step 2
  sales <- df %>% dplyr::filter(Order_Id %in% highDiscounts$Order_Id) %>% dplyr::select(Customer_Name, Customer_Id, City, State, Order_Id, Profit) %>% dplyr::group_by(Customer_Name, Customer_Id, City, State, Order_Id) %>% dplyr::summarise(sumProfit = sum(Profit))
  # View(sales)
}

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$boxplotRegions <- renderUI({selectInput("selectedBoxplotRegions", "Choose Races:",
                                                 region_list5, multiple = TRUE, selected='All') })
  
  # These widgets are for the Histogram tab.
  online4 = reactive({input$rb4})
  
  # These widgets are for the Scatter Plots tab.
  online3 = reactive({input$rb3})
  
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$regions2 <- renderUI({selectInput("selectedRegions", "Choose Regions:", region_list, multiple = TRUE, selected='All') })
  
  # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- eventReactive(input$click5, {
    # if(input$selectedBoxplotRegions == 'All') region_list5 <- input$selectedBoxplotRegions
    # else region_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotRegions)
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="tommywilczek/s-17-dv-final-project", type="sql",
        query="select deaths_clean.`Causes of Death` as causeof, Median_Income_Raw.`Median Household Income` as medianof, sum(deaths_clean.Count) as sum_count, dominant_race.Dominant as dominant
        from deaths_clean inner join Median_Income_Raw on deaths_clean.`ZIP Code` = Median_Income_Raw.Zipcode inner join dominant_race on Median_Income_Raw.Zipcode = dominant_race.ZipCode
        group by deaths_clean.`Causes of Death`, Median_Income_Raw.`Median Household Income`, dominant_race.Dominant")
    }
    })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                extensions = list(Responsive = TRUE, 
                                                FixedHeader = TRUE)
  )
  })
  
  dfbp2 <- eventReactive(c(input$click5, input$boxSalesRange1), {
    dfbp1() %>% dplyr::filter(sum_count >= input$boxSalesRange1[1] & sum_count <= input$boxSalesRange1[2]) # %>% View()
  })
  
  # dfbp3 <- eventReactive(c(input$click5, input$range5a), {
  #   dfbp2() %>% dplyr::arrange(desc(medianof)) # %>% View()
  # })
    
  output$boxplotPlot1 <- renderPlotly({
    #View(dfbp3())
    p <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=causeof, y=sum_count, colour=dominant)) + 
      ylim(0, input$boxSalesRange1[2]) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
  
  # Begin Histgram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click4, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="tommywilczek/s-17-dv-final-project", type="sql",
        query="select deaths_clean.`Causes of Death` as causeof, 
        `Median_Age_Raw.csv/Median_Age_Raw`.`Median Age` as medianof, 
        (sum(deaths_clean.Count)/(count(distinct(deaths_clean.`ZIP Code`)))) as Weighted_sum_count 
        from deaths_clean inner join Median_Age_Raw on deaths_clean.`ZIP Code` = `Median_Age_Raw.csv/Median_Age_Raw`.`Zip Code`
        group by deaths_clean.`Causes of Death`, `Median_Age_Raw.csv/Median_Age_Raw`.`Median Age`"
      ) # %>% View()
    }
    
  })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                          extensions = list(Responsive = TRUE, 
                                                                            FixedHeader = TRUE)
  )
  })
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(dfh1()) +
    geom_histogram(aes(x= medianof)) +
    theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
  ggplotly(p)
  })
  # End Histogram Tab ___________________________________________________________
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="tommywilczek/s-17-dv-final-project", type="sql",
        query="select deaths_clean.`Causes of Death` as causeof, Median_Age_Raw.`Median Age` as medianof, sum(deaths_clean.Count) as sum_count
        from deaths_clean inner join Median_Age_Raw on deaths_clean.`ZIP Code` = Median_Age_Raw.`Zip Code`
        group by deaths_clean.`Causes of Death`, Median_Age_Raw.`Median Age`"
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/SuperStoreOrders.csv"
      df <- readr::read_csv(file_path)
      df %>% dplyr::select(Sales, Profit, State) %>% dplyr::filter(State == 'Texas' | State == 'Florida') # %>% View()
    }
    })
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(), rownames = FALSE,
                                                        extensions = list(Responsive = TRUE, 
                                                                          FixedHeader = TRUE)
  )
  })
  output$scatterPlot1 <- renderPlotly({p <- ggplot(dfsc1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=3)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_point(aes(x=medianof, y=sum_count, colour=causeof), size=2)
  ggplotly(p)
  })
  # End Scatter Plots Tab ___________________________________________________________
  
  # Begin Crosstab Tab ------------------------------------------------------------------
  dfct1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="tommywilczek/s-17-dv-final-project", type="sql",
        query="
        select `Crosstab.csv/Crosstab`.`Dominant Race` as `Race`, 
        `Crosstab.csv/Crosstab`.`Causes of Death` as Cause_of_Death, 
        sum(`Crosstab.csv/Crosstab`.`Death Count`) as Number_of_Deaths,
        
        sum(`Crosstab.csv/Crosstab`.`Death Count`)/count(`ZipCodeTotalDeathCount.csv/ZipCodeTotalDeathCount`.`ZIP Code`) as kpiRatio,
        
        case
        when `Crosstab.csv/Crosstab`.`Death Count`/`ZipCodeTotalDeathCount.csv/ZipCodeTotalDeathCount`.sum_death_count < ? then '03 Low'
        when `Crosstab.csv/Crosstab`.`Death Count`/`ZipCodeTotalDeathCount.csv/ZipCodeTotalDeathCount`.sum_death_count < ? then '02 Medium'
        else '01 High'
        end AS kpi_level
        
        from `Crosstab.csv/Crosstab`
        
        inner join `ZipCodeTotalDeathCount.csv/ZipCodeTotalDeathCount` on `Crosstab.csv/Crosstab`.ZipCode = `ZipCodeTotalDeathCount.csv/ZipCodeTotalDeathCount`.`ZIP Code`
        group by `Crosstab.csv/Crosstab`.`Dominant Race`, `Crosstab.csv/Crosstab`.`Causes of Death`
        ",
        queryParameters = list(KPI_Low(), KPI_Medium())
      ) # %>% View()
    }
    
    
  })
  output$data1 <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(dfct1()) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_text(aes(x=Race, y=Cause_of_Death, label=Number_of_Deaths), size=6) +
      geom_tile(aes(x=Race, y=Cause_of_Death, fill=kpi_level), alpha=0.50)
  })
  # End Crosstab Tab ___________________________________________________________
# Begin Barchart Tab ------------------------------------------------------------------
  dfc1 <- eventReactive(input$click2, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="tommywilczek/s-17-dv-final-project", type="sql",
        query="select deaths_clean.`Causes of Death` as causeof, 
        `median_age.csv/median_age`.MedianAge as medianof, 
        (sum(deaths_clean.Count)/(count(distinct(deaths_clean.`ZIP Code`)))) as Weighted_sum_count, 
        deaths_clean.`ZIP Code` as ZIPcode
        from deaths_clean inner join `median_age.csv/median_age` on deaths_clean.`ZIP Code` = `median_age.csv/median_age`.ZipCode
        group by deaths_clean.`Causes of Death`, `median_age.csv/median_age`.MedianAge"
      ) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(medianof) %>% summarize(window_countof= mean(Weighted_sum_count))
    dplyr::inner_join(tdf, tdf2, by = "medianof")
    
  })
  output$barchartData1 <- renderDataTable({DT::datatable(dfc1(), rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$barchartData2 <- renderDataTable({DT::datatable(discounts,
                                                         rownames = FALSE,
                                                         extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })

  output$barchartPlot1 <- renderPlot({ggplot(dfc1(), aes(x=causeof, y=Weighted_sum_count)) +
      scale_y_discrete(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~medianof, ncol=1) + 
      coord_flip() + 
      # Add sum_sales - window_avg_sales label.
      geom_text(mapping=aes(x=causeof, y=Weighted_sum_count, label=round(Weighted_sum_count)),colour="red", hjust=-.5) +
      # Add reference line.
      geom_hline(aes(yintercept = round(window_countof)), color="red")
  })
  
  thismap = query(
    data.world(propsfile = "www/.data.world"),
    dataset="tommywilczek/s-17-dv-final-project", type="sql",
    query="
    select `Crosstab.csv/Crosstab`.`Death Count` as deathCount, 
    `Crosstab.csv/Crosstab`.`Causes of Death` as cause_of_death, 
    `Crosstab.csv/Crosstab`.ZipCode as zipcode, 
    `zipLatLong.csv/zipLatLong`.latitude as latitude, 
    `zipLatLong.csv/zipLatLong`.longitude as longitude
    
    from `Crosstab.csv/Crosstab`
    
    inner join `zipLatLong.csv/zipLatLong` on `Crosstab.csv/Crosstab`.ZipCode = `zipLatLong.csv/zipLatLong`.zipcode
    where `Crosstab.csv/Crosstab`.`Death Count` > 1000 and `Crosstab.csv/Crosstab`.`Causes of Death` = 'CAN'
    ")  

  output$barchartMap1 <- renderLeaflet({leaflet(width = 400, height = 800) %>% 
      setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
      addTiles() %>% 
      addProviderTiles("MapQuestOpen.Aerial") %>%
      addMarkers(lng = discounts$Longitude,
                 lat = discounts$Latitude,
                 options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
                 popup = as.character(paste(discounts$Customer_Name, 
                                            ", ", discounts$City,
                                            ", ", discounts$State,
                                            " Sales: ","$", formatC(as.numeric(discounts$sumSales), format="f", digits=2, big.mark=","),
                                            " Discount: ", ", ", discounts$sumDiscount)) )
  })

  getColor <- function(thismap) {
    sapply(thismap$deathCount, function(deathCount) {
      if(deathCount <= 1500) {
        "green"
      } else if(deathCount <= 2000) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(thismap)
  )
  
  output$barchartMap1 <- renderLeaflet({leaflet(data = thismap, width = 400, height = 800) %>% 
      setView(lng = -119.9, lat = 37.2, zoom = 5) %>% 
      addTiles() %>% 
      #addProviderTiles("MapQuestOpen.Aerial") %>%
      addAwesomeMarkers( lng = thismap$longitude, lat = thismap$latitude, icon=icons,
                         options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
                         popup = as.character(paste( "Zip:", thismap$zipcode, hr(), 
                                                     "Cause of Death:", thismap$cause_of_death,hr(),
                                                     "Death Count:", thismap$deathCount
                                                     
                         )) )
  })

  # End Barchart Tab ___________________________________________________________
  # MAPS ------------------------------------------------------------------------------
  # dfbc1 <- eventReactive(input$click2, {
  #   if(input$selectedRegions == 'All') region_list <- input$selectedRegions
  #   else region_list <- append(list("Skip" = "Skip"), input$selectedRegions)
  #   if(online2() == "SQL") {
  #     print("Getting from data.world")
  #     tdf = query(
  #       data.world(propsfile = "www/.data.world"),
  #       dataset="tommywilczek/s-17-dv-final-project", type="sql",
  #       query="
  #       select `Crosstab.csv/Crosstab`.`Death Count` as deathCount, 
  #       `Crosstab.csv/Crosstab`.`Causes of Death` as cause_of_death, 
  #       `Crosstab.csv/Crosstab`.ZipCode as zipcode, 
  #       `zipLatLong.csv/zipLatLong`.latitude as latitude, 
  #       `zipLatLong.csv/zipLatLong`.longitude as longitude
  #       
  #       from `Crosstab.csv/Crosstab`
  #       
  #       inner join `zipLatLong.csv/zipLatLong` on `Crosstab.csv/Crosstab`.ZipCode = `zipLatLong.csv/zipLatLong`.zipcode
  #       ",
  #       queryParameters = region_list
  #     ) # %>% View()
  #   }
  #   
  #   
  # })
  # 
  # thismap = query(
  #   data.world(propsfile = "www/.data.world"),
  #   dataset="tommywilczek/s-17-dv-final-project", type="sql",
  #   query="
  #   select `Crosstab.csv/Crosstab`.`Death Count` as deathCount, 
  #   `Crosstab.csv/Crosstab`.`Causes of Death` as cause_of_death, 
  #   `Crosstab.csv/Crosstab`.ZipCode as zipcode, 
  #   `zipLatLong.csv/zipLatLong`.latitude as latitude, 
  #   `zipLatLong.csv/zipLatLong`.longitude as longitude
  #   
  #   from `Crosstab.csv/Crosstab`
  #   
  #   inner join `zipLatLong.csv/zipLatLong` on `Crosstab.csv/Crosstab`.ZipCode = `zipLatLong.csv/zipLatLong`.zipcode
  #   where `Crosstab.csv/Crosstab`.`Death Count` > 1000 and `Crosstab.csv/Crosstab`.`Causes of Death` = 'CAN'
  #   ")
  # 
  # output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
  #                                                        rownames = FALSE,
  #                                                        extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  # })
  # 
  # 
  # getColor <- function(thismap) {
  #   sapply(thismap$deathCount, function(deathCount) {
  #     if(deathCount <= 1500) {
  #       "green"
  #     } else if(deathCount <= 2000) {
  #       "orange"
  #     } else {
  #       "red"
  #     } })
  # }
  # 
  # icons <- awesomeIcons(
  #   icon = 'ios-close',
  #   iconColor = 'black',
  #   library = 'ion',
  #   markerColor = getColor(thismap)
  # )
  # 
  # output$barchartMap1 <- renderLeaflet({leaflet(data = thismap, width = 400, height = 800) %>% 
  #     setView(lng = -119.9, lat = 37.2, zoom = 5) %>% 
  #     addTiles() %>% 
  #     #addProviderTiles("MapQuestOpen.Aerial") %>%
  #     addAwesomeMarkers( lng = thismap$longitude, lat = thismap$latitude, icon=icons,
  #                        options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
  #                        popup = as.character(paste( "Zip:", thismap$zipcode, hr(), 
  #                                                    "Cause of Death:", thismap$cause_of_death,hr(),
  #                                                    "Death Count:", thismap$deathCount
  #                                                    
  #                        )) )
  # })
  
  
})
