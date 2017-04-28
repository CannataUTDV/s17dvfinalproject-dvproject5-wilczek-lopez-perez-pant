# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

shinyServer(function(input, output) { 
  online1 = reactive({input$rb1})
  online2 = reactive({input$rb2})
  online3 = reactive({input$rb3})
  KPI_Low = reactive({input$KPI1})     
  KPI_Medium = reactive({input$KPI2})
  # Begin Barchart Tab1 ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="bap2323/s-17-dv-project-6", type="sql",
        query="select rows.`Causes of Death` as causeof, median_age.MedianAge as medianof, sum(rows.Count) as sum_count
        from rows inner join median_age on rows.`ZIP Code` = median_age.ZipCode
        group by rows.`Causes of Death`, median_age.MedianAge"
      ) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(medianof) %>% summarize(window_countof= mean(sum_count))
    dplyr::inner_join(tdf, tdf2, by = "medianof")
    
    })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1(), aes(x=causeof, y=sum_count)) +
      scale_y_discrete(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~medianof, ncol=1) + 
      coord_flip() + 
      # Add sum_sales - window_avg_sales label.
      geom_text(mapping=aes(x=causeof, y=sum_count, label=round(sum_count - window_countof)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=causeof, y=sum_count, label=round(sum_count)),colour="blue", hjust=-5)+ 
      # Add reference line.
      geom_hline(aes(yintercept = window_countof), color="red")+
      geom_text(aes( -1, window_countof, label = window_countof, vjust = -.5, hjust = -.25), color="red")
  })
  # End Barchart Tab1 ___________________________________________________________

# Begin Barchart Tab2 ------------------------------------------------------------------
  df2 <- eventReactive(input$click2, {
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="bap2323/s-17-dv-project-6", type="sql",
        query="select rows.`Causes of Death` as causeof, dominant_race.Dominant as dominantof, sum(rows.Count) as sum_count
from rows inner join dominant_race on rows.`ZIP Code` = dominant_race.ZipCode
        group by rows.`Causes of Death`, dominant_race.Dominant"
      ) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(dominantof) %>% summarize(window_countof= mean(sum_count))
    dplyr::inner_join(tdf, tdf2, by = "dominantof")

  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlot({ggplot(df2(), aes(x=causeof, y=sum_count)) +
      scale_y_discrete(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~dominantof, ncol=1) + 
      coord_flip() + 
      # Add sum_sales - window_avg_sales label.
      geom_text(mapping=aes(x=causeof, y=sum_count, label=round(sum_count - window_countof)),colour="red", hjust=-.5) +
      # Add reference line.
      geom_hline(aes(yintercept = window_countof), color="red")
  })
  # End Barchart Tab2 ___________________________________________________________
  
  # Begin Barchart Tab3 ------------------------------------------------------------------
  df3 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="bap2323/s-17-dv-project-6", type="sql",
        query="select rows.`Causes of Death` as causeof, median_income.MedianIncome as medianof, sum(rows.Count) as sum_count
        from rows inner join median_income on rows.`ZIP Code` = median_income.ZipCode
        group by rows.`Causes of Death`, median_income.MedianIncome"
      ) # %>% View()
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(medianof) %>% summarize(window_countof= mean(sum_count))
    dplyr::inner_join(tdf, tdf2, by = "medianof")
    
    })
  output$data3 <- renderDataTable({DT::datatable(df3(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot3 <- renderPlot({ggplot(df3(), aes(x=causeof, y=sum_count)) +
      scale_y_discrete(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~medianof, ncol=1) + 
      coord_flip() + 
      # Add sum_sales - window_avg_sales label.
      geom_text(mapping=aes(x=causeof, y=sum_count, label=round(sum_count - window_countof)),colour="red", hjust=-.5) +
      # Add reference line.
      geom_hline(aes(yintercept = window_countof), color="red")
  })
  # End Barchart Tab3 ___________________________________________________________
  
})
