#web app code

#librarying packages

library("tidyverse")
library("RColorBrewer")
library("forecast")
library("ggthemes")
library("shiny")
library("bslib")
library("zip")



#making UI

UI <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  titlePanel("BoM Temperature Data Viewer"),
  h5("by Toby Jin 
     (iNaturalist user: toby_j_77,
     GitHub: toby-j-77)"),
  
  p(
  "To use this app, download temperature data from http://www.bom.gov.au/climate/data/
  Select your desired station from the website and select any year when requesting data. Once you have requested
  data, you will be taken to a new page. Ensure you download all of the data for that station, and not just the
  the data for the year you selected. Also ensure you download daily minimum and maximum temperatures.
  Unzip the downloaded files and upload the .csv file for the minimum and maximum temperature datasets in their 
  corresponding places. Use the buttons provided to customise your plots. To save your plots, press the download
  button and save your plots as zip folder containing .png files."
  ),
  
  fileInput(
    inputId = "BoM_min",
    label = strong("Upload Minimum Temperature Dataset"),
    accept = ".csv"
  ),
  
  fileInput(
    inputId = "BoM_max",
    label = strong("Upload Maximum Temperature Dataset"),
    accept = ".csv"
  ),
  
  radioButtons(
    inputId = "graph_type",
    label = strong("What to Display?"),
    choices = list(
      "Minimum and Maximum Temperatures" = 0,
      "Temperature Ranges" = 1
    )
  ),
  
  selectInput(
    inputId = "RA_input",
    label = strong("Running Average? (Overwrites Trendline For Temperature Timeseries)"),
    choices = list("None" = "None", 
                   "7 days" = 7, 
                   "31 days" = 31, 
                   "365 days" = 365)
  ),
  
  radioButtons(
    inputId = "trend",
    label = strong("Display Linear Trendline?"),
    choices = list(
      "No" = 0,
      "Yes" = 1
    )
  ),
  
  downloadButton(
    outputId = "DOWNLOADS",
    label = "Download Plots"),

  # dateRangeInput(
  #   inputId = "date",
  #   label = "View Data From",
  #   start = "0000-01-01",
  #   end = today()
  # ), 
  
  navset_pill(
    
    nav_panel(
      title = "Data Information",
      tableOutput(outputId = "Information_table")
    ),
    
    nav_panel(
      title = "Temperature Timeseries",
      plotOutput(outputId = "Timeseries_plot")
    ),
    
    nav_panel(
      title = "Mean Monthly Temperatures",
      plotOutput(outputId = "Monthly_barchart")
    ),
    
    nav_panel(
      title = "Monthly Anomalies",
      plotOutput(outputId = "Monthly_anomalies")
    ),
    
    nav_panel(
      title = "Monthly Anomalies by Month",
      plotOutput(outputId = "Monthly_anomalies_facet")
    )
  ),
  
)

server <- function(input, output, session) {
  #reactive objects
  
  ##dataset
  
  datasets <- reactive({
    #checking for inputs
    
    req(input$BoM_min)
    
    req(input$BoM_max)
    
    #saving what sort of graph the user wants
    
    Graph_type = input$graph_type
    
    #loading in data
    data_min <- read_csv(input$BoM_min$datapath) |> as_tibble()
    data_max <- read_csv(input$BoM_max$datapath) |> as_tibble()
    #changing data column names
    
    names(data_min) <- c(
      "Product_code",
      "Station_number",
      "Year",
      "Month",
      "Day",
      "Min_T",
      "Days_of_accumulation_min_T",
      "Quality_min_T"
    )
    
    names(data_max) <- c(
      "Product_code",
      "Station_number",
      "Year",
      "Month",
      "Day",
      "Max_T",
      "Days_of_accumulation_max_T",
      "Quality_max_T"
    )
    
    #checking if both datasets are from the same location
    
    if (data_min$Station_number[[1]] != data_max$Station_number[[1]]) {
      information_table <- c("Please select 2 stations from the same location") |>
        data.frame()
      names(information_table) <- c("Error")
      
      return(list(information_table = information_table, input_check = 0))
      
    }
    
    else{
      #checking if the datasets are identical
      
      if (data_min$Product_code[[1]] == data_max$Product_code[[1]]) {
        information_table <- c("Input of 2 identical datasets") |>
          data.frame()
        names(information_table) <- c("Error")
        
        return(list(
          information_table = information_table,
          input_check = 0
        ))
      }
      else{
        #processing data
        
        ##changing data structure
        
        data_min <- data_min |>
          mutate(Quality_min_T = as_factor(Quality_min_T)) |>
          mutate(Date = paste(Year, Month, Day, sep = "-")) |>
          mutate(Date = as_date(Date)) |>
          mutate(Month = as.factor(Month))
        
        data_max <- data_max |>
          mutate(Quality_max_T = as_factor(Quality_max_T)) |>
          mutate(Date = paste(Year, Month, Day, sep = "-")) |>
          mutate(Date = as_date(Date)) |>
          mutate(Month = as.factor(Month))
        
        ##binding data
        
        data_joined <- right_join(data_min, data_max, by = "Date", keep = FALSE)
        
        ##removing extra columns
        
        data_joined <- data_joined |>
          select(-ends_with(".y"))
        
        ##renaming columns
        
        names(data_joined) <- c(
          "Product_code",
          "Station_number",
          "Year",
          "Month",
          "Day",
          "Min_T",
          "Days_of_accumulation_min_T",
          "Quality_min_T",
          "Date",
          "Max_T",
          "Days_of_accumulation_max_T",
          "Quality_max_T"
        )
        
        #removing leading and trailing NAs
        
        data_joined <- data_joined[min(which(!is.na(data_joined$Min_T))):max(which(!is.na(data_joined$Min_T))), ]
        
        #calculating temperature range
        
        data_joined <- data_joined |>
          mutate(T_range = Max_T - Min_T) |>
          mutate(T_range_label = "Temperature_range")
        
        #creating information table
        
        information_table <- data.frame(
          station_number = data_joined$Station_number[[1]],
          start_date = data_joined$Date[[1]],
          end_date = data_joined$Date[[nrow(data_joined)]],
          no_of_days = as.integer(data_joined$Date[[nrow(data_joined)]] - data_joined$Date[[1]]),
          days_with_data = nrow(data_joined |> drop_na(Min_T))
        )
        
        information_table <- information_table |>
          mutate(data_percent = round(100 * (days_with_data / no_of_days), digits = 2)) |>
          mutate(start_date = as.character(start_date)) |>
          mutate(end_date = as.character(end_date))
        
        names(information_table) <- c(
          "Station Number",
          "Start Date",
          "End Date",
          "Number of Days",
          "Days With Data",
          "Percent of Days with Data"
        )
        
        #returning items
        
        return(
          list(
            information_table = information_table,
            data_joined = data_joined,
            input_check = 1
          )
        )
        
        
      }
    }
  })
  
  ##timeseries plot
  
  timeseries <- reactive({
    #this ensures datasets are processed correctly
    
    req(datasets()$input_check == 1)
    
    req(input$RA_input)
    
    req(input$trend)
    
    #inputs
    
    data_joined <- datasets()$data_joined
    
    RA_selection <- input$RA_input
    
    graph_type <- input$graph_type
    
    trend <- input$trend
    
    #if no runnning average is selected
    
    if (RA_selection == "None") {
      #creating temperature plot with no running average
      
      plot_1A <- ggplot(
        data_joined |>
          select(Min_T, Max_T, Date) |>
          pivot_longer(
            cols = c(Min_T, Max_T),
            names_to = "Min_or_max",
            values_to = "Temp"
          )
      ) +
        geom_line(aes(
          x = Date,
          y = Temp,
          col = Min_or_max,
          alpha = Min_or_max
        )) +
        labs(x = "Date", y = "Temperature (\u00b0C)", col = "Min/Max", alpha = "Min/Max") +
        ggtitle(
          paste0(
            "Minimum and Maximum Temperature Timeseries For Station ",
            (datasets()$data_joined)$Station_number[[1]]
          )
        ) +
        theme_bw() +
        scale_colour_manual(values = c("red", "blue"),
                            labels = c("Maximum", "Minimum")) +
        scale_alpha_manual(values = c(1, 1),
                           labels = c("Maximum", "Minimum"))
      
      #creating temperature range plot with no running average
      
      plot_1B <- ggplot(data_joined) +
        geom_line(aes(
          x = Date,
          y = T_range,
          col = T_range_label,
          alpha = T_range_label
        )) +
        labs(
          x = "Date",
          y = "Temperature Range (\u00b0C)",
          col = "Temperature_range",
          alpha = "Temperature_range"
        ) +
        ggtitle(paste0(
          "Temperature Range Timeseries For Station ",
          (datasets()$data_joined)$Station_number[[1]]
        )) +
        theme_bw() +
        scale_color_manual(values = c('black')) +
        scale_alpha_manual(values = c(1))
      
      if (trend == 1) {
        #adding trendlines
        
        plot_1A <- plot_1A +
          geom_smooth(
            aes(x = Date, y = Temp, col = Min_or_max),
            method = 'lm',
            se = FALSE,
            linetype = 1,
            linewidth = 1.25
          ) +
          scale_alpha_manual(values = c(0.25, 0.25),
                             labels = c("Maximum", "Minimum"))
        
        plot_1B <- plot_1B +
          geom_smooth(
            aes(x = Date, y = T_range),
            col = 'black',
            method = 'lm',
            se = FALSE,
            linetype = 1,
            linewidth = 1.25,
            alpha = 1
          ) +
          scale_alpha_manual(values = c(0.25))
      }
      
      #returning plots
      
      if (graph_type == 0) {
        return(list(timeseries_plot = plot_1A))
        
      }
      
      else{
        return(list(timeseries_plot = plot_1B))
        
      }
    }
    
    #if a value is provided for the running average
    else{
      #adding running average into the data
      
      data_joined_RA <- data_joined |>
        mutate(RA_min_T = split_fill(
          Min_T,
          na_length = as.integer(RA_selection),
          ra_length = as.integer(RA_selection)
        )) |>
        mutate(RA_max_T = split_fill(
          Max_T,
          na_length = as.integer(RA_selection),
          ra_length = as.integer(RA_selection)
        )) |>
        mutate(RA_T_range = split_fill(
          T_range,
          na_length = as.integer(RA_selection),
          ra_length = as.integer(RA_selection)
        )) |>
        select(Min_T,
               Max_T,
               RA_min_T,
               RA_max_T,
               T_range,
               RA_T_range,
               Date) |>
        pivot_longer(
          cols = c(Min_T, Max_T, RA_min_T, RA_max_T),
          names_to = "Min_or_max",
          values_to = "Temp"
        ) |>
        pivot_longer(
          cols = c(T_range, RA_T_range),
          names_to = "T_range_data",
          values_to = "Temperature_range"
        )
      
      #ggplotting min max
      
      plot_1A <- ggplot(data_joined_RA) +
        geom_line(aes(
          x = Date,
          y = Temp,
          col = Min_or_max,
          alpha = Min_or_max
        )) +
        labs(
          x = "Date",
          y = "Temperature (\u00b0C)",
          col = "Min/Max",
          alpha = "Min/Max"
        ) +
        ggtitle(
          paste0(
            "Minimum and Maximum Temperature Timeseries For Station ",
            (datasets()$data_joined)$Station_number[[1]],
            " With ",
            RA_selection,
            " Day Running Average"
          )
        ) +
        theme_bw() +
        scale_colour_manual(
          values = c("red", "blue", "red", "blue"),
          labels = c(
            "Maximum",
            "Minimum",
            "Running Average Maximum",
            "Running Average Minimum"
          )
        ) +
        scale_alpha_manual(
          values = c(0.25, 0.25, 1, 1),
          labels = c(
            "Maximum",
            "Minimum",
            "Running Average Maximum",
            "Running Average Minimum"
          )
        )
      
      #ggplotting temperature range
      
      plot_1B <- ggplot(data_joined_RA) +
        geom_line(aes(
          x = Date,
          y = Temperature_range,
          col = T_range_data,
          alpha = T_range_data
        )) +
        labs(
          x = "Date",
          y = "Temperature Range (\u00b0C)",
          col = "Temperature Range",
          alpha = "Temperature Range"
        ) +
        ggtitle(
          paste0(
            "Temperature Range Timeseries For Station ",
            (datasets()$data_joined)$Station_number[[1]],
            " With ",
            RA_selection,
            " Day Running Average"
          )
        ) +
        theme_bw() +
        scale_colour_manual(
          values = c("black", "black"),
          labels = c("Running Average Temperature Range", "Temperature Range")
        ) +
        scale_alpha_manual(
          values = c(1, 0.25),
          labels = c("Running Average Temperature Range", "Temperature Range")
        )
      #returning plots
      
      if (graph_type == 0) {
        return(list(timeseries_plot = plot_1A))
        
      }
      
      else{
        return(list(timeseries_plot = plot_1B))
        
      }
      
    }
  })
  
  ##monthly data (includes mean monthly temp, and both monthly anomaly charts)
  
  monthly_data <- reactive({
    #required inputs
    
    req(datasets()$input_check == 1)
    
    req(input$RA_input)
    
    req(input$trend)
    
    #getting values
    
    data_joined <- datasets()$data_joined
    
    graph_type <- input$graph_type
    
    trend <- input$trend
    
    #calculating monthly means and monthly mean anomalies
    
    monthly_data <- data_joined  |>
      group_by(Month) |>
      mutate(Monthly_mean_min_T = mean(Min_T, na.rm = TRUE)) |>
      mutate(Monthly_mean_max_T = mean(Max_T, na.rm = TRUE)) |>
      mutate(Monthly_mean_T_range = mean(T_range, na.rm = TRUE)) |>
      ungroup() |>
      group_by(Year, Month) |>
      mutate(Monthly_anomaly_min_T = mean(Min_T, na.rm = TRUE) -
               Monthly_mean_min_T) |>
      mutate(Monthly_anomaly_max_T = mean(Max_T, na.rm = TRUE) -
               Monthly_mean_max_T) |>
      mutate(Monthly_anomaly_T_range = mean(T_range, na.rm = TRUE) -
               Monthly_mean_T_range) |>
      ungroup() |>
      mutate(Month_middle = month_middle(Date))
    
    #creating plots
    
    ##monthly mean temperatures
    
    plot_2A <- ggplot(
      monthly_data |>
        group_by(Month, Monthly_mean_min_T, Monthly_mean_max_T) |>
        summarise(
          Monthly_mean_min_T = mean(Monthly_mean_min_T),
          Monthly_mean_max_T = mean(Monthly_mean_max_T)
        ) |>
        pivot_longer(
          cols = c(Monthly_mean_min_T, Monthly_mean_max_T),
          names_to = "Min_or_max",
          values_to = "Monthly_mean_T"
        )
    ) +
      geom_col(
        aes(x = Month, y = Monthly_mean_T, fill = Min_or_max),
        col = "black",
        position = "dodge"
      ) +
      ggtitle(paste0(
        "Mean Monthly Temperatures For Station ",
        (datasets()$data_joined)$Station_number[[1]]
      )) +
      labs(x = "Month", y = "Mean Temperature (\u00b0C)", fill = "Min/Max") +
      theme_bw() +
      scale_fill_manual(values = c("red", "blue"),
                        labels = c("Maximum", "Minimum"))
    
    ##monthly mean temperature ranges
    
    plot_2B <- ggplot(
      monthly_data |>
        group_by(Month, Monthly_mean_T_range) |>
        summarise(Monthly_mean_T_range = mean(Monthly_mean_T_range))
    ) +
      geom_col(
        aes(x = Month, y = Monthly_mean_T_range),
        col = "black",
        fill = 'grey50',
        position = "dodge"
      ) +
      ggtitle(paste0(
        "Mean Monthly Temperature Range For Station ",
        (datasets()$data_joined)$Station_number[[1]]
      )) +
      labs(x = "Month", y = "Mean Temperature Range (\u00b0C)") +
      theme_bw()
    
    ##monthly min max temperature anomalies
    
    plot_3A <- ggplot(
      monthly_data |>
        distinct(
          Month_middle,
          Monthly_anomaly_min_T,
          Monthly_anomaly_max_T
        ) |>
        pivot_longer(
          cols = c(Monthly_anomaly_min_T, Monthly_anomaly_max_T),
          names_to = "Min_or_max",
          values_to = "Monthly_anomaly"
        )
    ) +
      geom_line(
        aes(
          x = Month_middle,
          y = Monthly_anomaly,
          col = Min_or_max,
          alpha = Min_or_max
        ),
        linewidth = 0.75
      ) +
      ggtitle(paste0(
        "Monthly Temperature Anomalies For Station ",
        (datasets()$data_joined)$Station_number[[1]]
      )) +
      labs(
        x = "Date",
        y = "Monthly temperature anomaly (\u00b0C)",
        col = "Min/Max",
        alpha = "Min/Max"
      ) +
      theme_bw() +
      geom_hline(yintercept = 0,
                 col = "black",
                 alpha = 1) +
      scale_colour_manual(values = c("red", "blue"),
                          labels = c("Maximum", "Minimum")) +
      scale_alpha_manual(values = c(1, 1),
                         labels = c("Maximum", "Minimum"))
    
    ##monthly temperature range anomalies
    
    plot_3B <- ggplot(
      monthly_data |>
        distinct(Month_middle, Monthly_anomaly_T_range) |>
        mutate(T_range_label = "Temperature")
    ) +
      geom_line(
        aes(
          x = Month_middle,
          y = Monthly_anomaly_T_range,
          col = T_range_label,
          alpha = T_range_label
        ),
        col = "black",
        linewidth = 0.75
      ) +
      ggtitle(paste0(
        "Monthly Temperature Range Anomalies For Station ",
        (datasets()$data_joined)$Station_number[[1]]
      )) +
      labs(
        x = "Date",
        y = "Monthly Temperature Range Anomaly (\u00b0C)",
        col = "Monthly Temperature Range Anomaly",
        alpha = "Monthly Temperature Range Anomaly"
      ) +
      theme_bw() +
      geom_hline(yintercept = 0,
                 col = "black",
                 alpha = 1) +
      scale_colour_manual(
        values = c("black"),
        labels = c("Monthly Temperature Range Anomaly")
      ) +
      scale_alpha_manual(values = c(1),
                         labels = c("Monthly Temperature Range Anomaly"))
    
    ##monthly min max temperature anomaly matrix
    
    plot_4A <- ggplot(
      monthly_data |>
        distinct(Monthly_anomaly_min_T, Monthly_anomaly_max_T, Year, Month) |>
        pivot_longer(
          cols = c(Monthly_anomaly_min_T, Monthly_anomaly_max_T),
          names_to = "Min_or_max",
          values_to = "Monthly_anomaly"
        )
    ) +
      geom_line(aes(
        x = Year,
        y = Monthly_anomaly,
        col = Min_or_max,
        alpha = Min_or_max
      ),
      linewidth = 0.75) +
      ggtitle(
        paste0(
          "Monthly Temperature Anomalies For Station ",
          (datasets()$data_joined)$Station_number[[1]],
          " For Each Month"
        )
      ) +
      labs(
        x = "Date",
        y = "Monthly Temperature Range Anomaly (\u00b0C)",
        col = "Min/Max",
        alpha = "Min/Max"
      ) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "black") +
      scale_colour_manual(values = c("red", "blue"),
                          labels = c("Maximum", "Minimum")) +
      scale_alpha_manual(values = c(1, 1),
                         labels = c("Maximum", "Minimum")) +
      facet_wrap(~ Month)
    
    ##monthly temperature range anomaly matrix
    
    plot_4B <- ggplot(
      monthly_data |>
        distinct(Monthly_anomaly_T_range, Year, Month) |>
        mutate(T_range_label = "Temperature")
    ) +
      geom_line(
        aes(
          x = Year,
          y = Monthly_anomaly_T_range,
          col = T_range_label,
          alpha = T_range_label
        ),
        linewidth = 0.75
      ) +
      ggtitle(
        paste0(
          "Monthly Temperature Range Anomalies For Station ",
          (datasets()$data_joined)$Station_number[[1]],
          " For Each Month"
        )
      ) +
      labs(
        x = "Date",
        y = "Monthly Temperature Range Anomaly (\u00b0C)",
        col = "Monthly Temperature Range Anomaly",
        alpha = "Monthly Temperature Range Anomaly"
      ) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "black") +
      scale_colour_manual(
        values = c('black'),
        labels = c("Monthly Temperature Range Anomaly")
      ) +
      scale_alpha_manual(values = c(1),
                         labels = c("Monthly Temperature Range Anomaly")) +
      facet_wrap(~ Month)
    
    ##adding trends
    
    if (trend == 1) {
      plot_3A <- plot_3A +
        geom_smooth(
          aes(x = Month_middle, y = Monthly_anomaly, col = Min_or_max),
          method = 'lm',
          se = FALSE,
          alpha = 1,
          linetype = 1,
          linewidth = 1.25
        ) +
        scale_alpha_manual(values = c(0.25, 0.25),
                           labels = c("Maximum", "Minimum"))
      
      plot_3B <- plot_3B +
        geom_smooth(
          aes(x = Month_middle, y = Monthly_anomaly_T_range),
          col = 'black',
          method = 'lm',
          se = FALSE,
          linetype = 1,
          linewidth = 1.25,
          alpha = 1
        ) +
        scale_alpha_manual(
          values = c(0.25),
          labels = c("Monthly Temperature Range Anomaly")
        )
      
      plot_4A <- plot_4A +
        geom_smooth(
          aes(x = Year, y = Monthly_anomaly, col = Min_or_max),
          method = 'lm',
          se = FALSE,
          alpha = 1,
          linetype = 1,
          linewidth = 1.25
        ) +
        scale_alpha_manual(values = c(0.25, 0.25),
                           labels = c("Maximum", "Minimum"))
      
      plot_4B <- plot_4B +
        geom_smooth(
          aes(x = Year, y = Monthly_anomaly_T_range),
          col = 'black',
          method = 'lm',
          se = FALSE,
          linetype = 1,
          linewidth = 1.25,
          alpha = 1
        ) +
        scale_alpha_manual(
          values = c(0.25),
          labels = c("Monthly Temperature Range Anomaly")
        )
      
      
    }
    
    ##returning plots
    
    if (graph_type == 0) {
      return(
        list(
          monthly_data = monthly_data,
          monthly_mean_plot = plot_2A,
          monthly_anomaly_plot = plot_3A,
          monthly_anomaly_facet = plot_4A
        )
      )
    }
    
    else{
      return(
        list(
          monthly_data = monthly_data,
          monthly_mean_plot = plot_2B,
          monthly_anomaly_plot = plot_3B,
          monthly_anomaly_facet = plot_4B
        )
      )
    }
  })
  
  #updating date selection
  
  # observe({
  #
  #   req(datasets()$input_check == 1)
  #
  #   data_joined <- datasets()$data_joined
  #
  #   #setting boundaries for dates
  #
  #   updateDateRangeInput(
  #
  #     session = session,
  #     inputId = "date",
  #     label = "Select Taxa (up to 5 selections)",
  #     min = "0000-01-26",
  #     start = min(data_joined$Date),
  #     max = today(),
  #     end = max(data_joined$Date)
  #   )
  #
  #   #doing it again
  #
  #   updateDateRangeInput(
  #
  #     session = session,
  #     inputId = "date",
  #     label = "Select Taxa (up to 5 selections)",
  #     min = min(data_joined$Date),
  #     max = max(data_joined$Date)
  #   )
  #
  # })
  
  
  #outputs
  
  ##information table
  
  output$Information_table <- renderTable({
    #checks
    
    req(input$BoM_min)
    
    req(input$BoM_max)
    
    #return table
    
    return(datasets()$information_table)
    
  })
  
  #plots for min and max
  
  ##timeseries plot
  
  output$Timeseries_plot <- renderPlot({
    #checks
    
    req(datasets()$input_check == 1)
    
    #return plot
    
    return(timeseries()$timeseries_plot)
  })
  
  #mean monthly temperature barchart
  
  output$Monthly_barchart <- renderPlot({
    #checks
    
    req(datasets()$input_check == 1)
    
    #return plot
    
    return(monthly_data()$monthly_mean_plot)
  })
  
  #monthly anomaly
  
  output$Monthly_anomalies <- renderPlot({
    #checks
    
    req(datasets()$input_check == 1)
    
    #return plot
    
    return(monthly_data()$monthly_anomaly_plot)
  })
  
  #monthly anomaly facet
  
  output$Monthly_anomalies_facet <- renderPlot({
    #checks
    
    req(datasets()$input_check == 1)
    
    #return plot
    
    return(monthly_data()$monthly_anomaly_facet)
  })
  
  #download plots
  
  output$DOWNLOADS <- downloadHandler(
    #creating zip folder
    
    filename = "plots.zip",
    
    content = function(file) {
      req(datasets()$input_check == 1)
      
      #timeseries plot
      
      ggsave(
        file.path(tempdir(), "Timeseries_plot.png"),
        plot = timeseries()$timeseries_plot,
        device = "png",
        width = 15,
        height = 5,
        dpi = 400
      )
      
      #Monthly_barchart
      
      ggsave(
        file.path(tempdir(), "Mean_Monthly_Temperature.png"),
        plot = monthly_data()$monthly_mean_plot,
        device = "png",
        width = 5,
        height = 5,
        dpi = 400
      )
      
      #Monthly_anomalies
      
      ggsave(
        file.path(tempdir(), "Monthly_Anomalies.png"),
        plot = monthly_data()$monthly_anomaly_plot,
        device = "png",
        width = 15,
        height = 5,
        dpi = 400
      )
      
      #Monthly_anomalies_facet
      
      ggsave(
        file.path(tempdir(), "Monthly_Anomalies_Facet.png"),
        plot = monthly_data()$monthly_anomaly_facet,
        device = "png",
        width = 15,
        height = 10,
        dpi = 400
      )
      
      #zipping files
      
      zip(
        file,
        files = c(
          file.path(tempdir(), "Timeseries_plot.png"),
          file.path(tempdir(), "Mean_Monthly_Temperature.png"),
          file.path(tempdir(), "Monthly_Anomalies.png"),
          file.path(tempdir(), "Monthly_Anomalies_Facet.png")
        ),
        mode = "cherry-pick"
      )
    }
  )
}
  

shinyApp(UI, server)
