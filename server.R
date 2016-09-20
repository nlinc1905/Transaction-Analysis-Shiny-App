
library(sqldf)
library(lubridate)
library(igraph)
library(jsonlite)
library(ggplot2)
library(plotly)
library(dbscan)
library(fpc)
library(factoextra)
library(shiny)
library(shinydashboard)
library(googleway)
library(magrittr)

set.seed(12)

refreshAllGraphs <- function(nodes, d) {
  
  #------------------------------------------------------------------#
  #---------------------Prepare Network Data-------------------------#
  #------------------------------------------------------------------#
  
  #Query all unique combinations of links
  q_unique_links <- "SELECT DISTINCT d.ORIG_ID, d.BENEF_ID FROM d"
  res_unique_links <- sqldf(q_unique_links)
  
  #Query earliest transaction date between unique combinations of links
  d$TRXN_DT_S <- gsub('-', '', as.character(d$TRXN_DT))  #Put in SQLite acceptable format
  q_min_dates <- readChar("min_trxn_dt_btwn_parties.sql", file.info("min_trxn_dt_btwn_parties.sql")$size)
  res_min_dates <- sqldf(q_min_dates)
  res_min_dates$min_trxn_dt <- as.Date(parse_date_time(res_min_dates$min_trxn_dt, orders="ymd"))
  res_unique_links$MERGE_KEY <- paste(res_unique_links$ORIG_ID, res_unique_links$BENEF_ID, sep="-")
  res_unique_links <- merge(res_unique_links, res_min_dates, by.x='MERGE_KEY', by.y='parties')
  res_unique_links$MERGE_KEY <- NULL
  colnames(res_unique_links) <- c('ORIG_ID', 'BENEF_ID', 'MIN_TRXN_DT')
  
  #Loop through the resulting dataframe of unique combinations of links, removing any that are the reverse of another
  res_unique_links$REMOVE_DUPLICATES <- 'Keep'
  for (i in 1:nrow(res_unique_links)) {
    for (j in 1:nrow(res_unique_links)) {
      if (res_unique_links[i,which(colnames(res_unique_links) %in% 'REMOVE_DUPLICATES')]=='Remove') {
        break
      } else if (paste(res_unique_links[i,which(colnames(res_unique_links) %in% 'ORIG_ID')], res_unique_links[i,which(colnames(res_unique_links) %in% 'BENEF_ID')])==paste(res_unique_links[j,which(colnames(res_unique_links) %in% 'BENEF_ID')], res_unique_links[j,which(colnames(res_unique_links) %in% 'ORIG_ID')])) {
        res_unique_links[j,which(colnames(res_unique_links) %in% 'REMOVE_DUPLICATES')] <- 'Remove'
        break
      }
    }
  }
  res_unique_links <- res_unique_links[res_unique_links$REMOVE_DUPLICATES=='Keep',1:3]
  
  #Loop through unique combinations of links, counting all rows and summing transaction amounts for each pair
  res_unique_links$TOT_TRXN_CT <- 0
  res_unique_links$TOT_TRXN_AMT <- 0
  for (i in 1:nrow(res_unique_links)) {
    ct_trxns <- 0
    tot_am <- 0
    for (j in 1:nrow(d)) {
      if (paste(res_unique_links[i,which(colnames(res_unique_links) %in% 'ORIG_ID')], res_unique_links[i,which(colnames(res_unique_links) %in% 'BENEF_ID')])==paste(d[j, which(colnames(d) %in% 'ORIG_ID')], d[j, which(colnames(d) %in% 'BENEF_ID')]) || paste(res_unique_links[i,which(colnames(res_unique_links) %in% 'ORIG_ID')], res_unique_links[i,which(colnames(res_unique_links) %in% 'BENEF_ID')])==paste(d[j, which(colnames(d) %in% 'BENEF_ID')], d[j, which(colnames(d) %in% 'ORIG_ID')])) {
        ct_trxns <- ct_trxns + 1
        tot_am <- tot_am + d[j, which(colnames(d) %in% 'TRXN_AMT')]
      }
    }
    res_unique_links[i, which(colnames(res_unique_links) %in% 'TOT_TRXN_CT')] <- ct_trxns
    res_unique_links[i, which(colnames(res_unique_links) %in% 'TOT_TRXN_AMT')] <- tot_am
  }
  
  #Scale trxn_ct and trxn_amt b/c they will be the link weights and need to be values that show up well in a graph
  normalize <- function(x) {
    (x-min(x))/(max(x)-min(x))
  }
  res_unique_links$TOT_TRXN_CT <- round((normalize(res_unique_links$TOT_TRXN_CT)*10)+1,0)
  res_unique_links$TOT_TRXN_AMT <- round((normalize(res_unique_links$TOT_TRXN_AMT)*10)+1,0)
  
  #Convert date to format that D3 can understand
  res_unique_links$MIN_TRXN_DT <- format(as.Date(res_unique_links$MIN_TRXN_DT), "%d-%m-%Y")
  
  #Pass network data into igraph to calculate standardized centrality measures
  inet <- graph_from_data_frame(d=res_unique_links, vertices=nodes, directed=F)
  nodes$BETW_CENTRALITY <- round((normalize(betweenness(inet))*10)+1, 0)
  
  #Convert dataframes to JSON that can be read by D3
  links <- res_unique_links
  colnames(links) <- c('source', 'target', 'event_date', 'weight_ct', 'weight_amt')
  nodes$CLUSTER <- 1
  nodes_network <- nodes[,c(1,2,9,10)]
  colnames(nodes_network) <- c('id', 'name', 'centrality', 'group')
  json_string <- paste("{\"nodes\":", toJSON(nodes_network), ",\"links\":", toJSON(links), "}", sep="")
  
  #------------------------------------------------------------------#
  #--------------------Prepare Transaction Data----------------------#
  #------------------------------------------------------------------#
  
  #Draw line chart of trxn_amt over time, note: unit(top, right, bottom, left)
  trxn_amt_over_time <- aggregate(TRXN_AMT ~ TRXN_DT, data=d, sum)
  
  #Other graphs use d as the data source and were moved to server code below
  
  #------------------------------------------------------------------#
  #--------------------------Benford's Law---------------------------#
  #------------------------------------------------------------------#
  
  #Remove amounts < $1
  dl <- d[d$TRXN_AMT>=1,]
  
  #Isolate digits for testing
  b <- data.frame(trxn_id = dl$TRXN_ID,
                  firstDigit = as.factor(as.numeric(substring(dl$TRXN_AMT,1,1))),
                  firstTwoDigit = as.factor(as.numeric(substring(dl$TRXN_AMT,1,2))))
  
  #Expected ratios according to Benford's Law
  firstDigitBenford <- vector()
  for (i in 1:9) {
    firstDigitBenford[i] <- log((1+(1/i)), 10)
  }
  names(firstDigitBenford) = c(1:9)
  
  firstTwoDigitBenford <- vector()
  for (i in 1:99) {
    firstTwoDigitBenford[i] <- log((1+(1/(10*as.numeric(substring(i, 1, 1))+as.numeric(substring(i, 2, 2))))), 10)
  }
  names(firstTwoDigitBenford) = c(1:99)
  
  #Observed ratios in the data
  rowCount <- nrow(dl)
  firstDigitFrequency <- summary(b$firstDigit)
  firstTwoDigitFrequency <- summary(b$firstTwoDigit)
  #Insert 0's for missing digits
  for (i in 1:9) {
    if (!(i %in% names(firstDigitFrequency))) {
      firstDigitFrequency <- append(firstDigitFrequency, 0, after=i-1)
    }
  }
  for (i in 1:99) {
    if (!(i %in% names(firstTwoDigitFrequency))) {
      firstTwoDigitFrequency <- append(firstTwoDigitFrequency, 0, after=i-1)
    }
  }
  names(firstDigitFrequency) <- c(1:9)
  names(firstTwoDigitFrequency) <- c(1:99)
  firstDigitObserved <- firstDigitFrequency/rowCount
  firstTwoDigitObserved <-firstTwoDigitFrequency/rowCount
  
  #Z-scores for each digit test
  firstDigitZN <- ifelse (1/(2*rowCount) < abs(firstDigitObserved-firstDigitBenford), 
                          abs(firstDigitObserved-firstDigitBenford) - 1/(2*rowCount), 
                          abs(firstDigitObserved-firstDigitBenford))
  firstDigitZD <- sqrt((firstDigitBenford*(1-firstDigitBenford))/rowCount)
  firstDigitZ <- firstDigitZN/firstDigitZD
  
  firstTwoDigitZN <- ifelse (1/(2*rowCount) < abs(firstTwoDigitObserved-firstTwoDigitBenford), 
                             abs(firstTwoDigitObserved-firstTwoDigitBenford) - 1/(2*rowCount), 
                             abs(firstTwoDigitObserved-firstTwoDigitBenford))
  firstTwoDigitZD <- sqrt((firstTwoDigitBenford*(1-firstTwoDigitBenford))/rowCount)
  firstTwoDigitZ <- firstTwoDigitZN/firstTwoDigitZD
  
  #Z Critical Value at 95% confidence
  zCrit <- 1.96
  
  #Test for significance: if higher than critical value then abnormal
  firstDigitTests <- abs(firstDigitZ) > zCrit
  firstTwoDigitTests <- abs(firstTwoDigitZ) > zCrit
  
  #Combine the expected vs observed frequencies and ratios of each digit and calculate MAD values
  firstDigitActual <- firstDigitFrequency
  firstDigitExpected <-firstDigitBenford*rowCount 
  indexValue <- as.factor(c("1","2","3","4","5","6","7","8","9"))
  firstDigitResults = data.frame (indexValue, firstDigitActual, firstDigitExpected, firstDigitTests)
  colnames(firstDigitResults) <- c("First_Digit", "Actual_Frequency", "Expected_Frequency", "Abnormal_Flag")
  firstDigitResults$Perc_Abnormal <- nrow(firstDigitResults[firstDigitResults$Abnormal_Flag==T,])/nrow(firstDigitResults)
  firstDigitResults$Observed_Ratio <- firstDigitObserved
  firstDigitResults$Expected_Ratio <- firstDigitBenford
  fdMAD <- sum(abs(firstDigitResults$Observed_Ratio-firstDigitResults$Expected_Ratio))/nrow(firstDigitResults)
  firstDigitResults$MAD_Value <- fdMAD
  firstDigitResults$MAD_Conformity <- "Close Conformity"
  if (fdMAD <= 0.004) {
    firstDigitResults$MAD_Conformity <- "Close Conformity"
  } else if (fdMAD > 0.004 & fdMAD <= 0.008) {
    firstDigitResults$MAD_Conformity <- "Acceptable Conformity"
  } else if (fdMAD > 0.008 & fdMAD <= 0.012) {
    firstDigitResults$MAD_Conformity <- "Marginally Acceptable Conformity"
  } else if (fdMAD > 0.012) {
    firstDigitResults$MAD_Conformity <- "Non-Conformity"
  }
  
  firstTwoDigitActual <- firstTwoDigitFrequency
  firstTwoDigitExpected <-firstTwoDigitBenford*rowCount 
  ftindexValue <- as.factor(c(1:99))
  firstTwoDigitResults = data.frame (ftindexValue, firstTwoDigitActual, firstTwoDigitExpected, firstTwoDigitTests)
  colnames(firstTwoDigitResults) <- c("First_Two_Digits", "Actual_Frequency", "Expected_Frequency", "Abnormal_Flag")
  firstTwoDigitResults$Perc_Abnormal <- nrow(firstTwoDigitResults[firstTwoDigitResults$Abnormal_Flag==T,])/nrow(firstTwoDigitResults)
  firstTwoDigitResults$Observed_Ratio <- firstTwoDigitObserved
  firstTwoDigitResults$Expected_Ratio <- firstTwoDigitBenford
  f2dMAD <- sum(abs(firstTwoDigitResults$Observed_Ratio-firstTwoDigitResults$Expected_Ratio))/nrow(firstTwoDigitResults)
  firstTwoDigitResults$MAD_Value <- fdMAD
  firstTwoDigitResults$MAD_Conformity <- "Close Conformity"
  if (fdMAD <= 0.004) {
    firstTwoDigitResults$MAD_Conformity <- "Close Conformity"
  } else if (fdMAD > 0.004 & fdMAD <= 0.008) {
    firstTwoDigitResults$MAD_Conformity <- "Acceptable Conformity"
  } else if (fdMAD > 0.008 & fdMAD <= 0.012) {
    firstTwoDigitResults$MAD_Conformity <- "Marginally Acceptable Conformity"
  } else if (fdMAD > 0.012) {
    firstTwoDigitResults$MAD_Conformity <- "Non-Conformity"
  }
  
  #Clear up space
  remove(dl, indexValue, ftindexValue, firstDigitActual, firstDigitBenford,
         firstDigitExpected, firstDigitFrequency, firstDigitObserved, firstDigitZ,
         firstDigitZD, firstDigitZN, firstTwoDigitActual, firstTwoDigitBenford, 
         firstTwoDigitExpected, firstTwoDigitFrequency, firstTwoDigitObserved, 
         firstTwoDigitZ, firstTwoDigitZD, firstTwoDigitZN, rowCount, firstDigitTests,
         firstTwoDigitTests, fdMAD, f2dMAD, zCrit)
  
  #------------------------------------------------------------------#
  #-----------------------Change in Behavior-------------------------#
  #------------------------------------------------------------------#
  
  #Use KNN to determine optimal epsilon value for dbscan density based clustering
  kNNdistplot(as.matrix(d$TRXN_AMT), k=2)
  abline(h=70, lty=2)  #70 appears to be a knee
  
  #dbscan using fpc package
  res.fpc <- fpc::dbscan(d$TRXN_AMT, eps=70, MinPts=10)
  
  #View clusters using factoextra package, use TRXN_DT as an index of sorts (gives the plot x-values)
  dgraph <- d[,which(colnames(d) %in% c('TRXN_AMT', 'TRXN_DT'))]
  dgraph$TRXN_DT <- as.numeric(dgraph$TRXN_DT)
  #d_cluster_plot <- fviz_cluster(res.fpc, data=dgraph, geom="point", title="Density Based Cluster Plot")
  
  #Add outliers column to data, according to density based clustering (cluster 0 == outlier)
  d$OUTLIER_FLAG <- !res.fpc$cluster
  
  #------------------------------------------------------------------#
  #--------------------------Address Maps----------------------------#
  #------------------------------------------------------------------#
  
  #Info to appear in marker information box on Google map
  nodes_for_map <- nodes
  nodes_for_map$info <- paste("Cust_ID:", nodes_for_map$CUST_ID, "Cust_Name:", nodes_for_map$CUST_NM)
  nodes_for_map <- nodes_for_map[,7:9]
  names(nodes_for_map) <- c('lat', 'lon', 'info')
  
  #------------------------------------------------------------------#
  #-----------------Combine Data into Results List-------------------#
  #------------------------------------------------------------------#
  
  finalResults <- list(d, nodes, trxn_amt_over_time, firstDigitResults,
                       firstTwoDigitResults, res.fpc, dgraph, nodes_for_map,
                       json_string)
  return (finalResults)
  
}

#------------------------------------------------------------------#
#----------------------Load and Refresh Data-----------------------#
#------------------------------------------------------------------#

#Get data from CSV source
refreshAllData <- function(cust_nm=NULL, startdt=NULL, enddt=NULL) {
  nodes <- read.csv('Fake_Customers.csv', header=T)
  d <- read.csv('Fake_Transactions.csv', header=T)
  d$TRXN_DT <- as.Date(as.character(d$TRXN_DT), format='%m/%d/%Y')
  d$TRXN_AMT <- as.numeric(d$TRXN_AMT)
  #Apply filters if they are passed to the function
  if (!is.null(cust_nm)) {
    d <- d[d$ORIG_NM==cust_nm | d$BENEF_NM==cust_nm,]
  }
  if (!is.null(startdt) & !is.null(enddt)) {
    d <- d[d$TRXN_DT >= as.Date(startdt, format='%m/%d/%Y') & d$TRXN_DT <= as.Date(enddt, format='%m/%d/%Y'),]
  }
  return (refreshAllGraphs(nodes, d))
}

#------------------------------------------------------------------#
#--------------------------Shiny Server----------------------------#
#------------------------------------------------------------------#

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    all_dat <- refreshAllData()
    session$sendCustomMessage(type="jsondata", all_dat[[9]])  #json_string is the source of the network data
    return (all_dat)
    #List indices returned by refreshAllData are defined as:
    #transactions = 1
    #nodes = 2
    #total transaction amount over time = 3
    #first digit benford = 4
    #first 2 digits benford = 5
    #density clusters = 6
    #density clusters names = 7
    #map data = 8
    #json string for d3 network graph = 9
  })
  
  #Observe function listens for user actions like button clicks and renders stuff appropriately
  observe({
    #On page load, initialize data as complete dataset with no filters
    all_dat <- getData()
    #If the cust filter button is clicked, filter data by cust
    if (input$filterByCust > 0) {
      all_dat <- refreshAllData(req(input$cust_name_filter))
    }
    #If the remove filters button is clicked, refresh data
    if (input$refreshData > 0) {
      session$reload()
      #all_dat <- refreshAllData()
    }
    
    datasetInputOverview <- reactive({
      switch(input$dataset1, 
             "Transactions" = all_dat[[1]],
             "Customers" = all_dat[[2]])
    })
    
    #Render the overview tab data table outputs based on user selection
    output$table1 <- renderDataTable({
      datasetInputOverview()
    })
    
    #Link to download the selected dataset
    output$downloadData1 <- downloadHandler(
      filename=function() { paste(input$dataset1, ".csv", sep="") },
      content=function(filename) {
        write.csv(datasetInputOverview(), filename, row.names=F)
      })
    
    #Render transaction amount over time line plot
    output$trxnTimeLine <- renderPlotly({
      ggplotly(
        ggplot(data=all_dat[[3]], aes(x=TRXN_DT, y=TRXN_AMT)) + 
          geom_point(size=1.5, color="forestgreen") + 
          geom_smooth() +
          ggtitle("Daily Total Transaction Amounts") +
          scale_y_continuous(expand=c(0.02, 0)) +
          theme(text=element_text(size=12), plot.title=element_text(color="#666666", face="bold", size=14), 
                axis.title=element_text(color="#666666", face="bold", size=12),
                plot.margin = unit(c(0, 0.2, 0.5, 1.2), "cm"))
      )
    })
    
    #Render box plot of transaction amount by type
    output$trxnTypeBoxPlot <- renderPlotly({
      ggplotly(
        ggplot(data=all_dat[[1]], aes(x=TRXN_TYPE, y=TRXN_AMT, fill=TRXN_TYPE)) + 
          geom_boxplot() +
          ggtitle("Boxplot of Transaction Amount by Type") +
          theme(text=element_text(size=14), plot.title=element_text(color="#666666", face="bold", size=14), 
                axis.title=element_text(color="#666666", face="bold", size=12),
                plot.margin = unit(c(0, 0.2, 0.5, 1.2), "cm"), legend.position="none")
      )
    })
    
    #Render bar plot of transaction count by type
    output$trxnCtByType <- renderPlotly({
      ggplotly(
        ggplot(data=all_dat[[1]], aes(x=TRXN_TYPE, fill=TRXN_TYPE)) + 
          geom_bar() +
          ggtitle("Count Transactions by Type") +
          labs(y="TRXN_COUNT") +
          scale_y_continuous(expand=c(0.01, 0)) +
          theme(text=element_text(size=14), plot.title=element_text(color="#666666", face="bold", size=14), 
                axis.title=element_text(color="#666666", face="bold", size=12),
                plot.margin = unit(c(0, 0.2, 0.5, 1.2), "cm"), legend.position="none")
      )
    })
    
    datasetInputTransactions <- reactive({
      switch(input$dataset2, 
             "Transactions" = all_dat[[1]],
             "Outlier Transactions Only" = all_dat[[1]][all_dat[[1]]$OUTLIER_FLAG==T,],
             "First Digit Results" = all_dat[[4]],
             "First Two Digits Results" = all_dat[[5]])
    })
    
    #Render the transactions tab data table outputs based on user selection
    output$table2 <- renderDataTable({
      datasetInputTransactions()
    })
    
    #Link to download the selected dataset
    output$downloadData2 <- downloadHandler(
      filename=function() { paste(input$dataset2, ".csv", sep="") },
      content=function(filename) {
        write.csv(datasetInputTransactions(), filename, row.names=F)
      })
    
    #Render Benford's first digit test plot
    output$firstDigitPlot <- renderPlot({
      ggplot(all_dat[[4]]) +
        geom_bar(aes(x=First_Digit, y=Actual_Frequency), width=0.5, stat="identity", fill="slategray") +
        geom_line(aes(x=as.numeric(First_Digit), y=Expected_Frequency, color="Expected"), size=1.5) +
        scale_colour_manual("", breaks = c("Expected"), values = c("Expected"="indianred")) +
        ggtitle("First Digit Actual VS Expected") +
        labs(x="Digit", y="Frequency") +
        scale_y_continuous(expand=c(0.02, 0)) +
        theme(text=element_text(size=20), plot.title=element_text(color="#666666", face="bold", size=20), 
              axis.title=element_text(color="#666666", face="bold", size=16))
    })
    
    #Render Benford's first two digits test plot
    output$firstTwoDigitPlot <- renderPlot({
      ggplot(all_dat[[5]]) +
        geom_bar(aes(x=First_Two_Digits, y=Actual_Frequency), width=0.5, stat="identity", fill="slategray") +
        geom_line(aes(x=as.numeric(First_Two_Digits), y=Expected_Frequency, color="Expected"), size=1.5) +
        scale_colour_manual("", breaks = c("Expected"), values = c("Expected"="indianred")) +
        ggtitle("First Two Digits Actual VS Expected") +
        labs(x="Digits", y="Frequency") +
        scale_y_continuous(expand=c(0.02, 0)) +
        scale_x_discrete(breaks=c(1,10,20,30,40,50,60,70,80,90,99)) +
        theme(text=element_text(size=20), plot.title=element_text(color="#666666", face="bold", size=20), 
              axis.title=element_text(color="#666666", face="bold", size=16))
    })
    
    #Render transaction amount frequency distribution
    output$trxnAmtDist <- renderPlotly({
      ggplotly(
        ggplot(data=all_dat[[1]], aes(TRXN_AMT)) + 
          geom_histogram(size=2, binwidth=100, fill="cornflowerblue") + 
          ggtitle("Histogram of Transaction Amount") +
          #scale_x_continuous(expand=c(0, 0)) +
          scale_y_continuous(expand=c(0, 0)) +
          labs(x="Rounded Transaction Amount", y="Frequency") +
          theme(text=element_text(size=14), plot.title=element_text(color="#666666", face="bold", size=14), 
                axis.title=element_text(color="#666666", face="bold", size=12),
                plot.margin = unit(c(0, 0.2, 0.5, 1.2), "cm"))
      )
    })
    
    #Render density based clustering plot
    output$dbcPlot <- renderPlot({
      fviz_cluster(all_dat[[6]], data=all_dat[[7]], geom="point", title="Density Based Cluster Plot")
    })
    
    #Render Google map with markers for each customer's location
    output$gmap <- renderGoogle_map({
      google_map(key=input$googleMapsAPIKey, height=600, search_box=T) %>%
        add_markers(data=all_dat[[8]], info_window="info")
    })
  })
  
})


