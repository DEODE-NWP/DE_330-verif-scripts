# server for shiny_plot_point_verif atos
library(shiny)
library(tidyverse)
library(png)
library(magrittr)
library(here)

server <- function(input, output, session) {
  
  # Get path to cases directory
  basedir <- as.character(normalizePath(file.path(paste0(here(),'/plot_point_verif_local/cases/'))))
  print("basedir es")
  print(basedir)
  # Get list of PNG files in cases directory (recursively)
  png_files <- list.files(path = file.path(basedir), pattern = "\\.png",recursive=TRUE)
  #Separate list elements into components with / and - separators
  choices_list <- lapply(png_files, function(x) {
    strsplit(x, "[-/]+")[[1]]
  })
  
  split_list <- lapply(choices_list, strsplit, split = "-")
  my_table <- do.call(rbind, split_list)
  my_table_df <- as.data.frame(my_table)
  colnames(my_table_df) = c("EXP","A","B","C","D","E","F","G","H","I","J")
  print("original table")
  print(head(my_table_df, 2))
  my_table_df$F <- paste(my_table_df$F, my_table_df$G, sep = "-")
  my_table_df = subset(my_table_df, select = -c(J,G))
  #my_table_df= subset(my_table_df, select = c(EXP,A,B,C,D,E,F,H,I))
  print("subset table")
  print(head(my_table_df, 2))
  colnames(my_table_df) =c("EXP","Variable","Run_type","Metric","submetric","runs","period","Area","fc_length")
  
  j<-unlist(my_table_df)
  my_table_df<-matrix(j,nrow=nrow(my_table_df),ncol=length(j)/nrow(my_table_df))
  my_table_df <- as.data.frame(my_table_df)
  colnames(my_table_df) =c("var_one","var_six","var_four","var_seven","var_eight","var_five","var_two","var_three","var_nine")
  print("final table")
  print(head(my_table_df,2))
  tib <- as_tibble(my_table_df)
  tib %<>% mutate(across(where(is.character),as_factor))
  
  tab <- reactive({
    tib %>%
      filter(var_one == input$var1) %>%
      filter(var_two == input$var2) %>%
      filter(var_three == input$var3) %>%
      filter(var_four == input$var4) %>%
      filter(var_five == input$var5) %>%
      filter(var_six == input$var6) %>%
      filter(var_seven == input$var7) %>%
      filter(var_seven == input$var8) %>%
      filter(var_eigth == input$var9)
  })
  
  updateSelectizeInput(
    session = session,
    inputId = "var1",
    choices = sort(unique(as.character(tib$var_one))),
    selected = head(sort(unique(as.character(tib$var_one))),1),
    options = list(placeholder = 'select'),
    server = TRUE
  )
  
  observeEvent(input$var1,{
    choice_var2 <- sort(unique(as.character(tib$var_two[which(tib$var_one==input$var1)])))
    updateSelectizeInput(
      session = session,
      inputId = "var2",
      choices = choice_var2,
      selected=head(sort(unique(as.character(tib$var_two[which(tib$var_one==input$var1)]))),1)
    )
  })
  
  observeEvent(input$var2,{
    choice_var3 <- sort(unique(as.character(tib$var_three[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2)])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var3",
      choices = choice_var3,
      selected=head(sort(unique(as.character(tib$var_three[which(tib$var_one==input$var1 &
                                                                   tib$var_two==input$var2)]))),1)
    )
  })
  
  observeEvent(input$var3,{
    choice_var4 <- sort(unique(as.character(tib$var_four[which(tib$var_one==input$var1 &
                                                                 tib$var_two==input$var2 &
                                                                 tib$var_three==input$var3)])))
    updateSelectizeInput(
      session = session,
      inputId = "var4",
      choices = choice_var4,
      selected=head(sort(unique(as.character(tib$var_four[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2 &
                                                                  tib$var_three==input$var3)]))),1)
    )
  })
  
  observeEvent(input$var4,{
    choice_var5 <- sort(unique(as.character(tib$var_five[which(tib$var_one==input$var1 &
                                                                 tib$var_two==input$var2 &
                                                                 tib$var_three==input$var3 &
                                                                 tib$var_four==input$var4)])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var5",
      choices = choice_var5,
      selected=head(sort(unique(as.character(tib$var_five[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2 &
                                                                  tib$var_three==input$var3 &
                                                                  tib$var_four==input$var4)]))),1)
    )
  })
  
  observeEvent(input$var5,{
    choice_var6 <- sort(unique(as.character(tib$var_six[which(tib$var_one==input$var1 &
                                                                tib$var_two==input$var2 &
                                                                tib$var_three==input$var3 &
                                                                tib$var_four==input$var4 &
                                                                tib$var_five==input$var5)])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var6",
      choices = choice_var6,
      selected=head(sort(unique(as.character(tib$var_six[which(tib$var_one==input$var1 &
                                                                 tib$var_two==input$var2 &
                                                                 tib$var_three==input$var3 &
                                                                 tib$var_four==input$var4 &
                                                                 tib$var_five==input$var5)]))),1)
    )
  })
  
  observeEvent(input$var6,{
    choice_var7 <- sort(unique(as.character(tib$var_seven[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2 &
                                                                  tib$var_three==input$var3 &
                                                                  tib$var_four==input$var4 &
                                                                  tib$var_five==input$var5 &
                                                                  tib$var_six==input$var6)])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var7",
      choices = choice_var7,
      selected=head(sort(unique(as.character(tib$var_seven[which(tib$var_one==input$var1 &
                                                                   tib$var_two==input$var2 &
                                                                   tib$var_three==input$var3 &
                                                                   tib$var_four==input$var4 &
                                                                   tib$var_five==input$var5 &
                                                                   tib$var_six==input$var6)]))),1)
    )
  })
  
  observeEvent(input$var7,{
    choice_var8 <- sort(unique(as.character(tib$var_eight[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2 &
                                                                  tib$var_three==input$var3 &
                                                                  tib$var_four==input$var4 &
                                                                  tib$var_five==input$var5 &
                                                                  tib$var_six==input$var6 &
                                                                  tib$var_seven==input$var7)])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var8",
      choices = choice_var8,
      selected=tail(sort(unique(as.character(tib$var_eight[which(tib$var_one==input$var1 &
                                                                   tib$var_two==input$var2 &
                                                                   tib$var_three==input$var3 &
                                                                   tib$var_four==input$var4 &
                                                                   tib$var_six==input$var6 &
                                                                   tib$var_seven==input$var7)]))),1)
    )
  })
  
  observeEvent(input$var8,{
    choice_var9 <- sort(unique(as.character(tib$var_nine[which(tib$var_one==input$var1 &
                                                                  tib$var_two==input$var2 &
                                                                  tib$var_three==input$var3 &
                                                                  tib$var_four==input$var4 &
                                                                  tib$var_five==input$var5 &
                                                                  tib$var_six==input$var6 &
                                                                  tib$var_seven==input$var7 & 
                                                                  tib$var_eight==input$var8 )])))
    
    updateSelectizeInput(
      session = session,
      inputId = "var9",
      choices = choice_var9,
      selected=tail(sort(unique(as.character(tib$var_nine[which(tib$var_one==input$var1 &
                                                                   tib$var_two==input$var2 &
                                                                   tib$var_three==input$var3 &
                                                                   tib$var_four==input$var4 &
                                                                   tib$var_six==input$var6 &
                                                                   tib$var_seven==input$var7 &
                                                                   tib$var_eight==input$var8)]))),1)
    )
  })
  
  colnames(my_table_df) =c("var_one","var_six","var_four","var_seven","var_eight","var_five","var_two","var_three","var_nine")
  colnames(my_table_df) =c("EXP",   "Variable","Run_type","Metric","   submetric","runs",   "period",   "Area    fc_length")
  
  # Generate filename based on selected values
  filename <- reactive({
    paste(input$var6,input$var4,input$var7,input$var8,input$var5,input$var2,input$var3,
          input$var9,sep="-")
  })
  # Get the path to the PNG file
  png_path <- reactive({
    as.character(normalizePath(file.path(paste0(file.path(basedir),'/',input$var1,'/', filename(),'-NA.png'))))
  })
  
  
  #   Read PNG file
  png_file <- reactive({
    readPNG(png_path())
  })
  
  # Display PNG file
  observe({
    output$png_image <- renderImage({
      list(src = as.character(png_path()),
           width = "1000",
           contentType = "image/png",
           alt = png_path)
    }, deleteFile = FALSE)
  })
  
  observeEvent(png_path(),{ 
    output$subdirText <- renderPrint({
      # Print selected subdirectory
      paste("Selected File:", png_path())
    })
  })
  
}
