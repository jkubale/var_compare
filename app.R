library(shiny)
library(xtable)
library(shinyBS)
library(DT)

downloadObjUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary")
}

downloadObj <- function(input, output, session, data) {
  
  output$data_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      dat_hold2 <- data.frame(lapply(data(), function(x){gsub("<br>","\n",x)}))
      write.csv(dat_hold2, file) # add parentheses to data arg if reactive
    }
  )
}



dat <- data.frame(read.csv("covid_meas_long.csv"))
colnames(dat) <- c("cat", "subcat1", "concept", "study", "variable", "qtext", "i_instr", "vals", "cod_vals")




dat2 <- dat%>%
  dplyr::mutate(vals = stringr::str_replace_all(vals, "\n", "<br>"))




ui <- navbarPage(
  title=div(img(src="SBECCC_full-color2.png",
                style="margin-top: -20px;
                               padding-right:10px;
                               padding-bottom:10px",
                height = 70), "Comparing COVID-19 Questions from Large Survey Research Studies"),
  sidebarLayout(
    sidebarPanel(uiOutput("var1_select"),
                 uiOutput("var2_select"),
                 uiOutput("var3_select"),
                 uiOutput("var4_select"),
                 downloadObjUI(id = "download1")),
    mainPanel(
      bsCollapse(id = "collapseExample", open = "Comparison",
                 bsCollapsePanel("About", HTML("<p> After the start of the COVID-19 pandemic many
                                 existing survey research studies began developing 
                                 COVID-19 related questions to assess the impact of the
                                 pandemic on their study population. In an effort to 
                                 improve harmonization of measures the 
                                 <a href='https://www.icpsr.umich.edu/web/pages/sbeccc/'>Social, Behavioral, and Economic COVID Coordinating Center (SBE CCC)</a> 
                                 have developed this app to allow researchers to compare survey 
                                 questions across a range of large studies. </p>
                                 
                                 <p> Currently we include COVID-19 measures from 6 different surveys:
                                 <ul>
                                 <li><a href='https://hrs.isr.umich.edu/about'>Health and Retirement Study (HRS)</a>,</li> 
                                 <li><a href='https://www.nhats.org/researcher/nhats'>National Health and Aging Trends Study (NHATS)</a>,</li>
                                 <li><a href='https://www.cdc.gov/nchs/nhis/2022nhis.htm'>2022 National Health Interview Study (NHIS)</a>,</li> 
                                 <li><a href='https://psidonline.isr.umich.edu/'>Panel Study on Income Dynamics (PSID)</a>,</li> 
                                 <li><a href='https://www.census.gov/newsroom/press-releases/2022/household-pulse-phase-3-7.html'>Census Pulse Survey (3.7)</a>,</li> 
                                 <li><a href='https://www.maxwell.syr.edu/research/lerner-center/health-research/national-wellbeing-survey'>National Wellbeing Survey</a></li>
                                               </ul></p>
                                               
                                 <p>To compare survey questions across studies, select the topic 
                                               you are interested in and up to three studies to compare. 
                                               Visit <a href='https://www.icpsr.umich.edu/web/pages/sbeccc/data.html'>SBE CCC's website</a> to view the complete survey questionnaires 
                                               from these studies.</p>")),
                 bsCollapsePanel("Comparison",  tableOutput("reportOutput"))
      )
    ))
)

server <- function(input, output) {
  
  output$var1_select <- renderUI({
    selectInput(
      "ind_var_select1",
      "Select Category", 
      choices = as.character(dat[,1] ),
      multiple = TRUE,
      selected = NULL
    )
  })
  
  output$var2_select <- renderUI({
    selectInput(
      "ind_var_select2",
      "Select Sub-category", 
      choices = as.character(dat[dat$cat==input$ind_var_select1,2] ),
      multiple = FALSE,
      selected = NULL
    )
  })
  
  output$var3_select <- renderUI({
    selectInput(
      "ind_var_select3",
      "Select Variable",
      choices = as.character(dat[dat$subcat1==input$ind_var_select2,3] ),
      multiple = FALSE,
      selected = NULL
    )
  })
  
  output$var4_select <- renderUI({
    selectizeInput(
      "ind_var_select4",
      "Select Study",
      choices = as.character(dat[dat$concept==input$ind_var_select3,4] ),
      multiple = TRUE,
      selected = NULL,
      options = list(maxItems = 3)
    )
  })
  
  
  dat_hold <- reactive({
    # Filter it 
    validate(
      need(input$ind_var_select1 != "", "Please select a category")
    )
    
    validate(
      need(input$ind_var_select1 != "" &
             input$ind_var_select2 != "" &
             input$ind_var_select3 != "" &
             input$ind_var_select4 != "", "Please select an option for all fields")
    )
    
    
    data1 <- data.frame(subset(dat, (cat %in% input$ind_var_select1 &
                                       subcat1 %in% input$ind_var_select2 &
                                       concept %in% input$ind_var_select3 &
                                       study %in% input$ind_var_select4)))
    
    #   # Transpose so comparison is side-by-side
    data2 <- t(data1[,c(3,4,6,8)])
    data3 <- as.data.frame(data2[c(1,3,4),])
    # Create scalable (up to 3) column names
    for(i in 1:ncol(data2)){
      
      
      colnames(data3)[i] <- data2[2,i]
      
    }
    
    #    ## add row names for the displayed fields
    data4 <- data.frame(cbind(Field= c("Conceptual Variable", "Question Text", "Values"),data3))
    
    
  })
  
  ## Trying to remove HTML formatting for line breaks (for export) not currently working
  # dat_hold2 <- reactive({data.frame(lapply(isolate(dat_hold), function(x) {
  #                    gsub("<br>", "\n", x)
  #                 }))})
  
  output$reportOutput = renderTable({dat_hold()}, options = list(scrollX = TRUE), sanitize.text.function=identity)
  
  callModule(downloadObj, id = "download1", data = dat_hold)
  
}
# runApp(list(ui=ui,server=server),launch.browser = T)
shinyApp(ui, server)