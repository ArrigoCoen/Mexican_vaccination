library(shiny)


source("Fn_epidemiologic.R")

load(file="Variables_Vaccine.RData")

anhos <- 2003:2019
keyword <- "Asma"
keyword <- "Hepatitis vírica A"
keyword <- ""
keyword <- ""
keyword <- "Fiebre tifoidea"      # WORKS
keyword <- "Tétanos"  # WORKS
keyword <- "Cólera"   # WORKS

# load(file="Test_fast_data.RData")

keywords <- c("Fiebre tifoidea","Tétanos","Cólera")
keywords <- l_PDF$keyword_Patty
# OBS: COMENTAR LA SIGUIENTE LINEA PARA TENER INFO DE TODAS LAS ENFERMEDADES
keywords <- sub_keywords

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Datos Morbilidad"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      p("Los siguientes datos fueron obtenidos de los Anuarios de Morbilidad."),
      # Input: Select a dataset ----
      selectInput("dataset", "Enfermedad:",
                  choices = keywords,selected = "Influenza                     "),
      # selectInput("dataset", "Enfermedad:",
      #             choices = keywords,selected = "Tuberculosis respiratoria"),
      
      # Column to plot, this represent:
      # "Total"=2,"Tasa"=3,
      # "<1"=4,"1-4"=5,"5-9"=6,"10 - 14"=7,
      # "15 - 19"=8, "20 - 24"=9, "25 - 44"=10, 
      # "45 - 49"=11, "50 - 59"=12, "60 - 64"=13,
      # "65 y +"=14
      numericInput("obs", "Columna a graficar:", 2),
      
      # selectInput("obs2", "Columna a graficar:",
      #             choices = list("Total"=2,"Tasa"=3,
      #                            "<1"=4,"1-4"=5,"5-9"=6,"10 - 14"=7,
      #                            "15 - 19"=8, "20 - 24"=9, "25 - 44"=10, 
      #                            "45 - 49"=11, "50 - 59"=12, "60 - 64"=13,
      #                            "65 y +"=14),selected = 2),
      
      # Input: Specify the number of observations to view ----
      numericInput("anho1_barplot", "Primer año graf. barras:",value = 2003,min = 2003,max = 2019,step = 1),
      numericInput("anho2_barplot", "Segundo año graf. barras:",value = 2010,min = 2003,max = 2019,step = 1),
      numericInput("anho3_barplot", "Tercero año graf. barras:",value = 2019,min = 2003,max = 2019,step = 1),
      
      checkboxGroupInput("i_Influenza_vec", "Suma de datos de Influenza:",choices = list("Influenza "=1,
                                                                                         "Influenza A(H1N1), 2009 identificado "=2,
                                                                                         "Influenza debida a virus de la influenza identificado"=3,
                                                                                         "Influenza debida a virus no identificado          "=4),
                         selected = 1:4),
      
      
      checkboxGroupInput("vec_partition_bars", "Particion Fig_3:",choices = list("<1"=1,"1-4"=2,"5-9"=3,"10 - 14"=4,
                                                                                 "15 - 19"=5, "20 - 24"=6, "25 - 44"=7, 
                                                                                 "45 - 49"=8, "50 - 59"=9, "60 - 64"=10,
                                                                                 "65 y +"=11 ),
                         selected = 1:11),
      
      # Include clarifying text ----
      helpText("Nota: si no se grafica puede ser por que no se pudieron extraer",
               "los datos del pdf o por que no eligieron un numero entre 2 y 15 para la columna.",
               "La tasa puede variar con respecto a la enfermedad (consultar el pdf). Estos datos",
               "fueron extraidos de los "),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      # actionButton("update", "Update View"),
      
      p("Descargar Fig_1, grafica principal."),
      downloadButton('foo'),
      # downloadButton('Download_x_grp_edad'),
      
      p("Descargar Fig_2 por grupo de edad"),
      downloadButton('Download_x_grp_edad')
      
    ),  # END OF SIDE BAR
    
    # END OF SIDE BAR
    # END OF SIDE BAR
    # END OF SIDE BAR
    # END OF SIDE BAR
    # END OF SIDE BAR
    # END OF SIDE BAR
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Header + summary of distribution ----
      h4("Comportamiento Gráfico"),
      # h4(i_Influenza_vec),
      # verbatimTextOutput("summary"),
      
      plotOutput(outputId = "plot1"),
      # plotOutput(outputId = "T_percentage"),
      plotOutput(outputId = "barplot_3years"), # three years of info
      # Output: Header + table of distribution ----
      h4("Datos de Morbilidad por grupo de edad"),
      tableOutput("view"),
      h4("Datos de Morbilidad por grupo de edad (PORCENTAGES)"),
      tableOutput("T_percentage"),
      plotOutput(outputId = "plot_all_columns")
    )
    
  )
)



# HACER UN MATPLOT POR GRUPO DE EDAD

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  
  # keyword <- input$dataset
  # m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$i_Influenza_vec <- renderText({
    # paste("My Name is :", input$name)
    input$i_Influenza_vec
  })
  
  # Table with vaccine data
  output$view <- renderTable({
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    print(m_enfermedad)
  })
  
  
  # Table with vaccine data
  output$T_percentage <- renderTable({
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    # m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    m_enfer_per = extrae_m_enfer_per(keyword,i_Influenza_vec,anhos,l_PDF)
    
    print(m_enfer_per)
  })
  
  # First plot of all data w/r to column
  output$plot1 <- renderPlot({
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    plot(m_enfermedad[,1],m_enfermedad[,input$obs],type = "l",col="blue",
         xlab="Año",ylab = colnames(m_enfermedad)[input$obs],main = input$dataset)
  })
  
  
  # Now we plot the bar plot of 3 years of data
  output$barplot_3years <- renderPlot({
    keyword <- input$dataset
    vec_partition_bars <- input$vec_partition_bars
    (three_years <- c(input$anho1_barplot,input$anho2_barplot,input$anho3_barplot))
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    plot_3years_barplot(three_years,m_enfermedad,vec_partition_bars)
    # my_barplot_age_blocks(datos_anho,year,vec_partition_bars) 
  }) 
  # Main plot de casos por coluna
  output$plot_all_columns <- renderPlot({
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    # m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    # # We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
    # matplot(m_enfermedad[,1],m_enfermedad[,c(-1:-3,-15)],type = "l",ylab = "Todos los grupos de edad",
    #         xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))
    # 
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    # We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
    m_enfermedad_only_GROUPS <- m_enfermedad[,c(-1:-3,-15)]
    years <- m_enfermedad[,1]
    matplot(years,m_enfermedad_only_GROUPS,type = "l",ylab = "Número de casos",
            xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))
    
  })
  
  # Dowload principal plot
  plotInput = function() {
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    plot(m_enfermedad[,1],m_enfermedad[,input$obs],type = "l",col="blue",
         xlab="Año",ylab = colnames(m_enfermedad)[input$obs],main = input$dataset)
    # plot(1:10,sin(1:10))
  }
  output$foo = downloadHandler(
    filename = paste0(c("Main_plot_","enfermedad",".png"),collapse = ""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  
  
  
  # Dowload comportamiento por grupo de edad plot
  plotInput_x_grupo = function() {
    keyword <- input$dataset
    i_Influenza_vec <- as.numeric(input$i_Influenza_vec)
    # m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    # # We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
    # matplot(m_enfermedad[,1],m_enfermedad[,c(-1:-3,-15)],type = "l",ylab = "Todos los grupos de edad",
    #         xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))
    # 
    m_enfermedad <- GEN_m_enfermedad_w_INFLUENZA(keyword,i_Influenza_vec,anhos,l_PDF)
    # We eliminate the values of columns c(-1:-3,-15) = Year,Tatal,Rate, Ign.
    m_enfermedad_only_GROUPS <- m_enfermedad[,c(-1:-3,-15)]
    years <- m_enfermedad[,1]
    matplot(years,m_enfermedad_only_GROUPS,type = "l",ylab = "Número de casos",
            xlab = "Año",main=paste0(c("Comportamiento de todos los grupos de edad por año de ",keyword),collapse = ""))
    
    
  }
  output$Download_x_grp_edad = downloadHandler(
    filename = paste0(c("Plot_por_grupo_edad_","enfermedad",".png"),collapse = ""),
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput_x_grupo(), device = device)
    })
  
}

# Create Shiny app ----
shinyApp(ui, server)
