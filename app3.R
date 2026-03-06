library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(tidyverse)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(flextable)
library(tools)
library(bsicons)



ui <- page_fluid(
  tags$head(
    tags$style(HTML("

     body{
      background-color: #73B77B;
      padding-top: 70px;
    }"))),
  
  titlePanel("Análisis de Buttons"),
  fileInput(inputId = "subir", NULL, buttonLabel = "Subir archivo", accept = c(".xls", ".xlsx", ".csv")),
  hr(),
  layout_columns(
    col_widths = c(6, 6),
    selectizeInput(
    inputId = "equipos",
    label = "Equipo",
    choices = NULL,
    selected = NULL
 ), 
uiOutput("selec_fecha")),
hr(),

actionBttn(inputId = "analisis", label = "ANALIZAR"),
  hr(),
  card(
    card_header(htmlOutput(outputId = "fact_coerr")),
    card_body(
      height = 600,
      withSpinner(plotlyOutput(outputId = "grafico"))
    )
  ),
  layout_columns(col_widths = c(6,6),
  value_box(title = "Máximo desvío por debajo del rango",
            value = htmlOutput(outputId = "max_desv_inf"),
            showcase = bs_icon("arrow-down")),
  value_box(title = "Máximo desvío por encima del rango",
            value = htmlOutput(outputId = "max_desv_sup"),
            showcase = bs_icon("arrow-up"))),
  hr(),
  layout_columns(
    card(
      card_header("Desvíos por debajo del límite de tolerancia"),
      card_body(uiOutput(outputId = "tabla"))
    ),
    card(
      card_header("Desvíos por encima del límite de tolerancia"),
      card_body(uiOutput(outputId = "tabla2"))
    )
  ),
  layout_columns(
    card(
      card_header("Desvíos por debajo del límite de tolerancia mayores a 1 °C"),
      card_body(uiOutput(outputId = "tabla3"))
    ),
    card(
      card_header("Desvíos por encima del límite de tolerancia mayores a 1 °C"),
      card_body(uiOutput(outputId = "tabla4"))
    )
  )
)

server <- function(input, output, session) {
  
datos_xls <- eventReactive(input$subir, {
  datos <- read_xlsx(input$subir$datapath)
  datos
})



  datos1 <- reactive(
    datos_xls() %>% mutate(EquipoD = as.factor(EquipoD), across(where(is.numeric), ~ round(., digits = 2))) %>%
      rename("Fecha" = "Eq_Date_Id")
  )
  
  equipos <- reactive(
    levels(datos1()$EquipoD)
  )
  fechas <- reactive(as.Date(datos1()$Fecha))

  
 observe({
   updateSelectizeInput(session,
                        inputId = "equipos",
                        label = "Equipos",
                        choices = equipos(),
                        selected = NULL,
                        server = TRUE )
 })
 
 output$selec_fecha <- renderUI({
   pickerInput(
     inputId = "fechas",
     label = HTML(paste0("<b style = 'font-family: Raleway, Sans-serif';>", "Seleccione las fechas", "</b>")),
     choices = sort(unique(fechas())),
     options = pickerOptions(actionsBox = TRUE,
                             dropupAuto = FALSE),
     selected = unique(fechas()),
     multiple = TRUE
   )
 })
  
  datos_equipos <-reactive({
    req(input$equipos)
    switch(input$equipos,
           "Estufa 1652" = tibble(
             factor_correc = 0.24,
             lim_sup = 29.82,
             lim_inf = 29.29
           ),
           "Estufa 654" = tibble(
             factor_correc = 0.41,
             lim_sup = 41.90,
             lim_inf = 41.20
           ))
  })
  
  equip <- reactive(input$equipos)
  
  subsetted <- reactive(
    datos1() %>% 
    filter(EquipoD == equip() & Temperatura > 0 & as.Date(Fecha) %in% input$fechas) %>% 
    mutate(Temperatura_corregida = Temperatura + datos_equipos()$factor_correc)
  )
  
  max <- reactive(
    subsetted()$Temperatura_corregida[which.max(subsetted()$Temperatura_corregida)]
  )
  
  min <- reactive(
    subsetted()$Temperatura_corregida[which.min(subsetted()$Temperatura_corregida)]
  )
  
  promedio <-  reactive(
    mean(subsetted()$Temperatura_corregida) %>% round(2)
  )
    
  desv_inf <- reactive(
    subsetted() %>%
    filter(Temperatura_corregida < datos_equipos()$lim_inf) %>%
    mutate(Desvío = Temperatura_corregida - datos_equipos()$lim_inf) %>% 
      select(EquipoD, Fecha, Temperatura_corregida, Desvío)
  )
  
  desv_sup <- reactive(
    subsetted() %>%
    filter(Temperatura_corregida > datos_equipos()$lim_sup) %>%
    mutate(Desvío = Temperatura_corregida - datos_equipos()$lim_sup) %>% 
      select(EquipoD, Fecha, Temperatura_corregida, Desvío)
  )
  
  
  desv_inf1 <-  reactive(
    desv_inf() %>% filter(Desvío < -1)
  )
  
  desv_sup1 <-reactive(
    desv_sup() %>% filter(Desvío > 1)
  )
  
  
  
  inicio <- reactive(
    subsetted()$Fecha[1]
  )
  
  fin <-  reactive(
    subsetted()$Fecha[length(subsetted()$Fecha)]
  )
  
  fecha_max_desv_sup <- reactive({
    req(desv_sup())
    desv_sup()$Fecha[which.max(desv_sup()$Desvío)]
  })
  
  max_desv_sup <- reactive({
    req(desv_sup())
    round(base::max(desv_sup()$Desvío), 2)
    })

  fecha_max_desv_inf <- reactive({
    req(desv_inf())
    desv_inf()$Fecha[which.min(desv_inf()$Desvío)]
                                 })
  
  max_desv_inf <- reactive({
    req(desv_inf())
    round(base::min(desv_inf()$Desvío), 2)
   })

  
  output$grafico <- renderPlotly({
    req(input$analisis)
    p <- subsetted() %>% ggplot(aes(.data[["Fecha"]], .data[["Temperatura_corregida"]])) +
      geom_step(color = "#2E6F40") +
      geom_hline(yintercept = datos_equipos()$lim_sup) +
      geom_hline(yintercept = datos_equipos()$lim_inf) +
      xlab("Fecha") +
      ylab("Temperatura (°C)")
    ggplotly(p, height = 500, dynamicTicks = TRUE)
  })
  
  output$fact_coerr <- renderUI({
    req(input$analisis)
    HTML(paste0(
    "<b>EQUIPO: </b>", unique(input$equipos), "</br>",
    "<b>SONDA: </b>", unique(subsetted()$IdEquipo), "</br>",
    "<b>FACTOR DE CORRECCIÓN: </b>", unique(datos_equipos()$factor_correc), "</br>",
    "<b>TEMPERATURA DE TRABAJO: </b>", unique(datos_equipos()$lim_inf), " °C ", " - ",
    unique(datos_equipos()$lim_sup), " °C", "</br>",
    "<b>INICIO DE REGISTRO: </b>", unique(inicio()), "</br>",
    "<b>FIN DE REGISTRO: </b>", unique(fin()), "</br>",
    "<b>TEMPERATURA MÁXIMA: </b>", unique(max()), " °C", "</br>",
    "<b>TEMPERATURA MÍNIMA: </b>", unique(min()), " °C", "</br>",
    "<b>TEMPERATURA PROMEDIO: </b>", unique(promedio()), " °C"
  ))})
  
  output$tabla <- renderUI({
    req(input$analisis)
    desv_inf() %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$tabla2 <- renderUI({
    req(input$analisis)
    desv_sup() %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla3 <- renderUI({
    req(input$analisis)
    desv_inf1() %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  
  output$tabla4 <- renderUI({
    req(input$analisis)
    desv_sup1() %>%
      flextable() %>%
      autofit() %>%
      htmltools_value()
  })
  output$max_desv_inf <- renderUI({
    req(input$analisis)
    HTML(paste0(max_desv_inf(), " °C - ", fecha_max_desv_inf()))
  })
  output$max_desv_sup <- renderUI({
    req(input$analisis)
    HTML(paste0(max_desv_sup(), " °C - ", fecha_max_desv_sup()))
  })

  outputOptions(output, "max_desv_inf", suspendWhenHidden = FALSE)
  outputOptions(output, "max_desv_sup", suspendWhenHidden = FALSE)

  
}  

# Run the application 
shinyApp(ui = ui, server = server)
   
