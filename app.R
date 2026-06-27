library(shiny)
library(rsconnect)
library(bslib)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(DT)
source("load_data.r")
source("plots.r")
source("summary.r")
theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#2563EB",
  base_font = font_google("Poppins")
)

ui <- page_sidebar(

  theme = theme,

  title = "🩺 Disease Progression Explorer",
  p(
  class = "text-muted",

  "Interactive dashboard for exploring Chronic Kidney Disease progression."
),

  sidebar = sidebar(

    width = 300,

    h4("Filters"),

    sliderInput(
      "age",
      "Age Range",
      min(data$Age),
      max(data$Age),
      value = c(min(data$Age), max(data$Age))
    ),

    selectInput(
      "diabetes",
      "Diabetes",
      choices = c("All","Yes","No")
    ),

    selectInput(
      "hypertension",
      "Hypertension",
      choices = c("All","Yes","No")
    )

  ),


           layout_columns(

  card(

    full_screen = TRUE,

    card_header("👥 Patients"),

    h1(textOutput("patients"))

)

,

  card(

    card_header("🩺 CKD"),

    h1(textOutput("ckd")),
    full_screen=TRUE

  ),

  card(

    card_header("💉 Dialysis"),

    h1(textOutput("dialysis")),
    full_screen=TRUE

  )

),
           br(),
           layout_columns(

  card(

    full_screen = TRUE,

    card_header("Patient Age Distribution"),

    plotlyOutput("agePlot", height = "400px")

  ),

  card(

    full_screen = TRUE,

    card_header("Kidney Function"),

    plotlyOutput("bubblePlot", height = "400px")

  )


),
layout_columns(

  card(

    full_screen=TRUE,

    card_header("Creatinine Levels"),

    plotlyOutput(

      "boxPlot",

      height="400px"

    )

  ),

  card(

    full_screen=TRUE,

    card_header("Patient Records"),

    DTOutput("patientsTable")

  )

)
           
                   )


##Server
server <- function(input, output, session) {
  filtered <- reactive({
    filter_data(data, input$age, input$diabetes, input$hypertension)
  })
#KPI cards
output$patients <- renderText({
  total_patients(filtered())
})
output$ckd <- renderText({
  total_ckd(filtered())
})
output$dialysis <- renderText({
  total_dialysis(filtered())
})
#PLOTS
output$agePlot <- renderPlotly({
  age_histogram(filtered())
})
output$bubblePlot <- renderPlotly({
  kidney_bubble(filtered())
})
output$boxPlot <- renderPlotly({
  creatinine_box(filtered())
})
#Table
output$patientsTable <- renderDT({

    datatable(

        filtered(),

        options = list(

            pageLength = 10,

            scrollX = TRUE,

            autoWidth = TRUE

        ),

        rownames = FALSE

    )

})
}

shinyApp(ui, server)