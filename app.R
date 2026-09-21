library("shiny")
library("bslib")
library("tidyverse")
library("DT")

data <- read.table('data/productiedata.dat',
                   sep = ",",
                   dec = ".",
                   na.strings = "NA") |> 
  rename(vreettijd = 15) |> 
  rename_with(tolower) |> 
  mutate(datum = parse_date_time(datum, orders = "%Y-%m-%d")) |> 
  mutate(id = factor(id))

km <- colnames(data)[-c(1:4)]

inputs <- list(
  selectInput(inputId = "kenmerk", label = "Selecteer kenmerk(en)", choices = km, selected = km[1], multiple = T),
  selectInput(inputId = "dieren", label = "Selecteer dier(en)", choices = unique(data$id), multiple = T),
  sliderInput(inputId = 'lacts', label = "Laktaties:", min = min(data$lactatie, na.rm = T), max = max(data$lactatie, na.rm = T), step = 1, value = c(1,3)),
  sliderInput(inputId = 'dims', label = "Laktatiedagen:", min = min(data$dim, na.rm = T), max = max(data$dim, na.rm = T), step = 1, value = c(1,365)),
  hr(),
  # Extra: Downloadknop toegevoegd aan de zijbalk
  downloadButton("download_data", "Download gefilterde data", class = "btn-outline-primary btn-sm w-100")
)

cards <- list(
  # Extra: Waarde-boxen voor snelle statistieken bovenaan
  # Waarde-boxen voor snelle statistieken bovenaan
  layout_column_wrap(
    width = 1/2,
    value_box(
      title = "Geselecteerde Rijen", 
      # OPLOSSING: Verpak de output in een span met een aangepaste lettergrootte zodat het altijd past
      value = tags$span(textOutput("stat_rijen"), style = "font-size: clamp(1.5rem, 4vw, 2.5rem); font-weight: bold;"), 
      showcase = shiny::icon("database"),
      theme = "primary"
    ),
    value_box(
      title = "Geselecteerde Dieren", 
      value = tags$span(textOutput("stat_dieren"), style = "font-size: clamp(1.5rem, 4vw, 2.5rem); font-weight: bold;"), 
      showcase = shiny::icon("cow"), 
      theme = "teal"
    )
  ),
  

  card(
    full_screen = TRUE,
    card_header("Kenmerken over de tijd"),
    card_body(plotOutput("grafiek"))
  ),
  card(    
    full_screen = TRUE,
    card_header("Tabel van de gegevens"),
    card_body(fillable = TRUE, DTOutput("tabel"))
  )
)

ui = page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  title = "Analyse productiedata",
  sidebar = sidebar(inputs),
  
  nav_panel(
    title = "Dashboard",
    cards
  ),
  
  # Dit zorgt dat de links strak aan de rechterkant belanden
  nav_spacer(),
  
  nav_item(tags$a(shiny::icon("github"), " GitHub", href = "https://github.com/costerAnalytics/wsappproductie", target = "_blank", style = "color: inherit; text-decoration: none;")),
  nav_item(tags$a(shiny::icon("globe"), " Website", href = "https://github.com/costerAnalytics/wsappproductie", target = "_blank", style = "color: inherit; text-decoration: none; margin-left: 15px;"))
)



server = function(input, output,session) {
  dt <- reactive({ 
    req(input$lacts, input$dims, input$kenmerk) 
    
    data |> 
      filter(
        # Veiligheidschecks op basiskolommen
        productie > 0 &
          dim > 0 &
          lactatie > 0 &
          # Slider filters corrigeren: gebruik [1] voor min en [2] voor max!
          lactatie >= input$lacts[1] &
          lactatie <= input$lacts[2] &
          dim >= input$dims[1] &
          dim <= input$dims[2] & 
          (is.null(input$dieren) | id %in% input$dieren)
      ) |> 
      select(c(colnames(data)[1:4],input$kenmerk))
  })
  
  output$grafiek <- renderPlot({
    req(dt(), nrow(dt()) > 0)
    
    # 1. Pivot en bereid data voor de grafiek voor
    plot_data <- dt() |> 
      pivot_longer(
        cols = all_of(input$kenmerk), 
        values_to = "waarde",
        names_to = "kenmerk"
      ) |> 
      filter(!is.na(waarde), waarde > 0)
    
    # 2. Bepaal de logica op basis van wel/geen dierselectie
    if (is.null(input$dieren)) {
      # GEEN DIEREN GESELECTEERD -> Bereken groepsgemiddelde per dag
      plot_data <- plot_data |> 
        group_by(datum, kenmerk) |> 
        summarise(waarde = mean(waarde, na.rm = TRUE), .groups = "drop")
      
      mapping <- aes(x = datum, y = waarde, color = kenmerk, group = kenmerk)
      grafiek_titel <- "Groepsgemiddelde over de tijd"
    } else {
      # WEL DIEREN GESELECTEERD -> Bereken het gemiddelde PER DIER per dag 
      # (indien een dier meerdere metingen per dag heeft, anders tekent hij ze direct)
      plot_data <- plot_data |> 
        group_by(id, datum, kenmerk) |> 
        summarise(waarde = mean(waarde, na.rm = TRUE), .groups = "drop")
      
      # ID omzetten naar factor zorgt voor losse, unieke lijnen per dier in de legenda
      mapping <- aes(x = datum, y = waarde, color = id, group = id)
      grafiek_titel <- paste("Individueel verloop per geselecteerd dier")
    }
    
    # 3. grafiek
    ggplot(plot_data, mapping) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(size = 1.5) +
      theme_minimal() +
      labs(
        title = grafiek_titel,
        x = "Datum", 
        y = "Waarde",
        color = if(is.null(input$dieren)) "Kenmerk" else "Dier ID"
      ) +
      # rafieken per kenmerk als er meerdere gekozen zijn
      facet_wrap(~kenmerk, scales = "free_y")
  })
  
  
  # DataTable output genereren binnen de bslib card
  output$tabel <- renderDT({
    req(dt())
    
    # Zoek de exacte positie van de kolom 'datum' op (DT telt vanaf 0!)
    # Dit voorkomt fouten als de volgorde van je kolommen verschuift door de pivot_longer
    datum_index <- which(colnames(dt()) == "datum")
    id_index    <- which(colnames(dt()) == "id")
    # 3. Maak een lijst van alle kolomindexen die NIET de ID-kolom zijn
    geen_id_indexen <- which(colnames(dt()) != "id")
    
    datatable(
      dt() |> 
        arrange(id,datum),
      style = "bootstrap5",
      fillContainer = TRUE, 
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 'ltp',
        autoWidth = TRUE, # Verplicht om handmatige breedtes toe te staan
        columnDefs = list(
          
          list(
            targets = datum_index, # De index van de datumkolom
            width = '150px',       # Pas dit getal aan om hem breder of smaller te maken
            className = 'dt-left'  # Zorgt dat de datum netjes links uitlijnt
          )
        )
      )
    ) |> 
      # Formatteer de datumkolom naar Nederlands formaat (DD-MM-YYYY)
      formatDate(
        columns = "datum", 
        method = "toLocaleDateString",
        params = list(
          locales = "nl-NL", 
          options = list(
            year = "numeric", 
            month = "2-digit", 
            day = "2-digit"
          )
        )
      )
  })
  
  # Statistiek 1: aantal rijen
  output$stat_rijen <- renderText({ nrow(dt()) })
  
  # Statistiek 2: aantal unieke dieren in selectie
  output$stat_dieren <- renderText({ n_distinct(dt()$id) })
  
  
  
  # Download handlerfunctionaliteit
  output$download_data <- downloadHandler(
    filename = function() { paste("productiedata-export-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(dt(), file, row.names = FALSE) }
  )
}
  


shinyApp(ui,server)



shinyApp(ui = ui, server = server)
