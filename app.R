# Packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(kunakapir)
library(vchartr)

# Interface
ui <- page_navbar(
  title = "Qualidade do Ar - Belém COP30",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'logos.png\' align=\'right\' height = \'70px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Painel",

    # Card
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(
          open = "closed",
          selectInput(
            inputId = "indicators",
            label = "Indicadores",
            choices = NULL,
            multiple = TRUE
          ),
          numericInput(
            inputId = "readings",
            label = "Últimas leituras",
            min = 1,
            max = 4000,
            step = 1,
            value = 500
          ),
          tags$caption("A estação realiza uma leitura a cada 10 minutos.")
        ),
        vchartOutput(outputId = "graph")
      )
    )
  ),

  # About page
  nav_panel(
    title = "Sobre",
    card(
      card_header("Card title"),
      p("Bla bla bla.")
    ),
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Título A",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título B",
        p("Bla bla bla.")
      ),
      accordion_panel(
        "Título C",
        p("Bla bla bla.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  updateSelectInput(
    inputId = "indicators",
    choices = list_elements(device_id = "0325280630") |> unlist(),
    selected = c("PM2.5", "O3 GCc")
  )

  output$graph <- renderVchart({
    req(input$indicators)
    req(input$readings)

    # Refresh every 1 minute
    invalidateLater(millis = 600000, session = session)

    # Get data
    res <- get_reads_multiple_until(
      device_id = "0325280630",
      elements_ids = input$indicators,
      ts = Sys.time(),
      number = input$readings
    )

    # Adjust
    res <- res |>
      arrange(ts) |>
      mutate(ts = as_datetime(ts))

    # Plot
    vchart(res) |>
      v_line(
        aes(x = ts, y = value, color = sensor_tag),
        lineLabel = list(visible = TRUE)
      ) |>
      v_scale_y_continuous(min = 0) |>
      v_specs_legend(visible = FALSE)
  })
}

shinyApp(ui, server)
