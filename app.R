suppressMessages(library(shiny))
suppressMessages(library(leaflet))
suppressMessages(library(dplyr))
suppressMessages(library(htmltools))
suppressMessages(library(sf))


#### Input data ####
trace <- st_read("www/data/trace.geojson")
trace$longueur <- trace %>% st_transform(2154) %>% st_length()


#### Infobulles ####
trace$tooltip <- sprintf(
  "<strong>Trace : </strong>%s<br>
  <strong>Longueur : </strong>%s km<br>",
  trace$name, round(trace$longueur/1000, 1)
) %>% lapply(htmltools::HTML)
pal <- colorFactor(
  palette = c('red', 'blue', 'purple',  'brown'),
  domain = trace$name
)

####  UI  ####
ui <- fluidPage(

  # Chargement du CSS
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/default.css")),
  tags$style(HTML("
  .sidebar-panel {
    position: absolute;
    top: 30px;
    right: 30px;
    width: 300px;
    background-color: rgba(255,255,255,0.9);
    padding: 15px;
    border-radius: 10px;
    box-shadow: 0px 0px 10px rgba(0,0,0,0.3);
    z-index: 1000;
  }
  .sidebar-panel h4 {
    margin-top: 0;
  }
")),

  # Création de la layout
  navbarPage("Tour du Viso", id = "main",
    tabPanel("Carte du parcours",
      div(class = "outer",
        leafletOutput(
          outputId = "mymap",
          height = "100%",
          width = "100%"
        ),
        div(class = "sidebar-panel",
          h3("Transport"),
          br(),
          h4("Aller le jeudi 26 juin"),
          tags$ul(
            tags$li(strong("Train de nuit :"), " départ Gare d'Austerlitz à 20h57, arrivée à Mont-Dauphin Guillestre à 07h50 le lendemain"),
            tags$li(strong("Navette :"), " départ de la gare à 08h10, arrivée à Ristolas vers 09h30"),
          ),
          h4("Retour le lundi 30 juin"),
          tags$ul(
            tags$li(strong("Navette :"), " départ de Ristolas à 18h00, arrivée à la gare vers 19h20"),
            tags$li(strong("Train de nuit :"), "départ Mont-Dauphin Guillestre à 20h27, arrivée Gare d'Austerlitz à 06h54 le lendemain"),
          ),
          h3("Hébergements"),
          tags$ul(
            tags$li(strong("Vendredi soir :"), " Nuit au ", tags$a(href="https://www.refuges.info/point/2959/refuge-garde/refuge-de-Granero/", target="_blank", "refuge Granero")),
            tags$li(strong("Samedi soir :"), " Nuit au ", tags$a(href="https://www.refuges.info/point/2015/refuge-garde/queyras/refuge-quintino-sella/", target="_blank", "refuge Quintino Sella")),
            tags$li(strong("Dimanche soir :"), " Nuit au ", tags$a(href="https://www.refuges.info/point/2014/refuge-garde/refuge-Vallanta-Valante/", target="_blank", "refuge Vallanta")),
          ),
          h3("Autres infos"),
          tags$ul(
            tags$li("Prévoir un sac à viande pour les nuits en refuge"),
            tags$li("Prévoir de l'argent liquide pour les refuges (pas de CB)"),
            tags$li("Les douches sont payantes dans les refuges (~4€ les 20L)"),
            tags$li("Les bâtons de randonnée sont fortement conseillés"),
          )
        )
      )
    )
  )
)


#### Server ####
server <- function(input, output, session) {

  ## La carto
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    setView(lng = 7, lat = 44.7, zoom = 11) %>%
    
    addTiles("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",options = WMSTileOptions(tileSize = 256),group = "Plan IGN") %>%
    addTiles(group="OSM") %>%
    addTiles("https://a.tile.opentopomap.org/{z}/{x}/{y}.png", group = "OpenTopoMap") %>%
    addTiles("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}", options = WMSTileOptions(tileSize = 256),group = "Orthos") %>%      


    addPolylines(
      data = trace,
      stroke = TRUE,
      dashArray =  "5",
      color = ~pal(name),
      group = "Trace",
      weight = 5,
      popup = trace$tooltip,
      label = trace$tooltip,
      highlightOptions = highlightOptions(
        color = "#b16694", 
        weight = 3,
        bringToFront = TRUE
      )
    ) %>%

    addMeasure(
      position = "topleft",
      primaryLengthUnit="kilometers", 
      primaryAreaUnit = "sqmeters",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    ) %>%

    addLayersControl(
      baseGroups = c("Plan IGN", "OSM", "OpenTopoMap", "Orthos"),
      overlayGroups = c("Trace"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    )
    
  })
}

shinyApp(ui, server)