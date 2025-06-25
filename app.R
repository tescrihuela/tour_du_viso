library(shiny)
library(bs4Dash)
library(leaflet)
library(dplyr)
library(htmltools)
library(sf)

#### Données ####
trace <- st_read("www/data/tour_du_viso.geojson", quiet = TRUE)
trace$longueur <- trace %>% st_transform(2154) %>% st_length()

trace$popup <- sprintf(
  "<div style='width:500px;'>
     <img src='img/%s.png' style='width:100%%; max-width:100%%; margin-top:5px;'/>
   </div>",
  trace$name
) %>% lapply(htmltools::HTML)

trace$tooltip <- sprintf(
  "<strong>Trace : </strong>%s<br>
   <strong>Longueur : </strong>%s km<br>",
  trace$name, round(trace$longueur / 1000, 1)
) %>% lapply(htmltools::HTML)

pal <- colorFactor(
  palette = c('red', 'blue', 'purple', 'brown'),
  domain = trace$name
)

#### UI ####
ui <- bs4DashPage(
  title = "Tour du Viso",
  fullscreen = TRUE,

  # ✅ Ajout du header obligatoire
  header = bs4DashNavbar(title = "Tour du Viso"),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    title = "Infos pratiques",
    bs4SidebarMenu(
      bs4SidebarHeader("Informations"),
      bs4SidebarMenuItem(
        text = "Infos importantes",
        icon = icon("circle-info"),
        tabName = "infos"
      ),
      bs4SidebarHeader("Transport"),
      bs4SidebarMenuItem(
        text = "Aller le jeudi 26 juin",
        icon = icon("train"),
        tabName = "aller",
        badgeLabel = "Départ",
        badgeColor = "primary"
      ),
      bs4SidebarMenuItem(
        text = "Retour le lundi 30 juin",
        icon = icon("train"),
        tabName = "retour",
        badgeLabel = "Retour",
        badgeColor = "warning"
      ),
      bs4SidebarHeader("Hébergements"),
      bs4SidebarMenuItem(
        text = "Refuges",
        icon = icon("bed"),
        tabName = "refuges"
      ),
      bs4SidebarHeader("Autres"),
      bs4SidebarMenuItem("Téléchargement GPX", icon = icon("download"), tabName = "download")
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet.locatecontrol/dist/L.Control.Locate.min.css"),
      tags$script(src = "https://unpkg.com/leaflet.locatecontrol/dist/L.Control.Locate.min.js"),
      tags$script(src = "https://unpkg.com/leaflet.fullscreen@1.6.0/Control.FullScreen.js"),
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet.fullscreen@1.6.0/Control.FullScreen.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/default.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
    ),
    
    bs4TabItems(
      bs4TabItem(
        tabName = "aller",
        h4("Aller le jeudi 26 juin"),
        tags$ul(
          tags$li(strong("Train de nuit : "), "Départ Gare d'Austerlitz à 20h57, arrivée à Mont-Dauphin Guillestre à 07h50 le lendemain"),
          tags$li(strong("Navette : "), "Départ 08h10, arrivée Ristolas 09h30")
        )
      ),
      
      bs4TabItem(
        tabName = "retour",
        h4("Retour le lundi 30 juin"),
        tags$ul(
          tags$li(strong("Navette : "), "Départ Ristolas à 18h00, arrivée gare à 19h20"),
          tags$li(strong("Train de nuit : "), "Départ 20h27, arrivée Gare d'Austerlitz à 06h54 le lendemain")
        )
      ),
      
      bs4TabItem(
        tabName = "refuges",
        h4("Refuges"),
        tags$ul(
          tags$li("Vendredi soir : ", tags$a("Refuge Granero", href="https://www.refuges.info/point/2959/refuge-garde/refuge-de-Granero/", target="_blank")),
          tags$li("Samedi soir : ", tags$a("Refuge Quintino Sella", href="https://www.refuges.info/point/2015/refuge-garde/queyras/refuge-quintino-sella/", target="_blank")),
          tags$li("Dimanche soir : ", tags$a("Refuge Vallanta", href="https://www.refuges.info/point/2014/refuge-garde/refuge-Vallanta-Valante/", target="_blank"))
        )
      ),
      
      bs4TabItem(
        tabName = "infos",
        h4("Infos importantes"),
        tags$p("La liste de matériel est disponible ", tags$a(href="https://lite.framacalc.org/cgqcvr3l2f-af88", "ici", target="_blank"))
      ),
      
      bs4TabItem(
        tabName = "download",
        h4("Téléchargement"),
        downloadButton("download_trace", "Télécharger la trace (GPX)")
      )
    ),
    
    fluidRow(
      box(
        title = NULL,
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        leafletOutput("mymap", height = "80vh")
      )
    )
  )
)

#### Server ####
server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = 7, lat = 44.7, zoom = 11) %>%
      addTiles("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}", options = WMSTileOptions(tileSize = 256), group = "Plan IGN") %>%
      addTiles(group = "OSM") %>%
      addTiles("https://a.tile.opentopomap.org/{z}/{x}/{y}.png", group = "OpenTopoMap") %>%
      addTiles("https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}", options = WMSTileOptions(tileSize = 256), group = "Orthos") %>%
      
      addPolylines(
        data = trace,
        stroke = TRUE,
        dashArray = "5",
        color = ~pal(name),
        group = "Trace",
        weight = 5,
        popup = trace$popup,
        label = trace$tooltip,
        highlightOptions = highlightOptions(
          color = "#b16694",
          weight = 3,
          bringToFront = TRUE
        )
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      ) %>%
      addLayersControl(
        baseGroups = c("Plan IGN", "OSM", "OpenTopoMap", "Orthos"),
        overlayGroups = c("Trace"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          if (L.control.fullscreen) {
            L.control.fullscreen({
              position: 'topright',
              title: 'Plein écran',
              titleCancel: 'Quitter le plein écran',
              forceSeparateButton: true
            }).addTo(map);
          } else {
            console.warn('Leaflet.fullscreen non chargé');
          }
        }
      ") %>% 
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          L.control.locate({
            position: 'topright',
            strings: {
              title: 'Ma position'
            },
            locateOptions: {
              enableHighAccuracy: true
            }
          }).addTo(map);
        }
      ")
  })

  output$download_trace <- downloadHandler(
    filename = function() {
      "tour_du_viso.gpx"
    },
    content = function(file) {
      trace_clean <- trace %>% st_transform(4326) %>% select(name, geometry) %>% st_union()
      st_write(trace_clean, file, driver = "GPX", delete_dsn = TRUE, layer_options = "FORCE_GPX_TRACK=YES")
    }
  )
}

shinyApp(ui, server)
