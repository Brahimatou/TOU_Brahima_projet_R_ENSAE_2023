

###Partie3:Creation d'une application shiny permettant de visualiser 
### les pays de l'Afrique et leurs faits historiques ou plans de developpement socuio-economique

###Chargement des library neccessaires pour la mise en place et le fonctionnement de l'application

library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(dbscan)
library(cluster)
library(ggplot2)

##Importation de la base sous format CSV, à noter que la base et le script sont 
## tous logés dansle meme repertoire
###On importe la base dans un objet appelée base TOU
base_tou <- read.csv("ACLED-Western_Africa.csv")


###Regroupement des données en clusters nous pemrettant de les mettre dans de groupe homogènes
clusters <- dbscan(base_tou[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
base_tou$cluster <- as.factor(clusters$cluster)

## La variable année etant un outil de filtrage, on procède à la verification de son existence
##Pour cela, on utilise la fonction if pour tester

if (!"annee" %in% colnames(base_tou)) {
  stop("La variable 'annee' n'est pas présente dans les données.")
}

###Il  ressort que la variable année est bien presente dans notre base

# Chargement des library devrant intervenir dans la preparation de l'interface utilisateurUI
library(shinydashboard)## pour les dashebor
library(shiny)### concernat le shiny
library(leaflet)## pour les cartes


###Remarque important:Dans une application, nous avons deux parties importantes et c
## complementaires, la partie UI ou interface utilisateur qui  est responsable de l'interaction avec les utilisateurs et de la présentation des données
## et la partie server le serveur gère le traitement des données, les opérations métier et la communication avec d'autres services ou serveurs


# 1.Interface  de l'utilisateur:Nous utiliserons un dashebord pour preparer ce environemmment

ui <- dashboardPage(
  dashboardHeader(title = "TOU application shiny"),### titre qui figurera dans l'en-tete
  dashboardSidebar(
    sidebarMenu(
      ##
      menuItem("Pays du monde et leurs évenements marquants", icon = icon("globe-americas"), tabName = "map_tab"),
      menuItem("Selection des évenements", icon = icon("filter"), tabName = "filter_tab")
    )
  ),
  dashboardBody(
    # Dans cette partie, nous definirons les thèmes du tableau de bord
    skin = "orange",
    
    tabItems(
      # Onglet - Carte du monde par évènements
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Choisir un pays",
            checkboxGroupInput("event_filter", "Choisir un pays :", choices = unique(base_tou$pays), selected = unique(base_tou$pays)),
            br(),
            actionButton("filter_button", "Filtrer", icon = icon("search"))
          ),
          box(
            title = "CARTOGRAPHIE DES PAYS DE L'AFRIQUE OCCIDENTALE",
            leafletOutput("map")
          )
        )
      ),
      
      ###Dans cettepartie, nous creerons l'onglet après que l'utilisatuer ait se
      ## selcetionnée un événement
      tabItem(
        tabName = "filter_tab",
        fluidRow(
          box(
            title = "SELECTIONNER DES FAITS MARQUANTS",##titrisation de l'onglet
            selectInput("country_filter", "Sélectionner un pays :", choices = unique(base_tou$pays),multiple = TRUE),## une liste deroulante 
            ##permettant àl'utilisateur de selectionnerun pays ou il souhaite visualier ces évenements
            br(),
            ###Après le choix du pays,l'utilisateur descend et l'onglet suivant est une liste deoulant
            ##comportant les types d'evenements à choisir, c'est ce qui decrit le code suivant
            selectInput("event_type_filter", "Choisir un type événement :", choices = unique(base_tou$type),multiple = TRUE),
            br(),
            sliderInput("year_filter", "Choisir une année  :", min = min(base_tou$annee), max = max(base_tou$annee), value = c(min(base_tou$annee), max(base_tou$annee)))
          ),
          box(
            title = "Répresentation spatiale filtrée",### La carte obtenue est isssue du choix du pays selectionner par l'utilisateur
            leafletOutput("filtered_map")
          )
        )
      )
    )
  )
)



# Partie Server: qui recupera des instruction, stokera les information et sera en interaction avec d'autres serveurs
server <- function(input, output, session) {
  
  # Carte de l'Afrique de l'Ouest
  # La carte est celle des pays de l'Afrique Occidentale
  output$map <- renderLeaflet({
    filtered <- subset(base_tou, pays %in% input$event_filter)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Filtrage des événements:choix des évéments
  filteredData <- reactive({
    subset(base_tou, pays == input$country_filter & type == input$event_type_filter & annee >= input$year_filter[1] & annee <= input$year_filter[2])
  })
  
  # Carte filtréeaprès lechoix du pays et de l'evenement recherché
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
}

### Demarrage de l'application en utilisant la fonction shiny qui prendra comme paramètre
### deux paramètres ui donnant qui est l'interface utilisateur et server quiou est logé les informations 
shinyApp(ui = ui, server = server)



















