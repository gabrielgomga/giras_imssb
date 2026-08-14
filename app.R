library("pacman")

p_load (dplyr,tidyverse,sf,s2,sp,leaflet,leaflet.extras,leafem,mapview,deldir,rlang, writexl, htmlwidgets, arrow,
        hereR,classInt,readr,readxl, tidyr, scales, janitor, mapview, purrr, stringr, osrm, nngeo, geosphere, shiny, rsconnect, DT)

giras <- read_excel("Insumos/giras2026 por CLUES.xlsx", 
                    col_types = c("date", rep("text", 8)), n_max = 43) %>% clean_names()
cat_clues <- arrow::read_parquet("Insumos/clues.parquet") %>% clean_names()
dgis <- read_excel("Insumos/ESTABLECIMIENTO_SALUD_202606.xlsx") %>% clean_names()
entidad_imssb <- st_read("Insumos/entidad_concurrente.gpkg") %>% clean_names()
entidad_no_imssb <- st_read("Insumos/ent_no_concurrente_dissolv.gpkg") %>% clean_names()


# Preparar la bases a trabajar --------------------------------------------
# Reproyectar capas de entidades
entidad_imssb <- st_transform(entidad_imssb, 4326)
entidad_no_imssb <- st_transform(entidad_no_imssb, 4326)

#Revisar CRS
st_crs(entidad_imssb)

# giras <- giras %>%
#   mutate(fecha = as.Date(fecha, origin = "1899-12-30"),
#          fecha = format(fecha, "%d/%m/%Y")) %>% 
#   filter(!is.na(clues_unidad_1))

# Acomodar la matriz de abajo para arriba
giras <- giras %>%
  pivot_longer(
    cols = -fecha,
    names_to = c(".value", "unidad"),
    names_pattern = "(.*)_(\\d+)$"
  ) %>%
  filter(!is.na(clues_unidad), clues_unidad != "") %>% 
  mutate(id = sprintf("%03d", row_number())) %>% 
  rename(clues = clues_unidad,
         objetivo = objetivo_clues)

# CLUES 
cat_clues <- cat_clues %>%
  mutate(clave_de_la_entidad = sprintf("%02d", clave_de_la_entidad))

#cat_clues <- st_as_sf(cat_clues, coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)

# CLUES Campa
campa <- cat_clues %>% 
  filter(clues_imb == "DFIMB000014")%>%
  select(clues_imb, clave_de_la_entidad, entidad, nombre_de_la_unidad, longitud, latitud) %>% 
  rename(clues = clues_imb)

giras_clues <- left_join(giras, cat_clues %>% select(clues_imb, clave_de_la_entidad, nombre_de_la_unidad, longitud, latitud), 
                         by = c("clues" = "clues_imb"))

# Agregar entidad
giras_clues <- left_join(giras_clues, entidad_imssb %>% select(cve_ent, entidad),
                         by = c("clave_de_la_entidad" = "cve_ent"))

# Aislar aquellos registros en las giras que no se encontraron en clues 
giras_diferencia <- giras_clues %>% 
  filter(is.na(longitud))

# convertir columnas de coordenadas de decirmales a decimales
dgis <- dgis %>%
  mutate(
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud)
  )

# Convertir en minuculas Entidades DGIS               ## Es posible que cree un vector con los nombres de entidades para evitar errores
dgis <- dgis %>% 
  mutate(
    entidad = str_to_title(entidad),
  )

# Asignar coordenadas con catalogo DGIS 
giras_clues <- giras_clues %>%
  left_join(
    dgis %>%
      st_drop_geometry() %>%
      select(
        clues,
        clave_de_la_entidad,
        entidad,
        nombre_de_la_unidad,
        longitud,
        latitud
      ) %>%
      rename(
        clave_ent_dgis = clave_de_la_entidad,
        entidad_dgis = entidad,
        nombre_unidad_dgis = nombre_de_la_unidad,
        longitud_dgis = longitud,
        latitud_dgis = latitud
      ),
    by = "clues"
  ) %>%
  mutate(
    clave_de_la_entidad = coalesce(clave_de_la_entidad, clave_ent_dgis),
    entidad             = coalesce(entidad, entidad_dgis),
    nombre_de_la_unidad = coalesce(nombre_de_la_unidad, nombre_unidad_dgis),
    longitud            = coalesce(longitud, longitud_dgis),
    latitud             = coalesce(latitud, latitud_dgis)
  ) %>%
  select(
    -clave_ent_dgis,
    -entidad_dgis,
    -nombre_unidad_dgis,
    -longitud_dgis,
    -latitud_dgis
  )


# Aquellos que ya tienen coordenadas asignadas agregar que tipo de unidad son
giras_clues <- giras_clues %>%
  mutate(
    tipo_unidad = if_else(
      is.na(longitud),
      NA_character_,
      "Hospital"
    )
  )


# Volver a sacar la diferencia de aquellos que siguen sin coordenadas asignadas
giras_diferencia <- giras_clues %>% 
  filter(is.na(longitud))

# Giras sin CLUES (rellenar manualmente)
giras_sin_clues <- tibble::tribble(
  ~id,   ~clave_de_la_entidad, ~entidad,  ~nombre_de_la_unidad,                   ~longitud, ~latitud,     ~tipo_unidad,
  "014", "20",                 "Oaxaca",  "Palacio de Gobierno de Oaxaca",        -96.725512, 17.060018,   "Gobierno",
  "025", "27",                 "Tabasco", "Obra HG Cárdenas (CLUES no tramitada)",-93.3827863, 17.990542,  "Obra",
  "033", "15",                 "Mexico",  "Palacio de Gobierno de Toluca",        -99.657174, 19.293405,   "Gobierno",
  "038", "27",                 "Tabasco", "Obra HG Cárdenas (CLUES no tramitada)",-93.3827863, 17.990542,  "Obra",
  "046", "12",                 "Guerrero","Obra del HBC Atlixtac sin CLUES",      -98.938993, 17.561725,   "Obra")


giras_clues <- giras_clues %>%
  left_join(
    giras_sin_clues,
    by = "id",
    suffix = c("", "_manual")
  ) %>%
  mutate(
    clave_de_la_entidad = coalesce(clave_de_la_entidad, clave_de_la_entidad_manual),
    entidad             = coalesce(entidad, entidad_manual),
    nombre_de_la_unidad = coalesce(nombre_de_la_unidad, nombre_de_la_unidad_manual),
    longitud            = coalesce(longitud, longitud_manual),
    latitud             = coalesce(latitud, latitud_manual),
    tipo_unidad         = coalesce(tipo_unidad, tipo_unidad_manual)
  ) %>%
  select(-ends_with("_manual")) %>% 
  mutate(
    fecha = as.Date(fecha, format = "%d/%m/%Y")
  )

# verificar que ya no hayan NA
giras_clues %>%
  filter(is.na(longitud) | is.na(latitud))
# 
# # Pasar mayusculas a minusculas
giras_clues <- giras_clues %>% 
  mutate(
    nombre_de_la_unidad = str_to_title(nombre_de_la_unidad)
  )



# giras_clues <- giras_clues %>%
#   mutate(
#     entidad = str_to_title(entidad),
#     nombre_de_la_unidad = str_to_title(nombre_de_la_unidad)
#   )

campa <- campa %>% 
  mutate(
    entidad = str_to_title(entidad),
    nombre_de_la_unidad = str_to_title(nombre_de_la_unidad)
  )

# Agregar el id a Campa 000
campa <- campa %>%
  mutate(id = "000")

# Cambiar nombre de Campa
campa$nombre_de_la_unidad <- "IMSS BIENESTAR Dirección General"

# Acomodar las columnas
giras_clues <- giras_clues %>% 
  select(id, fecha, unidad, clues, nombre_de_la_unidad, tipo_unidad, objetivo, clave_de_la_entidad, entidad, longitud, latitud) %>% 
  rename(orden_visita = unidad)


# convertir en sf
giras_clues <- st_as_sf(giras_clues, coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)
campa <- st_as_sf(campa, coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)

# Contar cuantos valores hay en cada categoria
# giras_clues %>%
#   count(orden_visita)

# Crear tabla intermedia 
# Garantizar orden correcto
giras_od <- giras_clues %>%
  arrange(fecha, orden_visita)

# G 
giras_od <- giras_od %>%
  group_by(fecha) %>%
  arrange(orden_visita, .by_group = TRUE) %>%
  mutate(
    id_origen = lag(id),
    clues_origen = lag(clues),
    nombre_origen = lag(nombre_de_la_unidad)
  ) %>%
  ungroup()

giras_od <- giras_od %>%
  mutate(
    id_origen = if_else(
      is.na(id_origen),
      campa$id[1],
      id_origen
    ),
    clues_origen = if_else(
      is.na(clues_origen),
      campa$clues[1],
      clues_origen
    ),
    nombre_origen = if_else(
      is.na(nombre_origen),
      campa$nombre_de_la_unidad[1],
      nombre_origen
    )
  )


# Crear catalogo único de puntos 
catalogo_puntos <- bind_rows(
  campa,
  giras_clues
) %>%
  select(
    id,
    clues,
    nombre_de_la_unidad,
    geometry
  )

geom_origen <- st_geometry(catalogo_puntos)
names(geom_origen) <- catalogo_puntos$id

# Agregar geometrías a matriz de origen - destino 
giras_od$geometry_origen <- geom_origen[giras_od$id_origen]


giras_od %>%
  select(
    id,
    id_origen,
    nombre_origen,
    geometry_origen,
    geometry
  )

st_distance(giras_od$geometry_origen, st_geometry(giras_od), by_element = TRUE)


clues_imb_no_visit <- cat_clues %>% 
  select(clues_imb, nombre_de_la_unidad, categoria_gerencial, categoria_gerencial_ampliada, latitud, longitud) %>% 
  rename(clues = clues_imb)

clues_imb_no_visit <- st_as_sf(clues_imb_no_visit, coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)

clues_imb_no_visit <- clues_imb_no_visit %>% 
  filter(!clues %in% giras_clues$clues)

clues_imb_no_visit <- clues_imb_no_visit %>% 
  filter(categoria_gerencial %in% c("Generales", "Basico comunitario"))

# Cambiar a minuscula el nombre
clues_imb_no_visit <- clues_imb_no_visit %>% 
  mutate(
    nombre_de_la_unidad = str_to_title(nombre_de_la_unidad)
  )



# Calculo de las distancias  ----------------------------------------------
# Cambiar crs 
giras_od_m <- st_transform(giras_od, 8858)
giras_od_m$geometry_origen <-
  st_transform(giras_od$geometry_origen, 8858)

giras_od_m$dist_km <- as.numeric(
  st_distance(
    giras_od_m$geometry_origen,
    st_geometry(giras_od_m),
    by_element = TRUE
  )
) / 1000

giras_od_m <- giras_od_m %>%
  mutate(dist_km = round(dist_km, 0))

# Regresar a la proyección original
giras_od_m <- st_transform(giras_od, 4326)
giras_od_m$geometry_origen <-
  st_transform(giras_od$geometry_origen, 4326)





# Crear una copia en metros
giras_od_m <- st_transform(giras_od, 8858)

giras_od_m$geometry_origen <- st_transform(
  giras_od$geometry_origen,
  8858
)

# Calcular distancia
giras_od_m$dist_km <- as.numeric(
  st_distance(
    giras_od_m$geometry_origen,
    st_geometry(giras_od_m),
    by_element = TRUE
  )
) / 1000

giras_od_m$dist_km <- round(giras_od_m$dist_km, 0)

# Pasar la distancia al objeto original
giras_od$dist_km <- giras_od_m$dist_km






# Líneas ------------------------------------------------------------------
library(sf)

bezier_line <- function(origen, destino, curvature = 0.25, n = 100){
  
  x1 <- origen[1]
  y1 <- origen[2]
  
  x2 <- destino[1]
  y2 <- destino[2]
  
  mx <- (x1 + x2) / 2
  my <- (y1 + y2) / 2
  
  dx <- x2 - x1
  dy <- y2 - y1
  
  d <- sqrt(dx^2 + dy^2)
  
  cx <- mx - dy / d * curvature * d
  cy <- my + dx / d * curvature * d
  
  t <- seq(0, 1, length.out = n)
  
  coords <- cbind(
    (1 - t)^2 * x1 + 2 * (1 - t) * t * cx + t^2 * x2,
    (1 - t)^2 * y1 + 2 * (1 - t) * t * cy + t^2 * y2
  )
  
  st_linestring(coords)
}

curvas <- lapply(seq_len(nrow(giras_od)), function(i){
  
  origen <- st_coordinates(giras_od$geometry_origen[i])[,1:2]
  
  destino <- st_coordinates(giras_od[i, ])[,1:2]
  
  bezier_line(
    origen = origen,
    destino = destino,
    curvature = 0.20,
    n = 100
  )
  
})

curvas_sf <- st_sf(
  giras_od %>% st_drop_geometry(),
  geometry = st_sfc(curvas, crs = 4326)
)

giras_od <- giras_od %>%
  select(-geometry_origen)

curvas_sf <- curvas_sf %>%
  select(-geometry_origen)

# mapview(curvas_sf)

curvas_df <- st_drop_geometry(curvas_sf)

giras_clues <- left_join(giras_clues, curvas_df %>% 
                           select(id, dist_km), 
                         by = "id")
  




# Leafleft ----------------------------------------------------------------
# Iconos
icons_giras <- iconList(
  "Hospital" = makeIcon("Iconos/cruz_verde.svg", iconWidth = 12, iconHeight = 12),
  "Gobierno" = makeIcon("Iconos/gobierno_verde.svg", iconWidth = 20, iconHeight = 20),
  "Obra" = makeIcon("Iconos/construction_verde.svg", iconWidth = 20, iconHeight = 20)
)

icon_campa <- makeIcon(
  iconUrl = "Iconos/cruz_roja.svg", iconWidth = 13, iconHeight = 13)

icon_no_visitas <- makeIcon(
  iconUrl = "Iconos/cruz_gris_claro.svg", iconWidth = 10, iconHeight = 10)

# Simbología del mapa
legend_html <- paste0(
  "<div style='background:white; padding:10px 12px; border-radius:6px; ",
  "box-shadow:0 0 6px rgba(0,0,0,0.3); font-size:13px; line-height:22px;'>",
  "<b>Simbología</b><br>",
  "<img src='", base64enc::dataURI(file = "Iconos/cruz_roja.svg", mime = "image/svg+xml"),
  "' width='13' height='13'> Dirección General<br>",
  "<img src='", base64enc::dataURI(file = "Iconos/cruz_verde.svg", mime = "image/svg+xml"),
  "' width='12' height='12'> Hospital<br>",
  "<img src='", base64enc::dataURI(file = "Iconos/gobierno_verde.svg", mime = "image/svg+xml"),
  "' width='16' height='16'> Edificio de Gobierno<br>",
  "<img src='", base64enc::dataURI(file = "Iconos/construction_verde.svg", mime = "image/svg+xml"),
  "' width='16' height='16'> Obra<br>",
  "<hr style='margin:6px 0;'>",
  "<span style='display:inline-block; width:16px; height:3px; background:#A57F2C; margin-right:6px;'></span> Ruta de visita<br>",
  "<span style='display:inline-block; width:16px; height:2px; background:#3B3A3A; margin-right:6px;'></span> Límite de entidad",
  "</div>"
)

# Left left  --------------------------------------------------------------

# leaflet(giras_clues) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   
#   # Unidades y hospitales no visitados
#   addMarkers(
#     data = clues_imb_no_visit,
#     icon = icon_no_visitas,
#     popup =  ~paste0(
#       "<b>", nombre_de_la_unidad, "</b>"
#     )
#   ) %>% 
# 
#   # Giras_clues
#   addMarkers(
#     icon = ~icons_giras[tipo_unidad],
#     popup = ~paste0(
#       "<b>", nombre_de_la_unidad, "</b><br>",
#       entidad, "<br>",
#       "<b>Fecha:</b> ", format(fecha, "%d/%m/%Y"), "<br>",
#       "<b>Objetivo:</b> ", objetivo
#     )
#   ) %>%
# 
#   # Campa
#   addMarkers(
#     data = campa,
#     icon = icon_campa,
#     popup = ~paste0(
#       "<b>", nombre_de_la_unidad, "</b>"
#     )
#   ) %>%
# 
#   # Curvas
#   addPolylines(
#     data = curvas_sf,
#     color = "#A57F2C",   # Color
#     weight = 1,          # Grosor
#     opacity = 0.7,       # Transparencia
#     smoothFactor = 1
#   ) %>%
# 
#   # Entidad no concurrentes
#   addPolygons(
#     data = entidad_no_imssb,
#     color = "#666666",
#     weight = 0.4,
#     opacity = 0.8,
#     smoothFactor = 1
#   ) %>%
# 
#   # Entidades concurrentes
#   addPolygons(
#     data = entidad_imssb,
#     fillColor = "transparent",
#     fillOpacity = 0,
#     color = "#3B3A3A",
#     weight = 0.5,
#     opacity = 0.8
#   )






# Comenzar con Shiny para mapa --------------------------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      #visitas table.dataTable {
        font-size: 12px;
      }
      #visitas table.dataTable th,
      #visitas table.dataTable td {
        padding: 4px 6px !important;
      }
    "))
  ),
  
  # Encabezado
  div(
    style = "
      background-color:#691C32;
      color:white;
      padding:10px;
      border-radius:6px;
      margin-bottom:10px;
      text-align:center;
    ",

    h2("Giras de Trabajo IMSS-BIENESTAR 2026", style = "margin:0; font-size:24px;"),

    p(
      "Seguimiento de visitas, recorridos y actividades de la Dirección General",
      style = "font-size:14px; margin-bottom:0;"
    )

  ),

  # Panel + mapa
  fluidRow(

    column(
      width = 3,
      
      selectInput(
        "entidad",
        "Seleccione una entidad:",
        choices = c(
          "Todas las entidades",
          sort(unique(entidad_imssb$entidad))
        ),
        selected = "Todas las entidades"
      ),
      
      selectizeInput(
        "fecha",
        "Seleccione una fecha:",
        choices = c(
          "Todas",
          format(sort(unique(giras_clues$fecha)), "%d/%m/%Y")
        ),
        selected = "Todas",
        options = list(
          maxOptions = 1000
        )
      ),
      
      uiOutput("distancia_total"),
      
      DT::dataTableOutput("visitas"),
      
      tags$br(), ### A partir de aquí comienza el logo

      div(
        style = "text-align:center;",
        tags$img(
          src = "logo_imss_bienestar.png",
          style = "
      width:100%;
      max-width:320px;
      height:auto;
    "
        )
      ) ## Aquí termina el logo
    ),

    column(
      width = 9,

      leafletOutput("mapa", height = "calc(100vh - 130px)")

    )

  )

)


  
  
server <- function(input, output, session) {
  
  giras_filtradas <- reactive({
    
    datos <- giras_clues
    
    # Filtrar por fecha
    if (input$fecha != "Todas") {
      fecha_sel <- as.Date(
        input$fecha,
        format = "%d/%m/%Y"
      )
      
      datos <- datos %>%
        filter(fecha == fecha_sel)
    }
    
    # Filtrar por entidad
    if (input$entidad != "Todas las entidades") {
      datos <- datos %>%
        filter(entidad == input$entidad)
    }

    datos
    
  })
  
  curvas_filtradas <- reactive({
    
    datos <- curvas_sf
    
    # Filtrar por fecha
    if (input$fecha != "Todas") {
      
      fecha_sel <- as.Date(
        input$fecha,
        format = "%d/%m/%Y"
      )
      
      datos <- datos %>%
        filter(fecha == fecha_sel)
    }
    
    # Filtrar por entidad
    if (input$entidad != "Todas las entidades") {
      
      datos <- datos %>%
        filter(entidad == input$entidad)
      
    }
    
    datos
    
  })
  
  entidades_filtradas <- reactive({
    
    if (input$entidad == "Todas las entidades") {
      
      entidad_imssb
      
    } else {
      
      entidad_imssb %>%
        filter(entidad == input$entidad)
      
    }
    
  })
  
  observe({
    
    datos <- giras_clues
    
    if (input$entidad != "Todas las entidades") {
      
      datos <- datos %>%
        filter(entidad == input$entidad)
      
    }
    
    fechas <- c(
      "Todas",
      format(sort(unique(datos$fecha)), "%d/%m/%Y")
    )
    
    updateSelectizeInput(
      session,
      "fecha",
      choices = fechas,
      selected = if (input$fecha %in% fechas)
        input$fecha
      else
        "Todas"
    )
    
  })

  output$mapa <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Entidades no concurrentes
      addPolygons(
        data = entidad_no_imssb,
        color = "#666666",
        weight = 0.4,
        opacity = 0.8,
        smoothFactor = 1
      ) %>% 
      
      # Campa
      addMarkers(
        data = campa,
        icon = icon_campa,
        popup = ~nombre_de_la_unidad
      ) %>% addControl(
        html = legend_html,
        position = "bottomleft"
      )
  })
  
  output$visitas <- DT::renderDataTable({
    
    datos <- giras_filtradas() %>%
      arrange(fecha, orden_visita) %>%
      st_drop_geometry() %>%
      mutate(Fecha = format(fecha, "%d/%m/%Y")) %>%
      select(
        Fecha,
        "Orden de visita" = orden_visita,
        Nombre = nombre_de_la_unidad,
        "Objetivo de la visita" = objetivo
      )
    
    DT::datatable(
      datos,
      rownames = FALSE,
      class = "compact stripe hover",
      options = list(
        dom = "t",
        paging = FALSE,
        scrollY = "35vh",
        scrollX = TRUE,
        columnDefs = list(
          list(width = "70px", targets = 0),
          list(width = "50px", targets = 1),
          list(width = "160px", targets = 2),
          list(width = "220px", targets = 3)
        )
      )
    )
  })
  
  output$distancia_total <- renderUI({
    
    total_km <- giras_filtradas() %>%
      summarise(total = sum(dist_km, na.rm = TRUE)) %>%
      pull(total)
    
    tags$div(
      style = "padding:10px; background:#f5f5f5; border-radius:5px; margin-bottom:10px;",
      tags$b("Distancia total recorrida"),
      tags$br(),
      paste0(round(total_km, 0), " km")
    )
    
  })

  observe({

    giras_sel <- giras_filtradas()

    curvas_sel <- curvas_filtradas()
    
    entidades_sel <- entidades_filtradas()

    leafletProxy("mapa") %>%
      
      clearGroup("entidades") %>%
      clearGroup("giras") %>%
      clearGroup("curvas") %>%
      
      # Entidades
      addPolygons(
        data = entidades_sel,
        fillColor = "transparent",
        fillOpacity = 0,
        color = "#3B3A3A",
        weight = 1.5,
        opacity = 0.8,
        group = "entidades"
      ) %>%

      # Giras
      addMarkers(
        data = giras_sel,
        icon = ~icons_giras[tipo_unidad],
        popup = ~paste0(
          "<b>", nombre_de_la_unidad, "</b><br>",
          entidad, "<br>",
          "<b>Fecha:</b> ", format(fecha, "%d/%m/%Y"), "<br>",
          "<b>Objetivo:</b> ", objetivo
        ),
        group = "giras"
      ) %>%

      # Curvas
      addPolylines(
        data = curvas_sel,
        color = "#A57F2C",
        weight = 2.5,
        group = "curvas"
      )

  })

}

shinyApp(ui, server)

