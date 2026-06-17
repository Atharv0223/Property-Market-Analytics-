# ==============================================================
# app_final_ALL.R
# When Affordability Breaks (VIC, 2000–2024)
# --------------------------------------------------------------
# STORY • EXPLORE (Time-series, Correlation, Geo Animation)
# • ABOUT & DATA
# ==============================================================


suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(plotly)
  library(leaflet)
  library(sf)
  library(glue)
  library(scales)
  library(shinyWidgets)
  library(shinyjs)
  library(lubridate)
})

# ==============================================================
# DATA PREP
# ==============================================================

YEARS_WANTED <- 2000:2024

rates <- readr::read_csv("data/CleanHouseLoanInterest.csv", show_col_types = FALSE) |>
  mutate(year = as.integer(lubridate::year(Date))) |>
  group_by(year) |>
  summarise(cash_rate = mean(FLRHOOTL, na.rm = TRUE), .groups = "drop")

rai_raw <- readr::read_csv("data/rent_affordability.csv", show_col_types = FALSE) |>
  mutate(year = as.integer(lubridate::year(Period))) |>
  group_by(year, LGA) |>
  summarise(rai = mean(Percent, na.rm = TRUE), .groups = "drop") |>
  rename(lga_name = LGA) |>
  mutate(lga_name = stringr::str_squish(lga_name))

dv_raw <- readr::read_csv("data/domestic_violence.csv", show_col_types = FALSE) |>
  mutate(year = as.integer(lubridate::year(Period))) |>
  rename(lga_name = `Local government area`, rate_per_100k = Rate_per_100k) |>
  mutate(lga_name = stringr::str_squish(lga_name)) |>
  select(lga_name, year, rate_per_100k)

YEARS_ALL <- sort(intersect(YEARS_WANTED, Reduce(intersect, list(rates$year, rai_raw$year, dv_raw$year))))
rates   <- rates  |> filter(year %in% YEARS_ALL)
rai_raw <- rai_raw |> filter(year %in% YEARS_ALL)
dv_raw  <- dv_raw  |> filter(year %in% YEARS_ALL)

# ==============================================================
# GEOSPATIAL
# ==============================================================

vic_lga <- sf::read_sf("data/vic_lga.geojson") |>
  st_transform(4326) |>
  mutate(LGA_NAME21 = stringr::str_squish(LGA_NAME21))

melb_pt <- st_sfc(st_point(c(144.9631, -37.8136)), crs = 4326)
dist_km <- as.numeric(st_distance(st_centroid(vic_lga), melb_pt)) / 1000
vic_lga$region_cat <- case_when(
  dist_km <= 60  ~ "Metro",
  dist_km <= 200 ~ "Regional",
  TRUE           ~ "Rural"
)

# ==============================================================
# GLOBALS
# ==============================================================

bubble_cols   <- c(Metro = "#3B82F6", Regional = "#22C55E", Rural = "#8B5CF6")
colour_afford <- "#56B4E9"  # blue
colour_viol   <- "#E69F00"  # orange

panel_long <- rai_raw |>
  full_join(dv_raw, by = c("lga_name","year")) |>
  mutate(tmp_join_key = year) |>
  left_join(rates |> transmute(year, cash_rate), by = c("tmp_join_key" = "year")) |>
  select(-tmp_join_key)

# ==============================================================
# THEME
# ==============================================================

app_theme <- bs_theme(
  version = 5, bootswatch = "flatly",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  primary = "#2c3e50", secondary = "#ecf0f1"
)

# ==============================================================
# UI
# ==============================================================

ui <- navbarPage(
  title = "When Affordability Breaks (VIC, 2000–2024)",
  theme = app_theme, id = "main_nav", collapsible = TRUE,
  header = tags$head(tags$style(HTML("
    .floating-panel {position:absolute; right:18px; top:90px; z-index:9999;
                     background:rgba(255,255,255,0.98); border-radius:12px;
                     box-shadow:0 10px 24px rgba(0,0,0,0.18); padding:12px 14px; width:340px;}
    .floating-title {font-weight:700; margin-bottom:6px;}
    .leaflet-control-container .leaflet-bottom.leaflet-right { margin-right:360px; }
    .well {
    background-color: #f9f9f9;
    border-left: 4px solid #3B82F6;
    padding: 10px 14px;
    margin-top: 6px;
    font-size: 13.5px;
  }
  .btn-link {
    color: #0078D7;
    font-weight: 500;
  }
  .btn-link:hover {
    text-decoration: underline;
  }
  "))),
  
  # ---------------- STORY ----------------
  tabPanel(
    "Story",
    fluidPage(
      useShinyjs(),
      h2("When Affordability Breaks"),
      h4("How Rising Rates Ripple into Domestic Violence (VIC, 2000–2024)"),
      p("A narrative link between interest rates, rental affordability (RAI), and domestic-violence incidents across Victorian LGAs."),
      
      # --- Trend Plot ---
      sliderInput("story_year", "Highlight Year",
                  min=min(YEARS_ALL), max=max(YEARS_ALL),
                  value=max(YEARS_ALL), step=1,
                  animate=animationOptions(interval=900, loop=TRUE)),
      
      plotlyOutput("story_trend", height="360px"),
      
      # Info button + box
      div(
        style = "margin-top: -5px;",
        actionLink("info_trend_btn", "ℹ️ More information", class = "btn btn-link"),
        hidden(div(id = "info_trend_box",
                   wellPanel(
                     p("This plot shows how housing loan interest rates (orange line) and the Rental Affordability Index (blue line) changed over time from 2000–2024."),
                     p("Both series are normalised to a base year of 100, allowing you to compare percentage shifts rather than raw values. 
             The dashed line represents the year selected in the 'Highlight Year' slider.")
                   )
        ))
      ),
      
      br(), br(),
      
      # --- Scatter Plot ---
      h3("The Pressure Points"),
      p("Bubbles show affordability vs domestic-violence rates; colour = Metro / Regional / Rural."),
      sliderInput("story_scatter_year", "Scatter Year",
                  min=min(YEARS_ALL), max=max(YEARS_ALL),
                  value=max(YEARS_ALL), step=1,
                  animate=animationOptions(interval=900, loop=TRUE)),
      
      plotlyOutput("story_scatter", height="380px"),
      
      div(
        style = "margin-top: -5px;",
        actionLink("info_scatter_btn", "ℹ️ More information", class = "btn btn-link"),
        hidden(div(id = "info_scatter_box",
                   wellPanel(
                     p("This scatter plot shows the relationship between Rental Affordability Index (RAI) and domestic-violence incident rates across Victorian LGAs for the selected year."),
                     p("Each dot represents one LGA. The bubble colour indicates whether it is Metro (blue), Regional (green), or Rural (purple).")
                   )
        ))
      ),
      
      br(), br(),
      
      # --- Map Plot ---
      h3("Where the Burden Falls"),
      p("A severity index combines falling affordability (ΔRAI < 0) and rising violence (ΔDV > 0). Darker regions face multiple pressures."),
      
      div(style="position:relative;",
          leafletOutput("story_map", height="500px"),
          div(class="floating-panel",
              div(class="floating-title", "Compare (hover) — Story Map"),
              uiOutput("story_hover_title"),
              plotlyOutput("story_hover_bar", height="170px"),
              uiOutput("story_hover_pct")
          )
      ),
      
      div(
        style = "margin-top: -5px;",
        actionLink("info_map_btn", "ℹ️ More information", class = "btn btn-link"),
        hidden(div(id = "info_map_box",
                   wellPanel(
                     p("This choropleth map combines changes in affordability (RAI) and domestic-violence rates to create a Severity Index."),
                     p("Darker regions indicate higher combined stress — where affordability has fallen while violence rates have increased."),
                     p("Hover over a region to compare its latest RAI and DV indicators.")
                   )
        ))
      ),
      
      br()
    )
  ),
  
  
  # ---------------- EXPLORE ----------------
  tabPanel(
    "Explore",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Filters & Controls"),
          selectizeInput("lga_select", "LGAs (for charts that allow multi-select)",
                         choices=sort(unique(rai_raw$lga_name)), multiple=TRUE,
                         options=list(placeholder="All LGAs")),
          sliderInput("year_range", "Year Range (applies everywhere)",
                      min=min(YEARS_ALL), max=max(YEARS_ALL),
                      value=c(min(YEARS_ALL), max(YEARS_ALL)), step=1),
          hr(),
          radioGroupButtons(
            inputId="explore_var", label="Variable for Time-series Explorer",
            choices=c("Affordability (RAI)"="rai",
                      "DV incidents /100k"="rate_per_100k",
                      "Interest rate"="cash_rate"),
            selected="rate_per_100k", justified=TRUE, size="sm"
          ),
          helpText("Tip: The map animation uses the controls in its own tab.")
        ),
        mainPanel(
          tabsetPanel(
            type="pills",
            
            tabPanel("Time-series",
                     div(style="display:flex; gap:20px; align-items:center; margin-bottom:8px;",
                         tags$div(tags$b("Common X-axis (2000–2024):"),
                                  tags$span(" all variables share the same year scale."))),
                     plotlyOutput("ts_explorer", height="420px"),
                     
                     # --- Info button + box
                     div(style="margin-top:-5px;",
                         actionLink("info_ts_btn", "ℹ️ More information", class="btn btn-link"),
                         hidden(div(id="info_ts_box",
                                    wellPanel(
                                      p("This plot shows yearly trends for the selected variable (Rental Affordability Index, Domestic-Violence rate, or Interest rate) across chosen LGAs."),
                                      p("Use the LGA selector on the left to focus on particular areas or leave empty to show the top 8 by average value.")
                                    )
                         ))
                     )
            ),
            
            
            tabPanel("Correlation Matrix",
                     p("Pearson correlations computed on the merged (LGA, year) panel in the selected year range."),
                     plotlyOutput("corr_heatmap", height="420px"),
                     
                     div(style="margin-top:-5px;",
                         actionLink("info_corr_btn", "ℹ️ More information", class="btn btn-link"),
                         hidden(div(id="info_corr_box",
                                    wellPanel(
                                      p("This heatmap shows the pairwise Pearson correlation between interest rates, affordability (RAI), and domestic-violence rates."),
                                      p("Red squares indicate positive correlation; blue squares indicate inverse relationships. The numeric labels display the exact correlation coefficients.")
                                    )
                         ))
                     )
            ),
            
            
            tabPanel("Geo Animation",
                     fluidRow(
                       column(6,
                              sliderInput("map_year", "Map Year",
                                          min=min(YEARS_ALL), max=max(YEARS_ALL),
                                          value=min(YEARS_ALL), step=1,
                                          animate=animationOptions(interval=900, loop=TRUE))
                       ),
                       column(6,
                              selectInput("map_var", "Map Variable",
                                          choices=c("DV incidents /100k"="rate_per_100k",
                                                    "Affordability (RAI)"="rai",
                                                    "Interest Rate"="cash_rate"),
                                          selected="rate_per_100k"))
                     ),
                     div(style="position:relative;",
                         leafletOutput("explore_map", height="560px"),
                         div(class="floating-panel",
                             div(class="floating-title", "Compare (hover) — Explore Map"),
                             uiOutput("hover_title"),
                             plotlyOutput("hover_sparkline", height="110px"),
                             plotlyOutput("hover_distribution", height="110px"),
                             uiOutput("hover_pct")
                         )
                     ),
                     
                     div(style="margin-top:-5px;",
                         actionLink("info_mapx_btn", "ℹ️ More information", class="btn btn-link"),
                         hidden(div(id="info_mapx_box",
                                    wellPanel(
                                      p("This animated map visualises how each LGA’s selected variable changes over time."),
                                      p("Use the year slider or animation controls to see trends evolve spatially. Hover on any region to view its local time-series and state-wide distribution.")
                                    )
                         ))
                     )
            ),
            
            
            
          )
        )
      )
    )
  ),
  
  # ---------------- ABOUT & DATA TAB ----------------
  tabPanel(
    "About & Data",
    fluidPage(
      h2("Project Information"),
      p("This dashboard explores how rising housing loan interest rates affect rental affordability 
       and correlate with domestic-violence incidents across Victoria’s Local Government Areas (LGAs). 
       It integrates financial, social, and spatial datasets to reveal where affordability pressures 
       and social harms converge between 2000 and 2024."),
      
      h3("Data Sources"),
      tags$ul(
        tags$li(
          HTML("<b>Reserve Bank of Australia (RBA)</b> – 
              <i>Housing Loan Interest Rates (CleanHouseLoanInterest.csv)</i>. 
              Monthly variable home loan rates, aggregated to yearly averages. 
              RBA Statistical Tables</a>.")
        ),
        tags$li(
          HTML("<b>SGS Economics & Planning / AIHW</b> – 
              <i>Rental Affordability Index (rent_affordability.csv)</i>. 
              Quarterly index data aggregated to annual means. Victorian RAI values were 
              mapped to LGAs based on metropolitan/regional correspondence. ")
        ),
        tags$li(
          HTML("<b>Crime Statistics Agency Victoria</b> – 
              <i>Family Violence Incidents by LGA (domestic_violence.csv)</i>. 
              Annual counts and rates per 100,000 population (2010–2024). 
              CSA Victoria</a>.")
        ),
        tags$li(
          HTML("<b>Australian Bureau of Statistics (ABS)</b> – 
              <i>Local Government Area Boundaries (vic_lga.geojson; ASGS 2021)</i>. 
              Used for spatial mapping of indicators. 
              ABS Geography Portal</a>.")
        )
      ),
      
      h3("Data Preparation Notes"),
      tags$ul(
        tags$li("All data converted to a common yearly timeline (2000–2024) based on intersection of available years."),
        tags$li("Interest rate series averaged per year to align with annual RAI and DV data."),
        tags$li("Domestic-violence counts normalised to rates per 100,000 using ABS population estimates."),
        tags$li("RAI region-level values mapped to LGAs using a metro–regional lookup table."),
        tags$li("Composite metrics (e.g., 'Stress Index') derived by ranking LGAs on both RAI decline and DV rate increase."),
        tags$li("Correlation matrix computed using all LGA–year combinations within the selected range (pairwise NA handling).")
      ),
      
      h3("Limitations"),
      tags$ul(
        tags$li("RAI is reported at regional level; mapping to LGA introduces some approximation."),
        tags$li("Family violence incidents may reflect reporting rates as well as true incidence."),
        tags$li("Correlation does not imply causation — trends suggest association, not direct effect."),
        tags$li("Boundary updates between ASGS versions may cause small spatial mismatches.")
      ),
      
      br(),
      p(em("Prepared by Atharv Sarathe for FIT5147 Data Visualisation Project – Monash University (2025)."))
    )
  )
  
)

# ==============================================================
# SERVER
# ==============================================================

server <- function(input, output, session) {
  
  yrs_in_range <- reactive(seq(input$year_range[1], input$year_range[2], by=1))
  
  panel_filtered <- reactive({
    df <- panel_long |> filter(year %in% yrs_in_range())
    if (length(input$lga_select)) df <- df |> filter(lga_name %in% input$lga_select)
    df
  })
  
  observe({
    message("RAI range: ", paste0(range(rai_raw$rai, na.rm = TRUE), collapse = " - "))
    message("Cash rate range: ", paste0(range(rates$cash_rate, na.rm = TRUE), collapse = " - "))
  })
  
  
  
  # ---------- STORY: Trend ----------
  output$story_trend <- renderPlotly({
    avg_rai <- rai_raw |> 
      group_by(year) |> 
      summarise(rai = mean(rai, na.rm = TRUE), .groups = "drop")
    
    df <- full_join(rates, avg_rai, by = "year") |> 
      filter(year %in% YEARS_ALL) |> 
      arrange(year)
    
    # normalize both to base year = 100
    base_year <- min(df$year)
    df <- df |> 
      mutate(
        rai_idx = 100 * rai / first(rai),
        rate_idx = 100 * cash_rate / first(cash_rate)
      )
    
    plot_ly(df, x = ~year) |>
      add_lines(
        y = ~rate_idx, name = "Cash Rate (Index)",
        line = list(color = colour_viol, width = 3)
      ) |>
      add_lines(
        y = ~rai_idx, name = "RAI (Index)",
        line = list(color = colour_afford, width = 3)
      ) |>
      add_segments(
        x = input$story_year, xend = input$story_year,
        y = min(c(df$rai_idx, df$rate_idx), na.rm = TRUE),
        yend = max(c(df$rai_idx, df$rate_idx), na.rm = TRUE),
        line = list(color = "#999", dash = "dash"), showlegend = FALSE
      ) |>
      layout(
        xaxis = list(dtick = 1, title = "Year"),
        yaxis = list(title = "Index (Base = 100)"),
        legend = list(orientation = "h", x = 0.3),
        margin = list(b = 60)
      ) |>
      config(displayModeBar = FALSE)
  })
  
  
  # ---------- STORY: Scatter ----------
  output$story_scatter <- renderPlotly({
    yr <- input$story_scatter_year
    latest <- inner_join(
      rai_raw %>% filter(year==yr) %>% select(lga_name, rai),
      dv_raw  %>% filter(year==yr) %>% select(lga_name, rate_per_100k),
      by="lga_name"
    ) |>
      left_join(vic_lga %>% st_drop_geometry() %>% select(lga_name=LGA_NAME21, region_cat),
                by="lga_name")
    
    plot_ly(latest, x=~rai, y=~rate_per_100k,
            color=~region_cat, colors=bubble_cols,
            type="scatter", mode="markers",
            marker=list(size=9, line=list(color="white", width=1)),
            text=~lga_name,
            hovertemplate="<b>%{text}</b><br>RAI: %{x:.2f}<br>DV/100k: %{y:.0f}<extra></extra>") |>
      layout(xaxis=list(title=paste0("RAI (", yr, ")")),
             yaxis=list(title=paste0("DV incidents per 100k (", yr, ")")),
             legend=list(orientation="h", x=0.35, y=-0.25),
             margin=list(b=80)) |>
      config(displayModeBar=FALSE)
  })
  
  # ---------- STORY: Map + Hover ----------
  output$story_map <- renderLeaflet({
    delta_rai <- rai_raw |> arrange(year) |> group_by(lga_name) |>
      summarise(delta_rai = last(rai) - first(rai), .groups="drop")
    delta_dv  <- dv_raw  |> arrange(year) |> group_by(lga_name) |>
      summarise(delta_dv  = last(rate_per_100k) - first(rate_per_100k), .groups="drop")
    sev_tbl <- full_join(delta_rai, delta_dv, by="lga_name") |>
      mutate(severity = rescale(-delta_rai) + rescale(delta_dv))
    
    joined <- vic_lga |> left_join(sev_tbl, by=c("LGA_NAME21"="lga_name"))
    pal <- colorNumeric("YlOrRd", domain=joined$severity, na.color="#f5f5f5")
    
    leaflet(joined) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(fillColor=~pal(severity), color="#666", weight=0.6, fillOpacity=0.86,
                  layerId=~LGA_NAME21,
                  label=~glue("{LGA_NAME21}\nSeverity: {round(severity,2)}"),
                  highlightOptions=highlightOptions(weight=2, color="#222", fillOpacity=0.92, bringToFront=TRUE)) |>
      addLegend("bottomright", pal=pal, values=~severity, title="Severity Index", opacity=0.86)
  })
  
  hovered_story_lga <- reactiveVal(NULL)
  observeEvent(input$story_map_shape_mouseover, { hovered_story_lga(input$story_map_shape_mouseover$id) })
  
  output$story_hover_title <- renderUI({
    lga <- hovered_story_lga()
    if (is.null(lga)) return(tags$span("Hover a region…"))
    tags$div(tags$b(lga))
  })
  # ---------- STORY: Info button toggle logic ----------
  observeEvent(input$info_trend_btn, {
    toggle("info_trend_box", anim = TRUE, time = 0.3)
  })
  
  observeEvent(input$info_scatter_btn, {
    toggle("info_scatter_box", anim = TRUE, time = 0.3)
  })
  
  observeEvent(input$info_map_btn, {
    toggle("info_map_box", anim = TRUE, time = 0.3)
  })
  
  # ---------- EXPLORE: Info button toggle logic ----------
  observeEvent(input$info_ts_btn, {
    toggle("info_ts_box", anim = TRUE, time = 0.3)
  })
  
  observeEvent(input$info_corr_btn, {
    toggle("info_corr_box", anim = TRUE, time = 0.3)
  })
  
  observeEvent(input$info_mapx_btn, {
    toggle("info_mapx_box", anim = TRUE, time = 0.3)
  })
  
  output$story_hover_bar <- renderPlotly({
    lga <- hovered_story_lga(); if (is.null(lga)) return(plotly_empty())
    yr <- max(YEARS_ALL)
    rate_y <- rates   |> filter(year==yr) |> pull(cash_rate)
    rai_y  <- rai_raw |> filter(lga_name==lga, year==yr) |> pull(rai)
    all_rai<- rai_raw |> filter(year==yr) |> pull(rai)
    if (!length(rate_y) || !length(rai_y) || !length(all_rai)) return(plotly_empty())
    
    df <- tibble(
      side=c("Affordability (RAI)","Interest Rate"),
      val = c(-rescale(rai_y, to=c(0,1), from=range(all_rai, na.rm=TRUE)),
              rescale(rate_y,to=c(0,1), from=range(rates$cash_rate, na.rm=TRUE))),
      clr=c(colour_afford, colour_viol)
    )
    
    plot_ly(df, x=~val, y=~side, type="bar", orientation="h", marker=list(color=~clr)) |>
      layout(barmode="relative",
             xaxis=list(title=NULL, zeroline=TRUE,
                        tickvals=c(-1,-0.5,0,0.5,1), ticktext=c("Low","","0","","High")),
             yaxis=list(title=NULL),
             showlegend=FALSE, margin=list(l=5,r=5,t=0,b=5)) |>
      config(displayModeBar=FALSE)
  })
  
  output$story_hover_pct <- renderUI({
    lga <- hovered_story_lga(); if (is.null(lga)) return(NULL)
    base <- min(YEARS_ALL); last <- max(YEARS_ALL)
    dv_b <- dv_raw  |> filter(lga_name==lga, year==base) |> pull(rate_per_100k)
    dv_e <- dv_raw  |> filter(lga_name==lga, year==last) |> pull(rate_per_100k)
    rai_b<- rai_raw |> filter(lga_name==lga, year==base) |> pull(rai)
    rai_e<- rai_raw |> filter(lga_name==lga, year==last) |> pull(rai)
    r_b  <- rates   |> filter(year==base) |> pull(cash_rate)
    r_e  <- rates   |> filter(year==last) |> pull(cash_rate)
    pct  <- function(a,b) ifelse(length(a)&&length(b)&&!is.na(a)&&a!=0, round(100*(b-a)/a,1), NA)
    tags$small(HTML(glue("<b>Δ%</b> {base}→{last} | DV: <b>{pct(dv_b,dv_e)}%</b> | RAI: <b>{pct(rai_b,rai_e)}%</b> | Rate: <b>{pct(r_b,r_e)}%</b>")))
  })
  
  # ---------------- EXPLORE: Time-series ----------------
  output$ts_explorer <- renderPlotly({
    df <- panel_filtered()
    vlab <- switch(input$explore_var,
                   "rai"="RAI",
                   "rate_per_100k"="DV incidents /100k",
                   "cash_rate"="Interest rate")
    
    plot_df <- df |>
      transmute(year, lga_name,
                value = case_when(
                  input$explore_var == "rai" ~ rai,
                  input$explore_var == "cash_rate" ~ cash_rate,
                  TRUE ~ rate_per_100k
                )) |>
      drop_na(value)
    
    if (!length(input$lga_select)) {
      keep <- plot_df |>
        group_by(lga_name) |>
        summarise(m=mean(value, na.rm=TRUE), .groups="drop") |>
        arrange(desc(m)) |> slice_head(n=8) |> pull(lga_name)
      plot_df <- plot_df |> filter(lga_name %in% keep)
    }
    
    plot_ly(plot_df, x=~year, y=~value, color=~lga_name, type="scatter", mode="lines+markers") |>
      layout(xaxis=list(title="Year", dtick=1),
             yaxis=list(title=vlab),
             legend=list(orientation="h", x=0.1, y=-0.25),
             margin=list(b=80)) |>
      config(displayModeBar=FALSE)
  })
  
  # ---------------- EXPLORE: Correlation Matrix ----------
  output$corr_heatmap <- renderPlotly({
    df <- panel_filtered() |> select(rai, rate_per_100k, cash_rate) |> drop_na()
    validate(need(nrow(df) > 4, "Not enough data for correlation"))
    cmat <- cor(df, use="pairwise.complete.obs", method="pearson")
    
    hdf <- tibble::as_tibble(expand.grid(
      x = colnames(cmat), y = colnames(cmat)
    )) |>
      mutate(r = as.numeric(cmat[cbind(match(x, colnames(cmat)), match(y, colnames(cmat)))]))
    
    plot_ly(hdf, x=~x, y=~y, z=~r, type="heatmap",
            colorscale="RdBu", reversescale=TRUE, zmin=-1, zmax=1) |>
      add_annotations(x=hdf$x, y=hdf$y, text=sprintf("%.2f", hdf$r),
                      showarrow=FALSE, font=list(color="black", size=12)) |>
      layout(xaxis=list(title=""), yaxis=list(title=""),
             margin=list(l=60, r=20, t=20, b=60)) |>
      config(displayModeBar=FALSE)
  })
  
  # ==========================================================
  # EXPLORE: GEO ANIMATION (smooth + stable like STORY)
  # ==========================================================
  
  # ---- Base map (render once) ----
  output$explore_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(144.9, -37.5, 7)
  })
  
  
  hovered_lga <- reactiveVal(NULL)
  # ---- Hover logic for Explore Map ----
  observeEvent(input$explore_map_shape_mouseover, {
    hovered_lga(input$explore_map_shape_mouseover$id)
  })
  
 # observeEvent(input$explore_map_shape_mouseout, {
  #  hovered_lga(NULL)
  #})
  
  
  
  # ---- Color palette factory ----
  get_palette <- function(var) {
    pal <- switch(
      var,
      "rate_per_100k" = colorNumeric(
        "YlOrRd",
        range(dv_raw$rate_per_100k, na.rm = TRUE),
        na.color = "#f5f5f5"
      ),
      "rai" = colorNumeric(
        "Blues",
        range(rai_raw$rai, na.rm = TRUE),
        na.color = "#f5f5f5"
      ),
      "cash_rate" = colorNumeric(
        "Greens",
        range(rates$cash_rate, na.rm = TRUE),
        na.color = "#f5f5f5",
        reverse = FALSE
      )
    )
    return(pal)
  }
  
  
  
  # ---- Throttle map year ----
  map_year_throttled <- throttle(reactive(input$map_year), 400)
  
  # ---- Reactive join ----
  map_data <- reactive({
    y   <- input$map_year
    var <- input$map_var
    
    base <- switch(
      var,
      "rate_per_100k" = dv_raw |> filter(year == y) |> select(lga_name, value = rate_per_100k),
      "rai"           = rai_raw |> filter(year == y) |> select(lga_name, value = rai),
      "cash_rate"     = {
        val <- rates |> filter(year == y) |> pull(cash_rate)
        tibble(lga_name = unique(vic_lga$LGA_NAME21), value = val)
      }
    )
    
    vic_lga |> 
      left_join(base, by = c("LGA_NAME21" = "lga_name")) |> 
      mutate(year = y, variable = var)
  })
  
  
  
  
  # ---- Update fill colour + legend only ----
  # ---- (A) Initialize shapes and legend when year or var changes ----
  observeEvent(list(input$map_var, input$map_year), {
    df  <- map_data()
    y   <- input$map_year
    var <- input$map_var
    pal <- get_palette(var)
    req(nrow(df) > 0)
    
    df$fill_col <- pal(df$value)
    
    leafletProxy("explore_map", data = df) |>
      clearShapes() |> clearControls() |>
      addPolygons(
        layerId = ~LGA_NAME21,
        fillColor = ~fill_col,
        color = "#666", weight = 0.6, fillOpacity = 0.86,
        label = ~glue("{LGA_NAME21}<br>{switch(var,
           'rate_per_100k'='DV incidents/100k',
           'rai'='Affordability (RAI)',
           'cash_rate'='Interest Rate')} ({y}): {round(value, 2)}"),
        highlightOptions = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.92, bringToFront = TRUE
        )
      ) |>
      addLegend(
        "bottomright", pal = pal, values = df$value,
        title = switch(
          var,
          "rate_per_100k" = glue("DV incidents /100k ({y})"),
          "rai"           = glue("Affordability (RAI) — {y}"),
          "cash_rate"     = glue("Interest Rate — {y}")
        ),
        opacity = 0.86
      )
  })
  
  # ---- (B) Update only fill color dynamically (no legend redraw) ----
  observeEvent(hovered_lga(), {
    # do nothing to legend here, only sparkline/distribution react
  })
  
  
  
  observeEvent(TRUE, {
    req(input$map_var, input$map_year)
    # Trigger the same logic as the main observer once
    y0 <- input$map_year
    var0 <- input$map_var
    
    base0 <- switch(
      var0,
      "rate_per_100k" = dv_raw  |> filter(year == y0) |> select(lga_name, value = rate_per_100k),
      "rai"           = rai_raw |> filter(year == y0) |> select(lga_name, value = rai),
      "cash_rate"     = {
        v <- rates |> filter(year == y0) |> pull(cash_rate)
        tibble(lga_name = unique(vic_lga$LGA_NAME21), value = v)
      }
    )
    
    df0  <- vic_lga |> left_join(base0, by = c("LGA_NAME21" = "lga_name"))
    pal0 <- get_palette(var0)
    
    leafletProxy("explore_map", data = df0) |>
      clearShapes() |> clearControls() |>
      addPolygons(
        layerId   = ~LGA_NAME21,
        fillColor = ~pal0(value),
        color     = "#666", weight = 0.6, fillOpacity = 0.86,
        highlightOptions = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.92, bringToFront = TRUE
        )
      ) |>
      addLegend(
        "bottomright", pal = pal0, values = df0$value,
        title = switch(
          var0,
          "rate_per_100k" = glue("DV incidents /100k ({y0})"),
          "rai"           = glue("Affordability (RAI) — {y0}"),
          "cash_rate"     = glue("Interest Rate — {y0}")
        ),
        opacity = 0.86
      )
  }, once = TRUE)
  
  
  #======================++#============#======================++#============#======================++#============
  # ----- (A) Sparkline: trend over time for hovered LGA -----
  # ----- (A) Sparkline: trend over time for hovered LGA -----
  output$hover_sparkline <- renderPlotly({
    lga <- hovered_lga()
    if (is.null(lga)) return(plotly_empty())
    
    var <- input$map_var
    df_lga <- panel_long %>% filter(lga_name == lga)
    
    ylab <- switch(
      var,
      "rate_per_100k" = "DV incidents /100k",
      "rai"           = "Affordability (RAI)",
      "cash_rate"     = "Interest Rate"
    )
    
    col_var <- dplyr::case_when(
      var == "rate_per_100k" ~ colour_viol,
      var == "rai"           ~ colour_afford,
      var == "cash_rate"     ~ "#009E73"
    )
    
    plot_ly(
      df_lga,
      x = ~year,
      y = as.formula(paste0("~", var)),
      type = "scatter",
      mode = "lines+markers",
      line = list(color = col_var, width = 2),
      marker = list(color = col_var, size = 4)
    ) |>
      layout(
        title = list(text = ylab, x = 0.05, y = 0.9, font = list(size = 10)),
        xaxis = list(title = "Year", showgrid = FALSE, tickfont = list(size = 9)),
        yaxis = list(
          title = list(text = ylab, font = list(size = 9)),  # 👈 smaller title font
          showgrid = FALSE,
          tickfont = list(size = 9)
        ),
        margin = list(l = 25, r = 5, t = 25, b = 25),
        showlegend = FALSE
      ) |>
      config(displayModeBar = FALSE)
  })
  

  
  # ----- (D) Distribution: position of hovered LGA among all LGAs (selected year) -----
  output$hover_distribution <- renderPlotly({
    lga <- hovered_lga()
    if (is.null(lga)) return(plotly_empty())
    
    y   <- input$map_year
    var <- input$map_var
    df_year <- panel_long %>% filter(year == y)
    
    value_lga <- df_year %>%
      filter(lga_name == lga) %>%
      dplyr::pull({{ var }})
    
    if (length(value_lga) == 0 || is.na(value_lga)) return(plotly_empty())
    
    col_var <- dplyr::case_when(
      var == "rate_per_100k" ~ colour_viol,
      var == "rai"           ~ colour_afford,
      var == "cash_rate"     ~ "#009E73"
    )
    
    plot_ly(
      df_year,
      x = ~get(var),
      type = "histogram",
      nbinsx = 25,
      marker = list(color = "rgba(100,100,100,0.3)",
                    line = list(color = "white", width = 0.5))
    ) |>
      layout(
        title = list(text = "State distribution", x = 0.05, y = 0.9, font = list(size = 10)),
        xaxis = list(
          title = "Value",          # 👈 add label
          showticklabels = TRUE,
          tickfont = list(size = 8)
        ),
        yaxis = list(
          title = "Frequency",      # 👈 add label
          showticklabels = TRUE,
          tickfont = list(size = 8)
        ),
        margin = list(l = 30, r = 10, t = 25, b = 30),
        showlegend = FALSE
      ) |>
      layout(shapes = list(
        list(
          type = "line",
          x0 = value_lga, x1 = value_lga,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = col_var, width = 3)
        )
      )) |>
      config(displayModeBar = FALSE)
  })
  
    
   
  
  
  #======================++#============#======================++#============#======================++#============
  
  
  output$hover_pct <- renderUI({
    lga <- hovered_lga(); if (is.null(lga)) return(NULL)
    base <- min(YEARS_ALL); last <- max(YEARS_ALL)
    dv_b <- dv_raw  |> filter(lga_name==lga, year==base) |> pull(rate_per_100k)
    dv_e <- dv_raw  |> filter(lga_name==lga, year==last) |> pull(rate_per_100k)
    rai_b<- rai_raw |> filter(lga_name==lga, year==base) |> pull(rai)
    rai_e<- rai_raw |> filter(lga_name==lga, year==last) |> pull(rai)
    r_b  <- rates   |> filter(year==base) |> pull(cash_rate)
    r_e  <- rates   |> filter(year==last) |> pull(cash_rate)
    pct  <- function(a,b) ifelse(length(a)&&length(b)&&!is.na(a)&&a!=0, round(100*(b-a)/a,1), NA)
    tags$small(HTML(glue("<b>Δ%</b> {base}→{last} | DV: <b>{pct(dv_b,dv_e)}%</b> | RAI: <b>{pct(rai_b,rai_e)}%</b> | Rate: <b>{pct(r_b,r_e)}%</b>")))
  })
  
  
}

# ==============================================================
# RUN
# ==============================================================
shinyApp(ui, server)




