library(shiny)

source("R/utils.R")
source("R/part1.R")
source("R/part2.R")
source("R/proxy.R")
source("R/engine.R")
source("R/validation.R")

cfg <- load_sherpa_config(".")
PROXY_ROOT <- Sys.getenv("SHERPA_PROXY_ROOT", unset="data/proxies")
PROXY_ROOT_PART1 <- Sys.getenv("SHERPA_PROXY_ROOT_PART1", unset=file.path(PROXY_ROOT,"Part1"))
PROXY_ROOT_PART2 <- Sys.getenv("SHERPA_PROXY_ROOT_PART2", unset=file.path(PROXY_ROOT,"Part2"))


legacy_cases <- read.csv(
  "data/validation/regression_legacy_12_cases.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

legacy_to_component <- c(
  soil_erosion = "erosion",
  landslide = "landslide",
  zinc = "metal_zn",
  antimony = "metal_sb",
  lead = "metal_pb",
  nickel = "metal_ni",
  mercury = "metal_hg",
  cobalt = "metal_co",
  chromium = "metal_cr",
  copper = "metal_cu",
  cadmium = "metal_cd",
  arsenic = "metal_as",
  salinity = "salinisation",
  nitrogen = "nitrogen",
  p_excess = "p_excess",
  p_mining = "p_mining",
  pesticide = "pesticide",
  soc = "soc",
  compaction = "compaction"
)

validation_category <- function(component) {
  if (component %in% c("erosion", "landslide", "compaction")) return("physical")
  if (grepl("^metal_", component) || component == "pesticide") return("contaminant")
  if (component %in% c("nitrogen", "p_excess", "p_mining")) return("nutrient")
  if (component == "salinisation") return("salinity")
  if (component == "soc") return("carbon")
  "neutral"
}

crop_choices <- setNames(cfg$crops$crop, paste0(cfg$crops$crop, " (", cfg$crops$n_yield_kgN_ha_yr, " kg N ha-1 yr-1)"))
bd_choices <- setNames(cfg$bulk_density$source_group, cfg$bulk_density$source_group)

lith_keys <- paste(cfg$lithology$sherpa_class, cfg$lithology$lithology, sep="::")
lith_labels <- paste0(cfg$lithology$lithology, " [", cfg$lithology$sherpa_class, "]")
lith_choices <- c("Unknown / enter SHERPA class manually"="", setNames(lith_keys, lith_labels))

cropland_js <- "(input.land_use == 'cropland' || input.land_use == 'orchard' || input.land_use == 'vineyard' || (input.land_use == 'grassland' && input.grassland_permanence == 'non_permanent'))"
permgrass_js <- "(input.land_use == 'grassland' && (input.grassland_permanence == 'permanent_all_year' || input.grassland_permanence == 'permanent_mediterranean'))"

details_panel <- function(title, ..., category="neutral", badge=NULL) {
  if (is.null(badge)) badge <- toupper(category)
  tags$details(
    class=paste("component-panel", paste0("cat-", category)),
    tags$summary(
      span(class="component-title", title),
      span(class=paste("category-badge", paste0("badge-", category)), badge)
    ),
    div(class="component-body", ...)
  )
}

ui <- fluidPage(
  tags$head(tags$link(rel="stylesheet", type="text/css", href="styles.css")),
  div(
    class="app-header",
    div(
      class="app-header-main",
      div(
        class="app-header-text",
        h1(class="app-title", "SHERPA field assessment — release v1.0"),
        p(class="subtitle",
          "Implementation of the published SHERPA decision key. Local/user field data are primary; optional pan-European proxies are used only for missing variables and with explicit provenance."
        ),
        div(
          class="release-metadata",
          p(
            strong("Method reference: "),
            "Alewell, C., Gupta, S., Poulenard, J., Niquille, N., Kaiser, A., Shokri, N., … & Borrelli, P. (2026). ",
            em("A first quantitative assessment of soil health at European scale considering soil genesis"),
            ". Journal of Plant Nutrition and Soil Science, 189(1), 6–16. DOI: 10.1002/jpln.70034."
          ),
          p(
            strong("Interface Developer: "),
            "Pasquale Borrelli, September 2026"
          )
        )
      ),
      div(
        class="app-header-logo-wrap",
        tags$img(
          src="ai4soilhealth_logo.png",
          class="app-header-logo",
          alt="AI4SoilHealth project logo"
        )
      )
    ),
    div(
      class="app-banner-wrap",
      tags$img(
        src="field_banner.jpg",
        class="app-banner",
        alt="Agricultural landscape banner"
      )
    )
  ),

  fluidRow(
      column(
        width=3,
        div(
          class="sherpa-sidebar",
          h4("Location & data"),
          p(class="small-note",
            "Define the assessment site and scope here. Local/user observations are the primary data source."),
          div(
            class="source-panel user-source-panel",
            div(class="source-panel-header",
                span(class="source-badge user-badge","USER / MANUAL"),
                strong("Site and local observations")),
            p(class="source-panel-note",
              "Primary source. Values entered here and in Parts 1–2 always override EU proxy data."),
            textInput("field_id","Field ID",value="Field_001"),
            fluidRow(
              column(6,textInput("lon","Longitude (WGS84)",placeholder="e.g. 12.50")),
              column(6,textInput("lat","Latitude (WGS84)",placeholder="e.g. 41.90"))
            ),
            selectInput("land_use","Observed land use",
              choices=c(
                "Forest / bushland"="forest",
                "Grassland"="grassland",
                "Cropland"="cropland",
                "Orchard"="orchard",
                "Vineyard"="vineyard",
                "Wetland"="wetland"
              )
            ),
            conditionalPanel(
              condition="input.land_use == 'grassland'",
              selectInput("grassland_permanence","Grassland permanence",
                choices=c(
                  "Unknown"="unknown",
                  "Permanent all year round >5 years"="permanent_all_year",
                  "Mediterranean: permanent winter/spring >5 years"="permanent_mediterranean",
                  "Non-permanent during last 5 years -> cropland route"="non_permanent"
                )
              )
            ),
            checkboxInput("drained_soil","Soil/site is drained",FALSE),
            checkboxInput("organic_soil_gt20","Organic carbon >20% (organic soil)",FALSE)
          ),

          div(
            class="source-panel settings-panel",
            div(class="source-panel-header",
                span(class="source-badge settings-badge","SETTINGS"),
                strong("Assessment settings")),
            selectInput("applicability_mode","Assessment mode",
              choices=c(
                "Standard SHERPA assessment (recommended)"="full_scheme",
                "Reproduce the published European study"="published_eu"
              )
            ),
            p(class="source-panel-note",
              "Standard mode uses all relevant SHERPA processes when valid data are available. The European-study mode reproduces the more restrictive process set used in the original pan-European assessment."),
            selectInput("aggregation_mode","How should missing Part 2 data be handled?",
              choices=c(
                "Use the mean of available relevant components (recommended)"="available_mean",
                "Calculate only when all relevant components are available"="complete_case"
              )
            )
          ),

          div(
            class="source-panel proxy-source-panel",
            div(class="source-panel-header",
                span(class="source-badge proxy-badge","EU PROXY"),
                strong("Pan-European fallback data")),
            p(class="source-panel-note",
              "Optional secondary source. Used only for variables that are missing from USER / MANUAL input."),
            checkboxInput("use_proxy","Use European raster data for missing inputs",FALSE),
            conditionalPanel(
              condition="input.use_proxy == true",
              textInput("proxy_root_part1","Part 1 raster folder",
                        value=PROXY_ROOT_PART1,
                        placeholder="e.g. C:/Users/.../SHERPA_rasters/PART1"),
              textInput("proxy_root_part2","Part 2 raster folder",
                        value=PROXY_ROOT_PART2,
                        placeholder="e.g. C:/Users/.../SHERPA_rasters/PART2"),
              actionButton("check_proxies","Check available raster data",class="btn-default"),
              uiOutput("proxy_root_status"),
              helpText("Select the Part 1 and Part 2 folders directly. WGS84 coordinates are transformed internally to each raster CRS. If a raster has no value at the selected location, the input remains missing.")
            ),
            p(class="source-panel-note",
              "European rasters are used only when a USER / MANUAL value is missing. Part 1 raster support is limited to forest climate, elevation, sand and lithology. FVC, humus/disturbance, structure and fertiliser must be supplied by the user.")
          ),

          div(
            class="sidebar-actions",
            actionButton("calculate","Refresh SHERPA results",class="btn-primary"),
            br(),br(),
            actionButton("example","Load cropland example")
          )
        )
      ),
      column(
        width=9,
        tabsetPanel(
          id="main_tabs",

          tabPanel(
          "Part 1 - intrinsic health",
          br(),
          uiOutput("route_banner"),
          uiOutput("part1_route_ui"),
          uiOutput("part1_live_preview")
        ),

          tabPanel(
          "Part 2 - degradation",
          br(),
          div(class="part2-intro",
            div(class="source-context-row",
                span(class="source-badge user-badge","USER / MANUAL"),
                strong("Part 2 local inputs")),
            h4("Part 2 — degradation processes"),
            p("Open each component below. These controls are USER / MANUAL inputs and therefore override any EU proxy. Leave an input blank when it is unknown; do not enter zero unless the measured/observed value is genuinely zero."),
            div(class="category-legend",
              span(class="legend-chip physical","Physical degradation"),
              span(class="legend-chip contaminant","Contaminants"),
              span(class="legend-chip nutrient","Nutrient imbalance"),
              span(class="legend-chip salinity","Salinisation"),
              span(class="legend-chip carbon","Soil carbon")
            )
          ),

          uiOutput("part2_proxy_inventory_ui"),

          details_panel(
            "2.1 Soil erosion",
            category="physical", badge="PHYSICAL",
            selectInput("erosion_method","Input method",
              choices=c("Unknown"="unknown","Direct cumulative erosion rate"="direct","Enter component erosion rates"="components")
            ),
            conditionalPanel(
              condition="input.erosion_method == 'direct'",
              textInput("erosion_total","Cumulative erosion rate (t ha-1 yr-1)",placeholder="e.g. 4.2")
            ),
            conditionalPanel(
              condition="input.erosion_method == 'components'",
              fluidRow(
                column(4,textInput("erosion_water","Water erosion","")),
                column(4,textInput("erosion_wind","Wind erosion (cropland)","")),
                column(4,textInput("erosion_tillage","Tillage erosion (cropland)",""))
              ),
              fluidRow(
                column(4,textInput("erosion_harvest","Harvest erosion (cropland)","")),
                column(4,checkboxInput("include_postfire_erosion","Include post-fire erosion mentioned in SI1 table caption",FALSE)),
                column(4,textInput("erosion_postfire","Post-fire erosion",""))
              ),
              helpText("Cropland uses cumulative water + wind + tillage + harvest. Forest and permanent grassland use water erosion only. Partial cropland component totals are not silently summed.")
            )
          ),

          details_panel(
            "2.1 Landslide density",
            category="physical", badge="PHYSICAL",
            selectInput("landslide_method","Scoring route",
              choices=c(
                "Unknown"="unknown",
                "Detailed landslide density (Table S1.2.1.2)"="detailed_density",
                "Hollis et al. coarse class used in current study (Table S1.2.1.2.1)"="hollis_class"
              )
            ),
            conditionalPanel(condition="input.landslide_method == 'detailed_density'",
              textInput("landslide_density","Landslides per km2",placeholder="e.g. 2.5")),
            conditionalPanel(condition="input.landslide_method == 'hollis_class'",
              selectInput("landslide_class","Hollis class",
                choices=c("Unknown"="unknown","No landslides"="none","Low (1-3 km-2)"="low",
                          "Medium (4-10 km-2)"="medium","High (11-268 km-2)"="high")))
          ),

          details_panel(
            "2.2 Heavy metals",
            category="contaminant", badge="CONTAMINANT",
            selectInput("metal_input_mode","Concentration input",
              choices=c("Single/bulk representative value"="bulk","Depth-resolved 0-10 / 10-20 / 20-30 cm"="depth_resolved")
            ),
            helpText("Each metal is an independent Part 2 indicator. For depth-resolved data, the most severe score among supplied upper/rooting layers is used."),
            uiOutput("metal_inputs_ui")
          ),

          details_panel(
            "2.3 Nitrogen surplus / load",
            category="nutrient", badge="NUTRIENT",
            selectInput("n_method","Nitrogen route",
              choices=c(
                "Land-use-specific field/local route (SI1 2.3.1-2.3.3)"="local",
                "Batool et al. 5-year surplus route used for the European point assessment (SI1 2.3.4)"="batool_proxy"
              )
            ),
            conditionalPanel(
              condition="input.n_method == 'batool_proxy'",
              textInput("n_surplus_batool","5-year mean N surplus (kg N ha-1 yr-1)",placeholder="e.g. 12")
            ),
            conditionalPanel(
              condition="input.n_method == 'local' && input.land_use == 'forest'",
              textInput("n_atmospheric_5y","5-year mean total atmospheric N deposition (kg N ha-1 yr-1)","")
            ),
            conditionalPanel(
              condition=paste0("input.n_method == 'local' && ",permgrass_js),
              textInput("n_total_inputs_5y","5-year mean total N input: deposition + mineral + organic fertiliser (kg N ha-1 yr-1)","")
            ),
            conditionalPanel(
              condition=paste0("input.n_method == 'local' && ",cropland_js),
              textInput("n_surplus_direct","Direct cropland N surplus, if already calculated (kg N ha-1 yr-1)",""),
              helpText("If direct surplus is blank, it is calculated from the fields below."),
              fluidRow(
                column(4,textInput("n_atmospheric_annual","Atmospheric deposition","")),
                column(4,textInput("n_mineral_fertilizer","Mineral fertiliser N","")),
                column(4,textInput("n_organic_fertilizer","Organic fertiliser N",""))
              ),
              fluidRow(
                column(6,textInput("n_harvest_export","Harvested N export (optional direct value)","")),
                column(6,selectInput("crop_name","Or use SHERPA crop N-yield lookup",
                                     choices=c("None / unknown"="",crop_choices)))
              )
            )
          ),

          details_panel(
            "2.4 Phosphorus mining / excess",
            category="nutrient", badge="NUTRIENT",
            p("Applicable to cropland and permanent grassland in the full scheme. Table S2.1 had no P-mining data for the European grassland evaluation."),
            textInput("p_balance_direct","Direct P balance (kg P ha-1 yr-1; input - harvested export)",""),
            helpText("If direct P balance is blank, it is calculated from the inputs below."),
            fluidRow(
              column(4,textInput("p_mineral_input","Mineral P input","")),
              column(4,textInput("p_organic_input","Organic P input","")),
              column(4,textInput("p_harvest_export","P harvested/exported",""))
            )
          ),

          details_panel(
            "2.5 Pesticide pressure",
            category="contaminant", badge="CONTAMINANT",
            p("Table S2.1 counts pesticide input for cropland/non-permanent grassland, not permanent grassland or forest."),
            selectInput("pesticide_method","Method",
              choices=c(
                "Not assessed"="none",
                "Tang et al. risk class + active ingredients (Table S1.2.5.2)"="Tang_RS_AI",
                "PLIFate (Table S1.2.5.1)"="PLIFate",
                "PLITotal (Table S1.2.5.1)"="PLITotal",
                "PLIUK x10^6 (Table S1.2.5.1)"="PLIUK"
              )
            ),
            conditionalPanel(condition="input.pesticide_method == 'Tang_RS_AI'",
              fluidRow(
                column(6,textInput("pesticide_rs","Pesticide risk class value (RS)","")),
                column(6,textInput("pesticide_ai","Number of active ingredients (AI)",""))
              )
            ),
            conditionalPanel(condition="input.pesticide_method == 'PLIFate' || input.pesticide_method == 'PLITotal' || input.pesticide_method == 'PLIUK'",
              textInput("pesticide_pli_value","PLI value","")
            )
          ),

          details_panel(
            "2.6 Salinisation",
            category="salinity", badge="SALINITY",
            selectInput("salinity_context","Origin/context",
              choices=c(
                "Unknown"="unknown",
                "Calcareous bedrock / carbonate or dolomite in fine earth"="calcareous_natural",
                "Within influence of marine waters"="marine_influence",
                "Semi-arid region without anthropogenic irrigation"="semiarid_nonirrigated",
                "Anthropogenically irrigated soil"="anthropogenic_irrigated"
              )
            ),
            conditionalPanel(condition="input.salinity_context == 'anthropogenic_irrigated'",
              textInput("ec","ECe of saturated soil extract (dS m-1)","")
            ),
            helpText("Natural/geogenic saline or calcareous contexts are routed to Part 1 and excluded from the Part 2 mean.")
          ),

          details_panel(
            "2.7 Compaction",
            category="physical", badge="PHYSICAL",
            selectInput("compaction_method","Assessment route",
              choices=c(
                "Not assessed"="none",
                "Field-observation route (preferred)"="field",
                "European bulk-density/texture proxy (Table S1.2.7.4)"="bulk_density_proxy"
              )
            ),
            conditionalPanel(
              condition="input.compaction_method == 'field' && input.land_use == 'forest'",
              textInput("compaction_surface_pct","Forest area with ruts/machinery disturbance and/or waterlogging (%)","")
            ),
            conditionalPanel(
              condition=paste0("input.compaction_method == 'field' && ",cropland_js," || input.compaction_method == 'field' && ",permgrass_js),
              textInput("compaction_surface_pct_ag","Surface area with waterlogging one day after rain and/or ruts/tracks (%)",""),
              selectInput("compaction_subsoil_class","Subsoil (>30 cm) profile class",
                choices=c(
                  "Unknown"="",
                  "0 - No signs"="0",
                  "1 - BD increasing; no stagnic/root restriction"="1",
                  "2 - First stagnic signs; no root restriction"="2",
                  "3 - Clear stagnic properties; no root restriction"="3",
                  "4 - Clear stagnic and/or first root restriction"="4",
                  "5 - Root restriction in 10-20% of profile"="5",
                  "6 - Root restriction in 21-30% of profile"="6",
                  "7 - Root restriction in 31-40% of profile"="7",
                  "8 - Root restriction in 41-50% of profile"="8",
                  "9 - Root restriction in >50% of profile"="9"
                )
              )
            ),
            conditionalPanel(
              condition="input.compaction_method == 'bulk_density_proxy'",
              fluidRow(
                column(6,textInput("bulk_density","Bulk density (g cm-3)","")),
                column(6,selectInput("bulk_density_texture_group","SI1 texture group",
                                     choices=c("Unknown"="",bd_choices)))
              ),
              helpText("The published European R implementation applies score 0 below the suitable threshold, 3 through the restrictive threshold, and 9 above the restrictive threshold.")
            )
          ),

          details_panel(
            "2.8 Soil organic carbon",
            category="carbon", badge="CARBON",
            fluidRow(
              column(4,textInput("soc_pct","Current SOC (%)","")),
              column(4,textInput("clay_pct","Clay (%)","")),
              column(4,textInput("soc_loss_9y","SOC change over 9 years (t C ha-1 9yr-1; negative = loss)",""))
            ),
            helpText("Cropland/permanent grassland: SOC loss + SOC/clay + SOC content are required. If any is missing, the SOC component is not scored and is reported as missing. Forest: SOC loss only.")
          )
        ),

          tabPanel(
          "Results",
          br(),
          p(class="small-note",
            "Results update automatically from the information currently available. A final SHERPA score is shown only when both Part 1 and at least one relevant Part 2 component can be evaluated."),
          uiOutput("result_headline"),
          uiOutput("result_status_cards"),
          h4("Part 1 detail"),
          tableOutput("part1_table"),
          h4("Part 2 process scores"),
          p(class="small-note",
            "Severity is shown as a positive degradation magnitude (0–9); negative_score is the value actually added to Part 1."),
          tableOutput("part2_table"),
          h4("Notes / source-sensitive decisions"),
          uiOutput("result_notes"),

          hr(),
          tags$details(
            class="result-details",
            tags$summary(strong("Input provenance")),
            br(),
            p(class="small-note",
              "Each input is identified as user-supplied, proxy-derived, or missing. User values always take precedence."),
            tableOutput("provenance_table")
          ),

          conditionalPanel(
            condition="input.use_proxy == true",
            tags$details(
              class="result-details",
              tags$summary(strong("European raster data used / missing")),
              br(),
              p(class="small-note","This table shows which European raster values were used and which could not be used. Missing raster information is never treated as zero."),
              tableOutput("proxy_log_table")
            )
          ),

          hr(),
          h4("Export"),
          downloadButton("download_csv","Download score table (CSV)"),
          tags$span(" "),
          downloadButton("download_gpkg","Download point GeoPackage"),
          br(),br(),
          helpText("GeoPackage export requires the R package 'sf' and valid WGS84 coordinates.")
        )
        )
      )
    )
)

server <- function(input, output, session) {

  proxy_inventory_reactive <- eventReactive(input$check_proxies, {
    proxy_inventory(cfg$proxy_manifest, proxy_roots_reactive())
  }, ignoreInit=TRUE)

  proxy_status_label <- function(status) {
    labels <- c(
      "READY"="Ready to use",
      "FILE MISSING"="File not found",
      "REVIEW REQUIRED"="Found - interpretation to be confirmed"
    )
    x <- as.character(status)
    ifelse(x %in% names(labels), unname(labels[x]), x)
  }

  part2_proxy_label <- function(variable) {
    labels <- c(
      erosion_water="Soil erosion - water",
      erosion_all="Soil erosion - all processes",
      landslide_density="Landslide density",
      Cu="Heavy metal - Cu",
      Hg="Heavy metal - Hg",
      Zn="Heavy metal - Zn",
      Cd="Heavy metal - Cd",
      Ni="Heavy metal - Ni",
      Pb="Heavy metal - Pb",
      Sb="Heavy metal - Sb",
      As="Heavy metal - As",
      Cr="Heavy metal - Cr",
      Co="Heavy metal - Co",
      n_surplus_batool="Nitrogen surplus",
      p_balance="Phosphorus",
      pesticide="Pesticide pressure",
      salinisation="Salinisation",
      soc_loss_9y="SOC loss",
      bulk_density="Bulk density",
      soil_texture="Soil texture (USDA)"
    )
    x <- as.character(variable)
    ifelse(x %in% names(labels), unname(labels[x]), x)
  }

  output$part2_proxy_inventory_ui <- renderUI({
    if (!isTRUE(input$use_proxy)) return(NULL)

    inv <- proxy_inventory_reactive()
    if (is.null(inv)) {
      return(div(
        class="source-panel proxy-source-panel main-source-panel",
        div(class="source-panel-header",
            span(class="source-badge proxy-badge","EU PROXY"),
            strong("European raster data for Part 2")),
        p(class="source-panel-note",
          "Click 'Check available raster data' in the left panel to see the Part 2 raster inventory.")
      ))
    }

    p2 <- inv[inv$section=="PART2",,drop=FALSE]
    found <- sum(p2$found)
    ready <- sum(p2$found & p2$auto_fill)
    review <- sum(p2$found & !p2$auto_fill)

    div(
      class="source-panel proxy-source-panel main-source-panel",
      div(class="source-panel-header",
          span(class="source-badge proxy-badge","EU PROXY"),
          strong("European raster data for Part 2")),
      p(class="source-panel-note",
        sprintf("%d/%d Part 2 raster layers found; %d configured for automatic use.",
                found,nrow(p2),ready)),
      p(class="source-panel-note",
        "Raster availability does not mean that every layer contributes at this site. SHERPA still applies the land-use rules (x / na / nr) and USER data retain priority."),
      if (review > 0)
        p(class="source-panel-note",
          sprintf("%d found layer(s) are listed but not yet used automatically because their interpretation still needs to be confirmed.",review))
      else NULL,
      tags$details(
        open=TRUE,
        tags$summary(strong("Show Part 2 raster layers")),
        br(),
        tableOutput("part2_proxy_inventory_table")
      )
    )
  })

  output$part2_proxy_inventory_table <- renderTable({
    inv <- proxy_inventory_reactive()
    if (is.null(inv)) return(NULL)
    p2 <- inv[inv$section=="PART2",,drop=FALSE]
    if (nrow(p2)==0) return(NULL)

    note_col <- if ("notes" %in% names(p2)) p2$notes else rep("",nrow(p2))

    data.frame(
      `SHERPA input`=part2_proxy_label(p2$variable),
      `Raster file`=p2$file,
      `Status`=proxy_status_label(p2$status),
      `Units / expected meaning`=ifelse(is.na(p2$units) | !nzchar(p2$units),"—",p2$units),
      `Note`=note_col,
      check.names=FALSE,
      stringsAsFactors=FALSE
    )
  },striped=TRUE,bordered=TRUE,na="")

  output$proxy_root_status <- renderUI({
    if (!isTRUE(input$use_proxy)) return(NULL)

    roots <- proxy_roots_reactive()

    if (!requireNamespace("terra",quietly=TRUE)) {
      return(div(class="warning-card",
                 strong("R package 'terra' is required for raster extraction."),
                 p("Install once with: install.packages('terra')")))
    }

    inv <- proxy_inventory_reactive()

    if (is.null(inv)) {
      return(div(
        class="proxy-status-card",
        strong("Raster folders"),
        p(sprintf("Part 1 folder: %s", if (dir.exists(roots$PART1)) "found" else "not found")),
        p(sprintf("Part 2 folder: %s", if (dir.exists(roots$PART2)) "found" else "not found")),
        p(class="small-note","Click 'Check available raster data' to inspect the files.")
      ))
    }

    p1 <- inv[inv$section=="PART1",,drop=FALSE]
    p2 <- inv[inv$section=="PART2",,drop=FALSE]

    p1_found <- sum(p1$found)
    p2_found <- sum(p2$found)
    p2_ready <- sum(p2$found & p2$auto_fill)
    p2_review <- sum(p2$found & !p2$auto_fill)

    div(
      class="proxy-status-card",
      strong("Raster data check"),
      p(sprintf("Part 1: %d/%d raster layer(s) found.",p1_found,nrow(p1))),
      p(sprintf("Part 2: %d/%d raster layer(s) found; %d configured for automatic use.",p2_found,nrow(p2),p2_ready)),
      if (p2_review > 0)
        p(sprintf("%d Part 2 layer(s) were found but are not yet activated because their interpretation still needs to be confirmed.",p2_review))
      else NULL
    )
  })

  effective_route_reactive <- reactive({
    rec <- list(
      drained_soil=isTRUE(input$drained_soil),
      organic_soil_gt20=isTRUE(input$organic_soil_gt20),
      land_use=input$land_use,
      grassland_permanence=char_or_na(input$grassland_permanence)
    )
    route_land_use(rec)
  })

  output$route_banner <- renderUI({
    r <- effective_route_reactive()
    txt <- switch(r,
      forest="Forest Part 1 route.",
      grassland="Permanent-grassland Part 1 route.",
      cropland="Cropland Part 1 route (also used for orchard, vineyard and non-permanent grassland).",
      not_considered="Not considered by the published SHERPA key: wetland/organic soil (>20% OC) and/or drained soil.",
      grassland_unresolved="Select grassland permanence to determine the Part 1 route.",
      "Unsupported route."
    )
    div(class=if (r=="not_considered") "route-banner warning-card" else "route-banner", strong(txt))
  })

  output$part1_route_ui <- renderUI({
    r <- effective_route_reactive()

    if (r == "cropland") {
      return(tagList(
        h4("Cropland / orchard / vineyard / non-permanent grassland"),
        div(class="source-panel user-source-panel main-source-panel",
          div(class="source-panel-header",
              span(class="source-badge user-badge","USER / MANUAL"),
              strong("Part 1 local inputs")),
          textInput("fvc_pct","Fractional vegetation cover (annual spatial-temporal mean, %)",""),
          selectInput("soil_structure","Prevailing A-horizon soil structure",
            choices=c(
              "Unknown"="unknown",
              "Granular"="granular",
              "Subangular"="subangular",
              "Angular or cloddy, aggregates <100 mm"="angular_cloddy_lt100",
              "Angular/cloddy >100 mm, or columnar, or platy"="angular_cloddy_gt100_columnar_platy",
              "Single grain or massive"="single_grain_massive"
            )
          ),
          checkboxInput("structure_after_8_weeks","Structure timing valid (>=8 weeks after ploughing, or no recent ploughing)",FALSE),
          textInput("organic_fertilizer_pct","Organic fertiliser as % of total fertiliser input",""),
          p(class="small-note","These Part 1 variables are USER-only. Part 1 = mean(FVC, structure, fertiliser). If structure is unavailable or its timing is not valid, SI1 Eq. 1.3.2 is used: mean(FVC, fertiliser), with higher uncertainty.")
        )
      ))
    }

    if (r == "grassland") {
      return(tagList(
        h4("Permanent grassland"),
        div(class="source-panel user-source-panel main-source-panel",
          div(class="source-panel-header",
              span(class="source-badge user-badge","USER / MANUAL"),
              strong("Part 1 local input")),
          textInput("fvc_pct","Fractional vegetation cover (%)",""),
          p(class="small-note","FVC is USER-only. SHERPA defines permanence over >5 years. Non-permanent grassland follows the cropland route.")
        )
      ))
    }

    if (r == "forest") {
      return(tagList(
        h4("Forest / bushland decision tree"),

        div(class="source-panel user-source-panel main-source-panel",
          div(class="source-panel-header",
              span(class="source-badge user-badge","USER / MANUAL"),
              strong("Forest local inputs")),
          p(class="source-panel-note",
            "Enter local values when available. These always override EU proxy values."),
          selectInput("forest_climate","Koppen-Geiger class",
            choices=c(
              "Unknown / not one of the published forest classes"="unknown",
              "Csa - temperate, dry summer, hot summer"="Csa",
              "Cfb - temperate, no dry season, warm summer"="Cfb",
              "Dfb - cold, no dry season, warm summer"="Dfb",
              "Dfc - cold, no dry season, cold summer"="Dfc",
              "ET - polar / tundra / alpine"="ET"
            )
          ),
          fluidRow(
            column(6,textInput("altitude_m","Altitude (m a.s.l.)","")),
            column(6,textInput("sand_pct","Sand content (%)",""))
          ),
          selectInput("specific_lithology","Specific lithology from SI2 Table S2.4 (optional)",
                      choices=lith_choices),
          selectInput("bedrock_class","SHERPA lithology class",
            choices=c(
              "Unknown"="unknown",
              "Calcareous / carbonate-bearing / siliceous base-rich"="base_rich",
              "Siliceous medium-base-content"="medium_base",
              "Expected sand >85% / high-sandy class"="high_sandy"
            )
          )
        ),

        uiOutput("forest_proxy_preview"),
        uiOutput("forest_branch_banner"),

        div(class="source-panel user-source-panel main-source-panel",
          div(class="source-panel-header",
              span(class="source-badge user-badge","USER / MANUAL"),
              strong("Forest humus condition / disturbance")),
          p(class="source-panel-note",
            "These observations have no EU raster surrogate and must be provided by the user."),
          uiOutput("forest_condition_ui"),
          uiOutput("forest_disturbance_ui")
        )
      ))
    }

    if (r == "not_considered") {
      return(div(class="warning-card",
        h4("No SHERPA score is assigned"),
        p("SI1 Table S1.0 marks wetlands/organic soils with >20% organic carbon and any drained land as not considered (n.c.) in this version of SHERPA.")
      ))
    }

    p("Resolve the land-use route to display Part 1 inputs.")
  })

  observeEvent(input$specific_lithology, {
    key <- input$specific_lithology
    if (is.null(key) || key == "") return()
    cls <- strsplit(key,"::",fixed=TRUE)[[1]][1]
    updateSelectInput(session,"bedrock_class",selected=cls)
  }, ignoreInit=TRUE)

  proxy_roots_reactive <- reactive({
    p1 <- char_or_na(input$proxy_root_part1)
    p2 <- char_or_na(input$proxy_root_part2)
    if (is.na(p1)) p1 <- PROXY_ROOT_PART1
    if (is.na(p2)) p2 <- PROXY_ROOT_PART2
    list(
      PART1=proxy_root_normalize(p1),
      PART2=proxy_root_normalize(p2)
    )
  })

  forest_effective_proxy_record <- reactive({
    rec <- list(
      lon=num_or_na(input$lon), lat=num_or_na(input$lat),
      land_use="forest", grassland_permanence=NA_character_,
      drained_soil=isTRUE(input$drained_soil), organic_soil_gt20=isTRUE(input$organic_soil_gt20),
      use_proxy=isTRUE(input$use_proxy),
      forest_climate=char_or_na(input$forest_climate),
      altitude_m=num_or_na(input$altitude_m),
      sand_pct=num_or_na(input$sand_pct),
      bedrock_class=char_or_na(input$bedrock_class)
    )
    src <- make_source_map(rec)
    if (isTRUE(rec$use_proxy)) return(resolve_forest_routing_proxies(rec,src,cfg$proxy_manifest,proxy_roots_reactive()))
    list(record=rec,sources=src,log=NULL)
  })

  output$forest_proxy_preview <- renderUI({
    if (effective_route_reactive() != "forest" || !isTRUE(input$use_proxy)) return(NULL)
    div(class="source-panel proxy-source-panel main-source-panel proxy-preview",
        div(class="source-panel-header",
            span(class="source-badge proxy-badge","EU PROXY"),
            strong("Effective forest proxy inputs")),
        p(class="source-panel-note",
          "Shown only where USER / MANUAL values are missing. If a value is missing, verify the Part 1 raster folder in the left panel."),
        tableOutput("forest_proxy_preview_table"))
  })

  output$forest_proxy_preview_table <- renderTable({
    if (effective_route_reactive() != "forest" || !isTRUE(input$use_proxy)) return(NULL)
    z <- forest_effective_proxy_record()
    data.frame(
      variable=c("Köppen climate route","Altitude (m)","Sand (%)","Lithology class"),
      value=c(
        ifelse(field_present(z$record$forest_climate),z$record$forest_climate,"MISSING"),
        ifelse(field_present(z$record$altitude_m),format(z$record$altitude_m),"MISSING"),
        ifelse(field_present(z$record$sand_pct),format(z$record$sand_pct),"MISSING"),
        ifelse(field_present(z$record$bedrock_class),
               sherpa_lithology_display(z$record$bedrock_class),
               "MISSING")
      ),
      source=c(z$sources[["forest_climate"]],z$sources[["altitude_m"]],z$sources[["sand_pct"]],z$sources[["bedrock_class"]]),
      stringsAsFactors=FALSE
    )
  },striped=TRUE,bordered=TRUE)

  forest_branch_reactive <- reactive({
    if (effective_route_reactive() != "forest") return("not_forest")
    z <- forest_effective_proxy_record()$record
    forest_branch(char_or_na(z$forest_climate),num_or_na(z$altitude_m),num_or_na(z$sand_pct),char_or_na(z$bedrock_class))
  })

  output$forest_branch_banner <- renderUI({
    b <- forest_branch_reactive()
    txt <- switch(b,
      high_sandy="Branch S1.1.2: ET and/or >1400 m and/or sand >=85% / high-sandy lithology.",
      base_rich="Branch S1.1.1.1: Csa/Cfb/Dfb/Dfc, <=1400 m, sand <85%, base-rich/calcareous.",
      medium_base="Branch S1.1.1.2: Csa/Cfb/Dfb/Dfc, <=1400 m, sand <85%, siliceous medium-base.",
      needs_bedrock="Climate-altitude-sand route resolved; select the bedrock/lithology class.",
      incomplete="Climate, altitude and sand/lithology information are insufficient to route the published forest key.",
      ""
    )
    if (!nzchar(txt)) return(NULL)
    div(class=if (b %in% c("needs_bedrock","incomplete")) "route-banner warning-card" else "route-banner",txt)
  })

  output$forest_condition_ui <- renderUI({
    b <- forest_branch_reactive()
    if (b == "base_rich") {
      return(selectInput("forest_condition","Organic layer / humus condition",
        choices=c(
          "Unknown"="",
          "Signs of disturbance -> use disturbance-area table"="disturbance",
          "Only litter layer; no developed O horizons"="score_10",
          "Only litter layer; patches of Oi developing"="score_9",
          "Oi <=1 cm"="score_8",
          "Oi <=1 cm and Oe <=0.5 cm"="score_7",
          "Oi 1-2 cm and Oe 0.5-1 cm"="score_6",
          "Oi 2-3 cm and Oe 1-2 cm"="score_5",
          "Oi 2-3 cm and Oe 1-2 cm + patches of Oa"="score_4",
          "Oi 2-3 cm and Oe 1-2 cm + Oa <1 cm"="score_3",
          "Oi 2-5 cm and Oe 1-3 cm + Oa 1-2 cm"="score_2",
          "Oi 2-5 cm and Oe 1-3 cm + Oa >2 cm"="score_1"
        )
      ))
    }
    if (b == "medium_base") {
      return(selectInput("forest_condition","Organic layer / disturbance condition",
        choices=c(
          "Unknown"="",
          "Oi <=1 cm; no disturbance"="score_10",
          "Oi <=1 cm + Oe <=0.5 cm; no disturbance"="score_9",
          "Oi 1-2 / Oe 0.5-1 cm OR <10% disturbance"="score_8",
          "Oi 2-3 / Oe 1-2 cm OR <15% disturbance"="score_7",
          "Oi 2-3 / Oe 1-2 + Oa patches OR <20% disturbance"="score_6",
          "Oi 2-3 / Oe 1-2 + Oa <1 cm OR <30% disturbance"="score_5",
          "Oi 2-5 / Oe 1-3 + Oa 1-2 cm OR <40% disturbance"="score_4",
          "Oi 2-5 / Oe 1-3 + Oa 2-3 cm OR <50% disturbance"="score_3",
          "Oi 2-5 / Oe 2-5 + Oa 3-4 cm OR <60% disturbance"="score_2",
          "Oi 2-5 / Oe 2-5 + Oa >4 cm OR >60% disturbance"="score_1"
        )
      ))
    }
    if (b == "high_sandy") {
      return(selectInput("forest_condition","Closed organic humus-layer coverage / disturbance",
        choices=c(
          "Unknown"="",
          "100% coverage; no disturbance"="score_10",
          ">95% coverage; <5% disturbed"="score_9",
          "90% coverage; <10% disturbed"="score_8",
          "80% coverage; <20% disturbed"="score_7",
          "70% coverage; published table says <3% disturbed"="score_6",
          "60% coverage; up to 40% disturbed"="score_5",
          "50% coverage; up to 50% disturbed"="score_4",
          "40% coverage; up to 60% disturbed"="score_3",
          "30% coverage; up to 70% disturbed"="score_2",
          "<30% coverage; >70% disturbed"="score_1"
        )
      ))
    }
    NULL
  })

  output$forest_disturbance_ui <- renderUI({
    if (forest_branch_reactive() == "base_rich" &&
        !is.null(input$forest_condition) && input$forest_condition == "disturbance") {
      textInput("forest_disturbance_pct","Area with mineral-soil / closed-humus-layer disturbance (%)","")
    } else NULL
  })

  output$metal_inputs_ui <- renderUI({
    bulk <- !identical(input$metal_input_mode, "depth_resolved")
    metal_units <- setNames(cfg$metals$units,cfg$metals$metal)

    if (bulk) {
      cards <- lapply(cfg$metals$metal, function(m) {
        div(class="metal-input-card",
          div(class="metal-label", paste0(m," (",metal_units[[m]],")")),
          textInput(paste0(m,"_bulk"),NULL,value="",placeholder="blank if unknown")
        )
      })
      return(div(class="metal-grid", cards))
    }

    rows <- lapply(cfg$metals$metal, function(m) {
      div(class="metal-depth-row",
        div(class="metal-depth-title", paste0(m," (",metal_units[[m]],")")),
        fluidRow(
          column(4,textInput(paste0(m,"_0_10"),"0–10 cm","")),
          column(4,textInput(paste0(m,"_10_20"),"10–20 cm","")),
          column(4,textInput(paste0(m,"_20_30"),"20–30 cm",""))
        )
      )
    })
    tagList(rows)
  })

  build_record <- reactive({
    r <- list(
      field_id=input$field_id,
      lon=num_or_na(input$lon),
      lat=num_or_na(input$lat),
      land_use=input$land_use,
      grassland_permanence=char_or_na(input$grassland_permanence),
      drained_soil=isTRUE(input$drained_soil),
      organic_soil_gt20=isTRUE(input$organic_soil_gt20),
      use_proxy=isTRUE(input$use_proxy),
      applicability_mode=coalesce_chr(input$applicability_mode,"full_scheme"),
      aggregation_mode=coalesce_chr(input$aggregation_mode,"available_mean"),

      # Part 1
      fvc_pct=num_or_na(input$fvc_pct),
      soil_structure=char_or_na(input$soil_structure),
      structure_after_8_weeks=isTRUE(input$structure_after_8_weeks),
      organic_fertilizer_pct=num_or_na(input$organic_fertilizer_pct),
      forest_climate=char_or_na(input$forest_climate),
      altitude_m=num_or_na(input$altitude_m),
      sand_pct=num_or_na(input$sand_pct),
      bedrock_class=char_or_na(input$bedrock_class),
      specific_lithology=char_or_na(input$specific_lithology),
      forest_condition=char_or_na(input$forest_condition),
      forest_disturbance_pct=num_or_na(input$forest_disturbance_pct),

      # Erosion
      erosion_method=char_or_na(input$erosion_method),
      erosion_total=num_or_na(input$erosion_total),
      erosion_water=num_or_na(input$erosion_water),
      erosion_wind=num_or_na(input$erosion_wind),
      erosion_tillage=num_or_na(input$erosion_tillage),
      erosion_harvest=num_or_na(input$erosion_harvest),
      include_postfire_erosion=isTRUE(input$include_postfire_erosion),
      erosion_postfire=num_or_na(input$erosion_postfire),

      # Landslide
      landslide_method=char_or_na(input$landslide_method),
      landslide_density=num_or_na(input$landslide_density),
      landslide_class=char_or_na(input$landslide_class),

      # Metals
      metal_input_mode=coalesce_chr(input$metal_input_mode,"bulk"),

      # N
      n_method=coalesce_chr(input$n_method,"local"),
      n_surplus_batool=num_or_na(input$n_surplus_batool),
      n_atmospheric_5y=num_or_na(input$n_atmospheric_5y),
      n_total_inputs_5y=num_or_na(input$n_total_inputs_5y),
      n_surplus_direct=num_or_na(input$n_surplus_direct),
      n_atmospheric_annual=num_or_na(input$n_atmospheric_annual),
      n_mineral_fertilizer=num_or_na(input$n_mineral_fertilizer),
      n_organic_fertilizer=num_or_na(input$n_organic_fertilizer),
      n_harvest_export=num_or_na(input$n_harvest_export),
      crop_name=char_or_na(input$crop_name),

      # P
      p_balance_direct=num_or_na(input$p_balance_direct),
      p_mineral_input=num_or_na(input$p_mineral_input),
      p_organic_input=num_or_na(input$p_organic_input),
      p_harvest_export=num_or_na(input$p_harvest_export),

      # Pesticide
      pesticide_method=coalesce_chr(input$pesticide_method,"none"),
      pesticide_rs=num_or_na(input$pesticide_rs),
      pesticide_ai=num_or_na(input$pesticide_ai),
      pesticide_pli_value=num_or_na(input$pesticide_pli_value),
      pesticide_proxy_score=NA_real_,

      # Salinity
      salinity_context=char_or_na(input$salinity_context),
      ec=num_or_na(input$ec),

      # Compaction
      compaction_method={ cm <- coalesce_chr(input$compaction_method,"none"); if (identical(cm,"none")) NA_character_ else cm },
      compaction_surface_pct=if (input$land_use=="forest") num_or_na(input$compaction_surface_pct) else num_or_na(input$compaction_surface_pct_ag),
      compaction_subsoil_class=num_or_na(input$compaction_subsoil_class),
      bulk_density=num_or_na(input$bulk_density),
      bulk_density_texture_group=char_or_na(input$bulk_density_texture_group),

      # SOC
      soc_pct=num_or_na(input$soc_pct),
      clay_pct=num_or_na(input$clay_pct),
      soc_loss_9y=num_or_na(input$soc_loss_9y)
    )

    for (m in cfg$metals$metal) {
      r[[paste0(m,"_bulk")]] <- num_or_na(input[[paste0(m,"_bulk")]])
      r[[paste0(m,"_0_10")]] <- num_or_na(input[[paste0(m,"_0_10")]])
      r[[paste0(m,"_10_20")]] <- num_or_na(input[[paste0(m,"_10_20")]])
      r[[paste0(m,"_20_30")]] <- num_or_na(input[[paste0(m,"_20_30")]])
    }
    r
  })

  part1_preview <- reactive({
    tryCatch({
      rec <- build_record()
      if (effective_route_reactive() == "forest" && isTRUE(rec$use_proxy)) {
        src <- make_source_map(rec)
        rec <- resolve_forest_routing_proxies(rec,src,cfg$proxy_manifest,proxy_roots_reactive())$record
      }
      calculate_part1(rec)
    },
      error=function(e) list(
        score=NA_real_,
        route=effective_route_reactive(),
        sub_scores=data.frame(component="Part 1",score=NA_real_),
        notes=paste0("Part 1 calculation issue: ",conditionMessage(e))
      )
    )
  })

  output$part1_live_preview <- renderUI({
    x <- part1_preview()
    if (is.null(x) || is_na_scalar(x$score)) {
      return(div(class="live-score-card pending",
        strong("Part 1 live score: "),
        "not yet calculable from the current inputs."
      ))
    }
    div(class="live-score-card complete",
      strong(sprintf("Part 1 live score: %.2f", x$score)),
      span(" — calculated independently of Part 2.")
    )
  })



  threshold_audit_result <- eventReactive(input$run_threshold_audit, {
    tryCatch(
      run_part2_threshold_audit(cfg),
      error = function(e) {
        data.frame(
          component = "Audit engine",
          source_table = "",
          case_id = "fatal_error",
          input = "",
          expected = "",
          observed = "",
          evidence = "SOURCE_DEFINED",
          status = "FAIL",
          note = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  output$threshold_audit_summary <- renderUI({
    req(threshold_audit_result())
    s <- summarise_part2_threshold_audit(threshold_audit_result())

    overall_class <- if (s$fail[1] == 0) "status-ok" else "status-pending"

    tagList(
      div(
        class = "status-grid",
        div(
          class = paste("status-card", overall_class),
          div(class = "status-title", "ALL ENFORCED TESTS"),
          div(class = "status-value", sprintf("%d PASS", s$pass[1])),
          div(class = "status-sub", sprintf("%d source-defined/anchor · %d original-code · %d project-decision cases", s$source_defined[1], s$original_code[1], s$project_decision[1]))
        ),
        div(
          class = paste("status-card", if (s$fail[1] == 0) "status-ok" else "status-pending"),
          div(class = "status-title", "FAILURES"),
          div(class = "status-value", as.character(s$fail[1])),
          div(class = "status-sub", "Must be zero before scientific sign-off")
        ),
        div(
          class = "status-card status-pending",
          div(class = "status-title", "SCIENTIFIC FLAGS"),
          div(class = "status-value", as.character(s$flag[1])),
          div(class = "status-sub", "Unresolved scientific matters only")
        )
      ),
      if (s$fail[1] == 0) {
        div(class = "result-card validation-pass",
            h4("Threshold audit: all enforced source rules and approved v1.7 decisions pass"),
            p(if (s$flag[1] == 0) "No unresolved scientific FLAG remains in this audit." else "FLAG rows identify unresolved scientific matters only."))
      } else {
        div(class = "result-card warning-card",
            h4("Threshold audit: source-defined mismatch detected"),
            p("Filter the table to FAIL and review before using the affected raw-input pathway."))
      }
    )
  })

  output$threshold_audit_table <- renderTable({
    req(threshold_audit_result())
    d <- threshold_audit_result()
    filt <- coalesce_chr(input$threshold_audit_filter, "all")
    if (!identical(filt, "all")) d <- d[d$status == filt, , drop = FALSE]
    d
  }, striped = TRUE, bordered = TRUE, hover = TRUE, na = "NA")

  output$download_threshold_audit <- downloadHandler(
    filename = function() {
      paste0("SHERPA_v1_8_11_Part2_threshold_audit_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(threshold_audit_result())
      write.csv(threshold_audit_result(), file, row.names = FALSE, na = "")
    }
  )

  validation_components <- reactive({
    route <- coalesce_chr(input$validation_route, "cropland")
    mode <- coalesce_chr(input$validation_applicability, "full_scheme")
    rr <- cfg$applicability[cfg$applicability$effective_land_use == route, , drop = FALSE]
    if (identical(mode, "published_eu")) {
      rr <- rr[rr$published_eu, , drop = FALSE]
    } else {
      rr <- rr[rr$full_scheme, , drop = FALSE]
    }
    rr$component
  })

  output$validation_class_ui <- renderUI({
    comps <- validation_components()
    if (length(comps) == 0) return(p("No applicable components for this route."))

    categories <- c(
      physical = "Physical degradation",
      contaminant = "Contaminants",
      nutrient = "Nutrient imbalance",
      salinity = "Salinisation",
      carbon = "Soil carbon"
    )

    sections <- lapply(names(categories), function(cat) {
      these <- comps[vapply(comps, validation_category, character(1)) == cat]
      if (length(these) == 0) return(NULL)

      cards <- lapply(these, function(comp) {
        div(
          class = paste("validation-class-card", paste0("val-", cat)),
          div(class = "validation-class-label", part2_component_label(comp)),
          textInput(
            paste0("val_", comp),
            NULL,
            value = "",
            placeholder = "0-9; blank = missing"
          )
        )
      })

      div(
        class = paste("validation-section", paste0("val-section-", cat)),
        h5(categories[[cat]]),
        div(class = "validation-class-grid", cards)
      )
    })

    tagList(sections)
  })

  selected_legacy_row <- reactive({
    pid <- coalesce_chr(input$legacy_pointid, as.character(legacy_cases$POINTID[1]))
    row <- legacy_cases[as.character(legacy_cases$POINTID) == pid, , drop = FALSE]
    if (nrow(row) != 1) return(NULL)
    row
  })

  output$legacy_case_summary <- renderUI({
    row <- selected_legacy_row()
    if (is.null(row)) return(NULL)

    div(
      class = "legacy-summary",
      h5(paste0("POINTID ", row$POINTID)),
      div(class = "status-grid",
        div(class = "status-card status-ok",
            div(class = "status-title", "LEGACY PART 1"),
            div(class = "status-value", sprintf("%.6f", row$part1)),
            div(class = "status-sub", "Stored reference value")),
        div(class = "status-card status-ok",
            div(class = "status-title", "LEGACY PART 2 SEVERITY"),
            div(class = "status-value", sprintf("%.6f", row$expected_part2_severity)),
            div(class = "status-sub", "Stored reference magnitude")),
        div(class = "status-card status-ok",
            div(class = "status-title", "LEGACY FINAL SHERPA"),
            div(class = "status-value", sprintf("%.6f", row$expected_final_sherpa)),
            div(class = "status-sub", "Stored reference value"))
      ),
      p(class = "small-note",
        "All 12 supplied legacy cases are cropland cases and contain the 19 pre-scored Part 2 components."
      )
    )
  })

  build_manual_validation_severity_map <- reactive({
    comps <- validation_components()
    out <- setNames(vector("list", length(comps)), comps)
    for (comp in comps) {
      out[[comp]] <- num_or_na(input[[paste0("val_", comp)]])
    }
    out
  })

  build_legacy_severity_map <- reactive({
    row <- selected_legacy_row()
    if (is.null(row)) return(list())

    out <- list()
    for (legacy_name in names(legacy_to_component)) {
      comp <- unname(legacy_to_component[[legacy_name]])
      out[[comp]] <- as.numeric(row[[legacy_name]][1])
    }
    out
  })

  validation_result <- eventReactive(input$calculate_validation, {
    tryCatch({
      if (safe_eq(input$validation_mode, "legacy")) {
        row <- selected_legacy_row()
        if (is.null(row)) stop("Legacy regression case could not be loaded.")

        calculate_sherpa_prescored(
          part1_score = row$part1[1],
          route = "cropland",
          severity_map = build_legacy_severity_map(),
          applicability_mode = "full_scheme",
          aggregation_mode = "available_mean",
          app_cfg = cfg$applicability,
          field_id = paste0("Legacy_", row$POINTID[1]),
          source_label = "LEGACY_REGRESSION"
        )
      } else {
        calculate_sherpa_prescored(
          part1_score = num_or_na(input$validation_part1),
          route = coalesce_chr(input$validation_route, "cropland"),
          severity_map = build_manual_validation_severity_map(),
          applicability_mode = coalesce_chr(input$validation_applicability, "full_scheme"),
          aggregation_mode = coalesce_chr(input$validation_aggregation, "available_mean"),
          app_cfg = cfg$applicability,
          field_id = "Manual_pre_scored_validation",
          source_label = "PRE_SCORED"
        )
      }
    }, error = function(e) {
      list(fatal_error = conditionMessage(e))
    })
  })

  output$validation_headline <- renderUI({
    req(validation_result())
    x <- validation_result()

    if (!is.null(x$fatal_error)) {
      return(div(class = "result-card warning-card",
                 h3("Validation calculation failed"),
                 p(x$fatal_error)))
    }

    p1_ok <- !is_na_scalar(x$part1$score)
    p2_ok <- !is.null(x$part2) && !is_na_scalar(x$part2$mean)
    final_ok <- !is_na_scalar(x$final)

    div(
      class = "status-grid",
      div(class = paste("status-card", if (p1_ok) "status-ok" else "status-pending"),
          div(class = "status-title", "PART 1"),
          div(class = "status-value", if (p1_ok) sprintf("%.6f", x$part1$score) else "Pending"),
          div(class = "status-sub", "Pre-scored validation value")),
      div(class = paste("status-card", if (p2_ok) "status-ok" else "status-pending"),
          div(class = "status-title", "PART 2 SEVERITY"),
          div(class = "status-value", if (p2_ok) sprintf("%.6f", x$part2$severity_magnitude) else "Pending"),
          div(class = "status-sub",
              if (p2_ok) sprintf("Contribution %.6f", x$part2$mean)
              else sprintf("%d/%d classes available", x$part2$n_available, x$part2$n_expected))),
      div(class = paste("status-card", if (final_ok) "status-ok" else "status-pending"),
          div(class = "status-title", "FINAL SHERPA"),
          div(class = "status-value", if (final_ok) sprintf("%.6f", x$final) else "Pending"),
          div(class = "status-sub", "Part 1 + negative Part 2 contribution"))
    )
  })

  output$validation_part2_table <- renderTable({
    req(validation_result())
    x <- validation_result()
    if (!is.null(x$fatal_error) || is.null(x$part2)) return(NULL)
    d <- x$part2$table
    d[d$applicable, c("label", "severity", "negative_score", "s2_1_status"), drop = FALSE]
  }, digits = 6, striped = TRUE, bordered = TRUE, na = "Missing")

  output$validation_comparison <- renderUI({
    req(validation_result())
    if (!safe_eq(input$validation_mode, "legacy")) return(NULL)

    x <- validation_result()
    row <- selected_legacy_row()
    if (!is.null(x$fatal_error) || is.null(row)) return(NULL)

    part2_delta <- abs(x$part2$severity_magnitude - row$expected_part2_severity[1])
    final_delta <- abs(x$final - row$expected_final_sherpa[1])

    pass <- part2_delta <= 1e-6 && final_delta <= 1e-6

    div(
      class = if (pass) "result-card validation-pass" else "result-card warning-card",
      h4(if (pass) "Legacy regression comparison: PASS" else "Legacy regression comparison: CHECK"),
      p(sprintf(
        "Part 2 severity: calculated %.9f; stored %.9f; |difference| = %.3g",
        x$part2$severity_magnitude,
        row$expected_part2_severity[1],
        part2_delta
      )),
      p(sprintf(
        "Final SHERPA: calculated %.9f; stored %.9f; |difference| = %.3g",
        x$final,
        row$expected_final_sherpa[1],
        final_delta
      )),
      p(class = "small-note", "Acceptance tolerance for the rounded legacy file: 1e-6.")
    )
  })

  result_raw <- reactive({
    # Explicit dependency lets the Refresh button force a recalculation,
    # while all build_record() inputs also update results automatically.
    input$calculate

    tryCatch(
      calculate_sherpa(build_record(),cfg,proxy_roots_reactive()),
      error=function(e) list(
        fatal_error=conditionMessage(e),
        record=build_record(),
        sources=make_source_map(build_record()),
        route=effective_route_reactive(),
        part1=part1_preview(),
        part2=NULL,
        final=NA_real_,
        proxy_log=NULL,
        notes=paste0("Internal calculation error: ",conditionMessage(e))
      )
    )
  })

  # Avoid repeated raster extraction while the user is still typing.
  result <- debounce(result_raw, 350)


  output$result_headline <- renderUI({
    req(result())
    x <- result()

    if (!is.null(x$fatal_error)) {
      return(div(class="result-card warning-card",
        h3("Calculation could not be completed"),
        p("Part 1 remains shown below if it could be calculated."),
        p(class="technical-error", x$fatal_error)
      ))
    }

    if (identical(x$route, "not_considered")) {
      return(div(class="result-card warning-card",
        h3("SHERPA score: not considered by the published key"),
        p("This site routes to the wetland/organic/drained category marked n.c. in SI1 Table S1.0.")
      ))
    }

    p1_available <- !is.null(x$part1) && !is_na_scalar(x$part1$score)
    p2_available <- !is.null(x$part2) && !is_na_scalar(x$part2$mean)

    if (!p2_available) {
      n_avail <- if (is.null(x$part2)) 0L else x$part2$n_available
      n_exp <- if (is.null(x$part2)) 0L else x$part2$n_expected
      return(div(class="result-card partial-card",
        h2(if (p1_available) sprintf("Part 1 intrinsic soil health: %.2f",x$part1$score) else "Part 1 intrinsic soil health: not yet calculable"),
        p("Final SHERPA score: pending Part 2 degradation information."),
        p(sprintf("Part 2 components currently scored: %d/%d.",n_avail,n_exp)),
        if (!is.null(x$part2) && length(x$part2$missing_components) > 0)
          div(class="warning-card",
              strong("Incomplete Part 2 assessment"),
              p("Missing applicable indicators are excluded from the mean and are not treated as zero degradation."),
              p(paste0("Missing: ", paste(x$part2$missing_components, collapse=", "), ".")))
      ))
    }

    if (is_na_scalar(x$final)) {
      return(div(class="result-card partial-card",
        h2(if (p1_available) sprintf("Part 1 intrinsic soil health: %.2f",x$part1$score) else "Part 1 intrinsic soil health: not yet calculable"),
        p(sprintf("Mean Part 2 degradation severity: %.2f", -x$part2$mean)),
        p(sprintf("Part 2 contribution to SHERPA: %.2f", x$part2$mean)),
        p("Final SHERPA score is pending because Part 1 is incomplete.")
      ))
    }

    div(class="result-card",
      h2(sprintf("SHERPA score: %.2f",x$final)),
      p(sprintf("Part 1 intrinsic soil health: %.2f",x$part1$score)),
      p(sprintf("Mean Part 2 degradation severity: %.2f", -x$part2$mean)),
      p(sprintf("Part 2 contribution to SHERPA: %.2f", x$part2$mean)),
      p(sprintf("Part 2 completeness: %d/%d applicable components (%.1f%%)",
                x$part2$n_available,x$part2$n_expected,100*x$part2$completeness)),
      if (x$part2$n_available < x$part2$n_expected) {
        div(class="warning-card",
            strong("Incomplete Part 2 assessment"),
            p("The score uses the available applicable degradation indicators. Missing indicators are excluded from the mean and are not treated as zero degradation."),
            if (length(x$part2$missing_components) > 0)
              p(paste0("Missing: ", paste(x$part2$missing_components, collapse=", "), "."))
        )
      }
    )
  })

  output$result_status_cards <- renderUI({
    req(result())
    x <- result()
    if (!is.null(x$fatal_error) || is.null(x$part1)) return(NULL)

    p1_ok <- !is_na_scalar(x$part1$score)
    p2_n <- if (is.null(x$part2)) 0L else x$part2$n_available
    p2_e <- if (is.null(x$part2)) 0L else x$part2$n_expected
    p2_pct <- if (is.null(x$part2) || is_na_scalar(x$part2$completeness)) 0 else 100*x$part2$completeness
    p2_ok <- !is.null(x$part2) && !is_na_scalar(x$part2$mean)

    div(class="status-grid",
      div(class=paste("status-card",if (p1_ok) "status-ok" else "status-pending"),
          div(class="status-title","PART 1"),
          div(class="status-value",if (p1_ok) sprintf("%.2f",x$part1$score) else "Pending"),
          div(class="status-sub","Intrinsic soil health")),
      div(class=paste("status-card",if (p2_ok) "status-ok" else "status-pending"),
          div(class="status-title","PART 2 SEVERITY"),
          div(class="status-value",if (p2_ok) sprintf("%.2f",-x$part2$mean) else "Pending"),
          div(class="status-sub",if (p2_ok)
              sprintf("Contribution %.2f · %d/%d components · %.0f%%",x$part2$mean,p2_n,p2_e,p2_pct)
            else sprintf("%d/%d components · %.0f%%",p2_n,p2_e,p2_pct))),
      div(class=paste("status-card",if (!is_na_scalar(x$final)) "status-ok" else "status-pending"),
          div(class="status-title","FINAL SHERPA"),
          div(class="status-value",if (!is_na_scalar(x$final)) sprintf("%.2f",x$final) else "Pending"),
          div(class="status-sub","Part 1 + negative Part 2 contribution"))
    )
  })

  output$part1_table <- renderTable({
    req(result())
    x <- result()
    if (is.null(x$part1) || is.null(x$part1$sub_scores)) return(NULL)

    d <- x$part1$sub_scores
    value <- ifelse(is.na(d$score), "Not assessed", sprintf("%.2f", d$score))

    if (identical(x$route, "forest") && "Forest branch" %in% d$component) {
      branch_text <- if (!is.null(x$part1$branch_label) && nzchar(x$part1$branch_label)) {
        x$part1$branch_label
      } else {
        forest_branch_label(x$part1$branch)
      }
      value[d$component == "Forest branch"] <- branch_text
    }

    data.frame(component=d$component, value=value, stringsAsFactors=FALSE)
  },striped=TRUE,bordered=TRUE,na="")

  output$part2_table <- renderTable({
    req(result())
    x <- result()
    if (is.null(x$part2)) return(NULL)
    d <- x$part2$table
    d[,c("label","applicable","s2_1_status","severity","negative_score","detail")]
  },digits=2,striped=TRUE,bordered=TRUE,na="Not assessed")

  output$result_notes <- renderUI({
    req(result())
    notes <- result()$notes
    notes <- notes[!is.na(notes) & nzchar(notes)]
    if (length(notes)==0) return(p("No additional notes."))
    tags$ul(lapply(notes,tags$li))
  })

  output$provenance_table <- renderTable({
    req(result())
    x <- result()
    vals <- x$record
    data.frame(
      variable=names(vals),
      value=vapply(vals,function(z) {
        if (length(z)==0 || is.na(z[1])) "" else paste(z,collapse=",")
      },character(1)),
      source=vapply(names(vals),function(nm) {
        z <- x$sources[[nm]]
        if (is.null(z) || length(z)==0 || is.na(z[1])) "MISSING" else z[1]
      },character(1)),
      stringsAsFactors=FALSE
    )
  },striped=TRUE,bordered=TRUE)

  output$proxy_log_table <- renderTable({
    req(result())
    x <- result()
    if (is.null(x$proxy_log) || nrow(x$proxy_log)==0) return(NULL)

    d <- x$proxy_log[,c("variable","status","raw_value","resolved_value","file","note"),drop=FALSE]
    status_labels <- c(
      "OK"="Raster value used",
      "PROXY_EU"="European raster used",
      "PROXY_EU_IMPUTED"="European raster used (model-filled source)",
      "PROXY_EU_GAP"="No raster data at this location",
      "PROXY_EU_UNSUPPORTED_CLASS"="Raster class not supported by published SHERPA",
      "PROXY_EU_SANDY_HINT"="Sandy lithology detected; sand % decides the route",
      "PROXY_FILE_MISSING"="Raster file not found",
      "PROXY_REVIEW_REQUIRED"="Raster available but not yet activated",
      "COORDINATES_MISSING"="Coordinates missing",
      "TERRA_NOT_INSTALLED"="Raster support not available in R"
    )
    d$status <- ifelse(d$status %in% names(status_labels),
                       unname(status_labels[d$status]),
                       d$status)
    names(d) <- c("Input","Raster status","Raster value","Value used","File","Details")
    d
  },striped=TRUE,bordered=TRUE,na="")

  output$download_csv <- downloadHandler(
    filename=function() paste0(input$field_id,"_SHERPA_v1_8_11.csv"),
    content=function(file) {
      x <- result(); req(x)
      if (is.null(x$part2)) {
        out <- data.frame(field_id=x$record$field_id,route=x$route,
                          part1=x$part1$score,
                          part2_severity=NA_real_,
                          part2_contribution=NA_real_,
                          sherpa=NA_real_)
      } else {
        out <- x$part2$table
        out$field_id <- x$record$field_id
        out$route <- x$route
        out$part1 <- x$part1$score
        out$part2_severity <- -x$part2$mean
        out$part2_contribution <- x$part2$mean
        out$sherpa <- x$final
      }
      write.csv(out,file,row.names=FALSE)
    }
  )

  output$download_gpkg <- downloadHandler(
    filename=function() paste0(input$field_id,"_SHERPA_v1_8_11.gpkg"),
    content=function(file) {
      if (!requireNamespace("sf",quietly=TRUE)) stop("Package 'sf' is required.")
      x <- result(); req(x)
      if (is.na(x$record$lon) || is.na(x$record$lat)) stop("Longitude and latitude are required.")
      dat <- data.frame(
        field_id=x$record$field_id,
        route=x$route,
        part1=x$part1$score,
        part2_severity=if (is.null(x$part2)) NA_real_ else -x$part2$mean,
        part2_contribution=if (is.null(x$part2)) NA_real_ else x$part2$mean,
        sherpa=x$final,
        lon=x$record$lon,
        lat=x$record$lat
      )
      obj <- sf::st_as_sf(dat,coords=c("lon","lat"),crs=4326,remove=FALSE)
      sf::st_write(obj,file,driver="GPKG",delete_dsn=TRUE,quiet=TRUE)
    }
  )
}

shinyApp(ui,server)
