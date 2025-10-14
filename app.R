# TikTok Doomscroller v0.1
## Author: Chris Vazquez (chris_v@mit.edu)
### A Shiny app to help coders manually review TikTok videos from a list of URLs in a Google Sheet


### Workflow:
### Step 1: Provide a Google Sheet URL with a column "URL" containing TikTok video links
### Step 2: Load the URLs, which randomizes their order
### Step 3: For each video, answer the coding questions and save responses
### Step 4: Responses are appended to a "responses" worksheet in the same Google Sheet

# ----- Configuration -----

library(shiny)
library(tidyverse)
library(googlesheets4)
library(shinyjs)

CODER_NAMES <- c("Minerva", "Sarita", "Mona", "Evan", "Claire", "Cami")
# FIXME: Test URL sheet
SHEET_URL <- "https://docs.google.com/spreadsheets/d/1BdZ-Ikbd-7yvS8CmrXLTtAHO6k_Hxi6hraanp_svoCY/edit?usp=sharing" 
options(gargle_oauth_email = TRUE)
gs4_auth(cache = ".secrets")

# ----- Helper Functions -----
extract_video_id <- function(url) {
  id <- str_match(url, "/video/([0-9]{8,})")[,2]
  id
}
extract_user_id <- function(url) {
  uid <- str_match(url, "tiktok\\.com/@([^/]+)/video/")[,2]
  uid
}
make_tiktok_iframe <- function(video_id){
  req(video_id)
  tags$iframe(
    src  = sprintf("https://www.tiktok.com/embed/v2/%s?lang=en-US", video_id),
    width = "480", height = "852",  # 16:9-ish tall, larger than before
    style = "border:none; width:100%; max-width: 520px; aspect-ratio: 9/16;",
    allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
    allowfullscreen = NA
  )
}
ensure_responses_ws <- function(ss, header){
  nms <- tryCatch(sheet_names(ss), error = function(e) character())
  if(!"responses" %in% nms){
    sheet_add(ss, sheet = "responses")
    sheet_write(data = header[0,], ss = ss, sheet = "responses")
  }
}
read_source_urls <- function(ss_url){
  dat <- read_sheet(ss_url, .name_repair = "minimal")
  validate(need("URL" %in% names(dat), "The Google Sheet must have a column named 'URL'."))
  dat <- dat %>%
    mutate(URL = trimws(as.character(URL))) %>%
    filter(!is.na(URL) & nzchar(URL)) %>%
    mutate(video_id = extract_video_id(URL),
           user_id  = extract_user_id(URL))
  validate(need(any(!is.na(dat$video_id)), "No valid TikTok /video/{id} links found in column 'URL'."))
  dat
}

# ----- Coding Questions -----
# 1) Big Raid? -> binary coded 1 (Yes) / 0 (No) saved as characters "1"/"0"
# 2) Agency?   -> open-ended text
question_spec <- list(
  # Event metadata
  list(id="event_date", type="text", label="Event date (DD-MM-YYYY)"),
  list(id="event_time", type="text", label="Event time (HH:MM)"),
  list(id="event_state", type="text", label="State (2-letter)"),
  list(id="event_city", type="text", label="City"),
  list(id="event_neighborhood", type="text", label="Neighborhood"),
  list(id="event_address", type="text", label="Street address"),
  list(id="event_location_text", type="textArea", label="Location description"),
  
  # Location type + conditional other
  list(id="location_type", type="select", label="Location type",
       choices=c("business_or_workplace","parking_lot","public_street","checkpoint",
                 "courthouse","private_residence","other")),
  list(id="location_type_other", type="text", label="If 'other', describe",
       show_if=list(field="location_type", equals="other")),
  
  # Raid & agencies
  list(id="big_raid", type="radio", label="Big raid?", choices=c("No"="0","Yes"="1")),
  list(id="federal_agencies", type="select", label="Federal agencies involved",
       choices=c("ICE","CBP","DEAR","ATF","FBI","national_guard","other_unclear")),
  
  # Local police + conditional agency text
  list(id="local_police_presence", type="radio", label="Local/state police present?",
       choices=c("No"="0","Yes"="1")),
  list(id="local_police_agencies", type="text",
       label="Name local/state police agencies (if any)",
       show_if=list(field="local_police_presence", equals="1")),
  
  # Counts & equipment
  list(id="agent_count_visible", type="text", label="Agents visible (count)"),
  list(id="agent_cars_visible", type="text", label="Agent vehicles visible (count)"),
  list(id="Masks_visible", type="text", label="Agents wearing masks (count)"),
  list(id="tacticalvests_visible", type="radio", label="Any tactical vests?",
       choices=c("No"="0","Yes"="1")),
  list(id="other_militarized_equipment", type="textArea",
       label="Other militarized equipment"),
  
  # Agent demographics & language
  list(id="any_latino_agents", type="radio", label="Any agents appear Latino/Hispanic?",
       choices=c("No"="0","Yes"="1")),
  list(id="any_black_agents", type="radio", label="Any agents appear Black/African American?",
       choices=c("No"="0","Yes"="1")),
  list(id="officer_speak_spanish", type="radio", label="Any officers speak Spanish?",
       choices=c("No"="0","Yes"="1")),
  
  # Bystanders: counts & estimates
  list(id="bystander_count_visible", type="text", label="Bystanders visible (count)"),
  list(id="bystander_est", type="select", label="Estimated crowd size",
       choices=c("<5","5-10","10-20","20+")),
  list(id="bystander_other_phones_visible_count", type="text",
       label="Other recording phones visible (count)"),
  
  # Bystander actions (all binary unless noted)
  list(id="bystanders_engage_officers", type="radio", label="Bystanders engage officers?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_engage_targets", type="radio", label="Bystanders engage targets?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_question_officers", type="radio", label="Bystanders question officers?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_refer_identity", type="radio", label="Bystanders refer to officers' identity?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_legal_language_warrants", type="radio",
       label="Bystanders use legal language / mention warrants?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_yell_at_officers", type="radio", label="Bystanders yell at officers?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_curse_or_insult", type="radio", label="Bystanders curse/insult?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_english", type="radio", label="Bystanders speak English?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_spanish", type="radio", label="Bystanders speak Spanish?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_otherlanguage", type="radio", label="Bystanders speak other languages?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_body_blocking", type="radio", label="Bystanders body-block officers?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_holding_target", type="radio", label="Bystanders hold onto target?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_holding_officer", type="radio", label="Bystanders hold back officers?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_org_membership", type="radio", label="Bystanders identify as org members?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_throw", type="radio", label="Bystanders throw objects?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_cars", type="radio", label="Bystanders use cars strategically?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_bodyblockade", type="radio", label="Bystanders form a body blockade?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_chase", type="radio", label="Bystanders chase/follow agents?",
       choices=c("No"="0","Yes"="1")),
  list(id="bystanders_brieftext", type="text", label="Brief description of bystander actions"),
  
  # Outcomes (all binary)
  list(id="outcomes_ICE_emptyhanded", type="radio", label="ICE leaves empty-handed?",
       choices=c("No"="0","Yes"="1")),
  
  list(id="outcome_target_ICE_pulls_gun", type="radio", label="Officers pull gun on target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_shoves", type="radio", label="Officers shove target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_tackles", type="radio", label="ICE tackles target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_restraints", type="radio", label="Restrain target physically?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_punches_or_kicks", type="radio",
       label="Officers punch/kick target?", choices=c("No"="0","Yes"="1")),
  list(id="outcomes_target_arrest_binary", type="radio", label="Target arrested?",
       choices=c("No"="0","Yes"="1")),
  
  list(id="outcome_bystander_ICE_pulls_gun", type="radio", label="Pull gun on bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_shoves", type="radio", label="Shove bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_tackles", type="radio", label="Tackle bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_restraints", type="radio", label="Restrain bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_punches_or_kicks", type="radio",
       label="Punch/kick bystander?", choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_carhit", type="radio", label="Bystander hit by vehicle?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystanders_ICE_threaten", type="radio", label="ICE threatens bystanders?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_ICE_takephone", type="radio", label="ICE takes someone’s phone?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_ICE_blockphone", type="radio", label="ICE blocks filming/observation?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystanders_arrested", type="radio", label="Bystanders arrested?",
       choices=c("No"="0","Yes"="1"))
)
question_header <- function(){
  tibble::tibble(!!!setNames(rep(list(character()), length(question_spec)),
                             vapply(question_spec, function(q) q$id, character(1))))
}


# ----- UI -----
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML('
    .topbar { position:sticky; top:0; z-index:100; background:#fff; border-bottom:1px solid #eee; padding:0.75rem 0; }
    .controls { display:flex; flex-wrap:wrap; align-items:center; gap:.5rem; }
    .meta { font-size:.9rem; color:#666; margin-left:.5rem; }
    .player-col { display:flex; justify-content:center; }
    .player-sticky { position: sticky; top: 72px; }  /* keep video in view; match topbar height */
    .qa-panel { max-height: calc(100vh - 96px); overflow: auto; } /* independent scroll for questions */
    .q-card { background:#fafafa; border:1px solid #eee; border-radius:12px; padding:1rem; }
    .viewer-iframe { width:100%; max-width:520px; aspect-ratio:9/16; border:none; }
  '))),
  titlePanel(NULL),
  
  # Top controls (no sidebar)
  div(class = "topbar",
      div(class = "container-fluid",
          div(class = "controls",
              h3("TikTok DoomScroller", style="margin:0 1rem 0 0;"),
              selectInput("coder_id", label = NULL, choices = CODER_NAMES, width = "200px", selected = CODER_NAMES[1]),
              actionButton("load_sheet", "Load URLs", class = "btn btn-primary"),
              actionButton("save_and_next", "Save response & next", class = "btn btn-success"),
              actionButton("next_random", "Next random →", class = "btn btn-primary"),
              actionButton("skip", "Skip"),
              span(class = "meta", textOutput("position", inline = TRUE))
          )
      )
  ),
  
  # Main content: left = sticky video, right = scrollable questions
  div(class = "container-fluid",
      fluidRow(
        column(
          width = 7, class = "player-col",
          div(class = "player-sticky",
              # keep it minimal so the iframe sizes correctly
              uiOutput("embed", container = div)
          )
        ),
        column(
          width = 5,
          div(class = "q-card qa-panel",
              h4("Questions"),
              uiOutput("questionnaire")
          )
        )
      )
  )
)


# ----- Server -----

server <- function(input, output, session){
  rv <- reactiveValues(
    ss = NULL,
    data = NULL,
    order = integer(0),
    idx_pos = 1L,
    log = tibble::tibble(),
    ready = FALSE
  )
  
  # Arrow keys
  runjs('document.addEventListener("keydown", function(e){
           if(e.key === "ArrowRight"){ Shiny.setInputValue("_k_next", Math.random()); }
           if(e.key === "ArrowLeft"){ Shiny.setInputValue("_k_prev", Math.random()); }
         });')
  observeEvent(input$`_k_next`, { nextRandom() }, ignoreInit = TRUE)
  
  # Load sheet (uses fixed SHEET_URL)
  observeEvent(input$load_sheet, {
    showNotification("Loading Google Sheet…", type = "message", duration = 2)
    dat <- read_source_urls(SHEET_URL)
    rv$ss <- SHEET_URL
    rv$data <- dat
    n <- nrow(dat)
    rv$order <- sample.int(n)
    rv$idx_pos <- 1L
    rv$ready <- TRUE
    
    header <- tibble::tibble(
      timestamp     = as.character(Sys.time()),
      coder_id      = character(),
      index_random  = integer(),
      index_source  = integer(),
      URL           = character(),
      user_id       = character(),
      video_id      = character()
    ) |> dplyr::bind_cols(question_header())
    ensure_responses_ws(rv$ss, header)
  })
  
  # Current row helpers
  cur_index <- reactive({ req(rv$ready); rv$order[[rv$idx_pos]] })
  cur_row   <- reactive({ rv$data[cur_index(), , drop = FALSE] })
  
  output$position <- renderText({ req(rv$ready); paste0(rv$idx_pos, " / ", nrow(rv$data)) })
  
  # Advance function
  nextRandom <- function(){
    req(rv$ready)
    if(rv$idx_pos < length(rv$order)){
      rv$idx_pos <- rv$idx_pos + 1L
    } else {
      showNotification("You reached the end of the randomized list.", type = "message")
    }
  }
  observeEvent(input$next_random, { nextRandom() })
  observeEvent(input$skip,        { nextRandom() })
  
  # Embed (left)
  output$embed <- renderUI({
    req(rv$ready)
    vid <- cur_row()$video_id
    validate(need(!is.na(vid), "This row doesn't have a parsable video ID (expected .../video/VIDEOID)."))
    # TikTok embed includes native UI (scrubber, play/pause) — nothing extra to do.
    make_tiktok_iframe(vid)
  })
  
  # Questionnaire (right)
  output$questionnaire <- renderUI({
    req(rv$ready)
    tagList(
      # Show the URL for reference (read-only)
      div(style="margin-bottom:.5rem; font-size:.9rem; color:#666; word-break:break-all;",
          strong("URL: "), cur_row()$URL),
      lapply(question_spec, function(q){
        switch(q$type,
               radio    = radioButtons(q$id, q$label, choices = q$choices, inline = TRUE),
               text     = textInput(q$id, q$label),
               textArea = textAreaInput(q$id, q$label, rows = 4),
               checkbox = checkboxGroupInput(q$id, q$label, choices = q$choices),
               select   = selectInput(q$id, q$label, choices = q$choices),
               div(class = "text-danger", sprintf("Unknown question type: %s", q$type))
        )
      })
    )
  })
  
  # Gather + Save
  gather_responses <- function(){
    vals <- lapply(question_spec, function(q){
      v <- input[[q$id]]
      if(is.null(v)) "" else if(is.character(v)) paste(v, collapse = ", ") else as.character(v)
    })
    setNames(as.list(vals), vapply(question_spec, function(q) q$id, character(1)))
  }
  save_response <- function(){
    req(rv$ready)
    row <- cur_row()
    payload <- tibble::tibble(
      timestamp    = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      coder_id     = if (is.null(input$coder_id)) "" else input$coder_id,
      index_random = rv$idx_pos,
      index_source = cur_index(),
      URL          = row$URL,
      user_id      = row$user_id,
      video_id     = row$video_id
    ) |> dplyr::bind_cols(as_tibble(gather_responses()))
    tryCatch({
      googlesheets4::sheet_append(ss = rv$ss, data = payload, sheet = "responses")
      rv$log <- dplyr::bind_rows(rv$log, payload)
      TRUE
    }, error = function(e){
      showNotification(paste("Write error:", e$message), type = "error"); FALSE
    })
  }
  
  observeEvent(input$save_and_next, {
    ok <- save_response()
    if(ok){
      showNotification("Saved.", type = "message", duration = 1.5)
      nextRandom()
      updateTextInput(session, "agency", value = "")  # clear free text after save
    }
  })
}

shinyApp(ui, server)


