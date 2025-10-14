# TikTok Doomscroller
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
  list(id = "big_raid", label = "Big Raid?", type = "radio", choices = c("No" = "0", "Yes" = "1")),
  list(id = "agency",   label = "Agency?",   type = "text")
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
    .viewer-wrap { display:flex; justify-content:flex-start; }
    .player-col { display:flex; justify-content:center; }
    .q-card { background:#fafafa; border:1px solid #eee; border-radius:12px; padding:1rem; }
    .btn-primary { background:#1f6feb; border-color:#1f6feb; }
    .btn-success { background:#2da44e; border-color:#2da44e; }
  '))),
  titlePanel(NULL),  # remove default big title spacing
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
  
  # Main content: left = video (bigger), right = questions
  div(class = "container-fluid",
      fluidRow(
        column(
          width = 7, class = "player-col",
          div(class = "viewer-wrap",
              div(style = "width:100%; max-width:520px;", uiOutput("embed", container = div))
          )
        ),
        column(
          width = 5,
          div(class = "q-card",
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


