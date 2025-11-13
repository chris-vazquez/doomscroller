# TikTok Doomscroller v2
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

# Google Sheet 
SHEET_URL <- Sys.getenv("SHEET_URL")

# Tabs in the Google sheet:
ASSIGN_TAB    <- Sys.getenv("ASSIGN_TAB",    unset = "assignments")
RESPONSES_TAB <- Sys.getenv("RESPONSES_TAB", unset = "responses")
CURSOR_TAB    <- Sys.getenv("CURSOR_TAB",    unset = "cursors")

# RA Names for the dropdown menu
CODER_NAMES <- c("Cami", "Claire", "Evan", "Minerva", "Mona", "Sarita", "Chris")

# safe null-coalescing for utility
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Personal error tracing for local dev
if (interactive()) options(shiny.fullstacktrace = TRUE)


# ----- Authentication -----

# accessing the JSON key
auth_gsheets <- function() {
  sa <- Sys.getenv("GCP_SA_JSON", unset = "")

# this is so that authentication can happen locally or when deployed
# not necessary to run but necessary to be flexible while developing
  if (nzchar(sa)) {
    # Case A: env var points to a file
    if (file.exists(sa)) {
      googlesheets4::gs4_auth(path = sa)
      message("✔ Authenticated with service account (file).")
      return(invisible(TRUE))
    }
    
    # Case B: env var contains JSON contents directly
    tf <- tempfile(fileext = ".json")
    writeLines(sa, tf, useBytes = TRUE)
    googlesheets4::gs4_auth(path = tf)
    message("✔ Authenticated with service account (from env string).")
    return(invisible(TRUE))
  }
  
  # Local development fallback: OAuth sign-in
  if (interactive()) {
    options(gargle_oauth_email = TRUE)
    googlesheets4::gs4_auth(cache = ".secrets")
    message("✔ Authenticated via OAuth (local development).")
    return(invisible(TRUE))
  }
  
  # Fail fast if deployed without credentials
  stop("❌ GCP_SA_JSON not set. Configure a service account before deploying.")
}

# Authenticate once at startup
auth_gsheets()


# ----- Helper Functions -----
# Small utilities used across the app structure

# Parse info from TikTok URLs
extract_video_id <- function(url) {
  id <- stringr::str_match(url, "/video/([0-9]{8,})")[,2]
  id
}
extract_user_id <- function(url) {
  uid <- stringr::str_match(url, "tiktok\\.com/@([^/]+)/video/")[,2]
  uid
}

# Embed player that returns the iframe for playing the video
make_tiktok_iframe <- function(video_id){
  req(video_id)
  tags$iframe(
    src  = sprintf("https://www.tiktok.com/embed/v2/%s?lang=en-US&autoplay=1&muted=0", video_id),
    width = "480", height = "852",
    style = "border:none; width:100%; max-width: 520px; aspect-ratio: 9/16;",
    allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
    allowfullscreen = NA
  )
}

# These functions are to help with cleaning URLs
canonical_tiktok_url <- function(user_id, video_id) {
  user_id  <- as.character(user_id)
  video_id <- as.character(video_id)
  is_bad <- is.na(user_id) | user_id == "" | is.na(video_id) | video_id == ""
  out <- sprintf("https://www.tiktok.com/@%s/video/%s", user_id, video_id)
  out[is_bad] <- NA_character_
  out
}

strip_query <- function(url) {
  url <- as.character(url)
  sub("([?#].*)$", "", url)
}

# Creates tabs if missing, ensure expected headers are there
ensure_tabs <- function(ss) {
  # Helper: get tab names (empty vector on error)
  tab_names <- tryCatch(googlesheets4::sheet_names(ss), error = function(e) character())
  
  # Helper: read header-only (no rows) with minimal name repair
  header_of <- function(tab) {
    names(googlesheets4::read_sheet(ss, sheet = tab, n_max = 0, .name_repair = "minimal"))
  }
  
  # Helper: write a header row into an empty tab
  write_header_row <- function(tab, headers) {
    empty_df <- tibble::as_tibble_row(setNames(rep(list(character()), length(headers)), headers))[0, ]
    googlesheets4::sheet_write(empty_df, ss = ss, sheet = tab)
  }
  
  # Expected headers for RESPONSES; if you define question_header(), include its names
  resp_expected <- c(
    "timestamp", "coder_id", "index_order", "URL", "user_id", "video_id",
    "relevant_video", "link_active"
  )
  if (exists("question_header") && is.function(question_header)) {
    resp_expected <- c(resp_expected, names(question_header()))
  }
  
  # RESPONSES tab
  if (!(RESPONSES_TAB %in% tab_names)) {
    googlesheets4::sheet_add(ss, RESPONSES_TAB)
    write_header_row(RESPONSES_TAB, resp_expected)
  } else {
    hdr <- header_of(RESPONSES_TAB)
    if (!identical(hdr, resp_expected)) {
      # If empty, rewrite header; if non-empty, attempt a gentle upgrade or stop
      rs <- googlesheets4::read_sheet(ss, sheet = RESPONSES_TAB, .name_repair = "minimal")
      if (nrow(rs) == 0) {
        write_header_row(RESPONSES_TAB, resp_expected)
      } else {
        missing <- setdiff(resp_expected, hdr)
        if (length(missing)) {
          stop(
            "The '", RESPONSES_TAB, "' tab is missing required columns: ",
            paste(missing, collapse = ", "),
            ". Please fix the header row."
          )
        }
      }
    }
  }
  
  # ASSIGNMENTS tab
  assign_expected <- c("coder_id", "index_order", "URL", "user_id", "video_id")
  if (!(ASSIGN_TAB %in% tab_names)) {
    googlesheets4::sheet_add(ss, ASSIGN_TAB)
    write_header_row(ASSIGN_TAB, assign_expected)
  } else {
    hdr <- header_of(ASSIGN_TAB)
    if (!identical(hdr, assign_expected)) {
      stop(
        "The '", ASSIGN_TAB, "' tab must have headers: ",
        paste(assign_expected, collapse = ", ")
      )
    }
  }
  
  # CURSORS tab (one row per coder; we overwrite their latest row)
  cursor_expected <- c("coder_id", "index_order", "video_id", "started_at")
  if (!(CURSOR_TAB %in% tab_names)) {
    googlesheets4::sheet_add(ss, CURSOR_TAB)
    write_header_row(CURSOR_TAB, cursor_expected)
  } else {
    hdr <- header_of(CURSOR_TAB)
    if (!identical(hdr, cursor_expected)) {
      cr <- googlesheets4::read_sheet(ss, sheet = CURSOR_TAB, .name_repair = "minimal")
      if (nrow(cr) == 0) {
        write_header_row(CURSOR_TAB, cursor_expected)
      } else {
        missing <- setdiff(cursor_expected, hdr)
        if (length(missing)) {
          stop(
            "The '", CURSOR_TAB, "' tab is missing columns: ",
            paste(missing, collapse = ", "),
            ". Please fix the header row."
          )
        }
      }
    }
  }
  
  invisible(TRUE)
}

# Pull a coder's ordered assignment list from ASSIGNMENTS and normalize columns.
read_assignments_for <- function(ss, coder_id) {
  df <- googlesheets4::read_sheet(ss, sheet = ASSIGN_TAB, .name_repair = "minimal")
  validate(need(nrow(df) > 0, "Assignments tab is empty. Add rows to 'assignments'."))
  
  df <- df %>%
    dplyr::filter(coder_id == !!coder_id) %>%
    dplyr::mutate(
      URL      = trimws(as.character(URL)),
      video_id = dplyr::if_else(is.na(video_id) | video_id == "", extract_video_id(URL), video_id),
      user_id  = dplyr::if_else(is.na(user_id)  | user_id  == "", extract_user_id(URL),  user_id),
      canonical_url = dplyr::coalesce(canonical_tiktok_url(user_id, video_id), strip_query(URL))
    ) %>%
    dplyr::arrange(index_order)
  
  validate(need(nrow(df) > 0, sprintf("No assignments for coder '%s' in 'assignments'.", coder_id)))
  df
}

# Return sorted unique index_order values coder has completed (from RESPONSES)
read_completed_idx <- function(ss, coder_id) {
  if (!(RESPONSES_TAB %in% googlesheets4::sheet_names(ss))) return(integer(0))
  
  rs <- googlesheets4::read_sheet(ss, sheet = RESPONSES_TAB, .name_repair = "minimal")
  if (!"index_order" %in% names(rs)) return(integer(0))
  
  idx <- rs %>%
    dplyr::filter(coder_id == !!coder_id) %>%
    dplyr::pull(index_order)
  
  idx <- suppressWarnings(as.integer(as.character(unlist(idx))))
  idx <- idx[!is.na(idx)]
  sort(unique(idx))
}

read_cursor <- function(ss, coder_id) {
  crs <- googlesheets4::read_sheet(ss, sheet = CURSOR_TAB, .name_repair = "minimal")
  crs %>% dplyr::filter(coder_id == !!coder_id) %>% dplyr::slice_tail(n = 1)
}

# Upsert cursor: if coder has no row, append; else overwrite their last row.
set_cursor <- function(ss, coder_id, index_order, video_id) {
  crs <- googlesheets4::read_sheet(ss, sheet = CURSOR_TAB, .name_repair = "minimal")
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  row <- tibble::tibble(
    coder_id    = coder_id,
    index_order = index_order,
    video_id    = video_id,
    started_at  = now
  )
  
  if (nrow(crs) == 0 || !(coder_id %in% crs$coder_id)) {
    # First time we see this coder → append
    googlesheets4::sheet_append(ss, data = row, sheet = CURSOR_TAB)
    return(invisible(TRUE))
  }
  
  # Overwrite the most recent row for this coder (preserves "one row per coder" contract)
  crs$.__row__ <- seq_len(nrow(crs)) + 1L  # +1 for header
  hit <- crs %>% dplyr::filter(coder_id == !!coder_id) %>% dplyr::slice_tail(n = 1)
  
  googlesheets4::range_write(
    ss, data = row, sheet = CURSOR_TAB,
    range = paste0("A", hit$.__row__[1]), col_names = FALSE
  )
  invisible(TRUE)
}

# Clear cursor by overwriting coder's row with a neutral record (index 0, blank video)
clear_cursor <- function(ss, coder_id) {
  crs <- googlesheets4::read_sheet(ss, sheet = CURSOR_TAB, .name_repair = "minimal")
  if (nrow(crs) == 0 || !(coder_id %in% crs$coder_id)) return(invisible())
  
  crs$.__row__ <- seq_len(nrow(crs)) + 1L
  hit <- crs %>% dplyr::filter(coder_id == !!coder_id) %>% dplyr::slice_tail(n = 1)
  
  row <- tibble::tibble(
    coder_id    = coder_id,
    index_order = 0L,
    video_id    = "",
    started_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  googlesheets4::range_write(
    ss, data = row, sheet = CURSOR_TAB,
    range = paste0("A", hit$.__row__[1]), col_names = FALSE
  )
  invisible(TRUE)
}



# Header Correction when needed
##migrate_responses_header <- function(ss) {
  # Read current data (keeps all existing rows)
  #resp <- googlesheets4::read_sheet(ss, sheet = RESPONSES_TAB, .name_repair = "minimal")
  
  # Build the expected header (matches your app logic)
  #expected <- c(
    #"timestamp","coder_id","index_order","URL","user_id","video_id",
    #"relevant_video","link_active",        # <-- new gate flags
    #names(question_header())               # <-- includes ALL question_spec ids,
    #     e.g. spanish_majority, video_complete
  #)
  
  # Add any missing columns as blank character columns
  #missing <- setdiff(expected, names(resp))
  #if (length(missing)) {
  #  for (col in missing) resp[[col]] <- NA_character_
  #}
  
  # Keep only expected columns in the expected order
  #resp <- resp[, expected, drop = FALSE]
  
  # Overwrite the sheet with corrected header + the same data
  #googlesheets4::sheet_write(resp, ss = ss, sheet = RESPONSES_TAB)
  
  #invisible(TRUE)
#}


# ----- Coding Questions -----
# 1) Big Raid? -> binary coded 1 (Yes) / 0 (No) saved as characters "1"/"0"
# 2) Agency?   -> open-ended text
question_spec <- list(
  # Beginning Flags
  ## Spanish Video?
  list(id="spanish_majority", type="radio",
       label="Is most dialogue in Spanish?",
       choices=c("No"="0","Yes"="1")),
  ## Complete Event?
  list(id="video_complete", type="radio",
       label="Does this video show a complete event (beginning/middle/end)?",
       choices=c("No"="0","Yes"="1")),
  
  
  # Event metadata
  list(id="event_date", type="text", label="Event date (MM-DD-YYYY)"),
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
  list(id="federal_agencies", type="checkbox", label="Federal agencies involved",
       choices=c("ICE","CBP","DEA","ATF","FBI","national_guard","other_unclear")),
  
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
       choices=c("<5","5-9","10-19","20+")),
  list(id="bystander_other_phones_cameras_visible_count", type="text",
       label="Other recording phones/cameras visible (count)"),
  
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
  list(id="bystanders_discuss_masks", type="radio", label="Does anyone talk about the officers wearing masks?",
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
  list(id="outcomes_ICE_emptyhanded", type="radio", label="Officers leaves empty-handed?",
       choices=c("No"="0","Yes"="1")),
  
  list(id="outcome_target_ICE_pulls_gun", type="radio", label="Officers pull gun on target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_shoves", type="radio", label="Officers shove target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_tackles", type="radio", label="Officers tackles target?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_restraints", type="radio", label="Officers restrain target physically?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_target_ICE_punches_or_kicks", type="radio",
       label="Officers punch/kick target?", choices=c("No"="0","Yes"="1")),
  list(id="outcomes_target_arrest_binary", type="radio", label="Target arrested?",
       choices=c("No"="0","Yes"="1")),
  
  list(id="outcome_bystander_ICE_pulls_gun", type="radio", label="Officers pull gun on bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_shoves", type="radio", label="Officers shove bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_tackles", type="radio", label="Officers tackle bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_restraints", type="radio", label="Officers restrain bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_punches_or_kicks", type="radio",
       label="Officers punch/kick bystander?", choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_gases" , type="radio", label="Officers use tear gas/smoke grenade on bystander?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_carhit", type="radio", label="Bystander hit by vehicle?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystanders_ICE_threaten", type="radio", label="Officer threatens bystanders?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystander_ICE_ignores", type="radio", label="Officer ignores bystanders?",
       choices = c("No" = "0", "Yes" = "1")),
  list(id="outcome_bystander_ICE_stepback", type="radio", label="Officer tells bystanders to step back?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_ICE_takephone", type="radio", label="Officer takes someone’s phone?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_ICE_blockphone", type="radio", label="Officer blocks filming/observation?",
       choices=c("No"="0","Yes"="1")),
  list(id="outcome_bystanders_arrested", type="radio", label="Bystanders arrested?",
       choices=c("No"="0","Yes"="1")),
  list(id="other_comments", type="textArea",
       label="Additional comments?")
)
question_header <- function(){
  tibble::tibble(!!!setNames(rep(list(character()), length(question_spec)),
                             vapply(question_spec, function(q) q$id, character(1))))
}

# ----- UI -----
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML('
    .topbar { position: sticky; top: 0; z-index: 1000; background:#fff; border-bottom:1px solid #eee; padding:0.75rem 0; }
    .controls { display:flex; flex-wrap:wrap; align-items:center; gap:.5rem; }
    .meta { font-size:.9rem; color:#666; margin-left:.5rem; }

    /* instructions banner directly under topbar */
    .ra-instructions { position: sticky; top: 56px; z-index: 900; background:#f7fafc; border:1px solid #eaecef; border-radius:8px; padding:.75rem; margin:.5rem 0; }

    /* left column player */
    .player-col { display:flex; justify-content:center; align-items:center; height: calc(100vh - 96px); }
    /* raise the sticky offset so it clears both topbar + banner */
    .player-sticky { position: sticky; top: 160px; z-index: 1; }

    .qa-panel { max-height: calc(100vh - 96px); overflow: auto; }
    .q-card { background:#fafafa; border:1px solid #eee; border-radius:12px; padding:1rem; }
    .viewer-iframe { width:100%; max-width:600px; aspect-ratio:9/16; border:none; }
  '))),
  titlePanel(NULL),
  
  # Top controls
  div(class = "topbar",
      div(class = "container-fluid",
          div(class = "controls",
              h3("Bystander Video Coding", style="margin:0 1rem 0 0;"),
              selectInput("coder_id", label = NULL, choices = CODER_NAMES,
                          width = "200px", selected = CODER_NAMES[1]),
              actionButton("load_list", "Load / Resume", class = "btn btn-primary"),
              actionButton("save_and_next", "Save & Next", class = "btn btn-success"),
              actionButton("mark_irrelevant", "Mark Irrelevant", class = "btn btn-warning"),
              actionButton("mark_link_broken", "Mark Link Broken", class = "btn btn-danger"),
              span(class = "meta", textOutput("position", inline = TRUE)),
              span(class = "meta", textOutput("current_info", inline = TRUE))
          )
      )
  ),
  
  # Instructions banner (sticky under topbar)
  div(class="container-fluid",
      div(class="ra-instructions",
          tags$b("Workflow: "),
          tags$ol(
            tags$li("Select your name from the dropdown and click ", tags$b("Load/Resume"), "to begin each session."),
            tags$li("Code only what you can see/hear. Click ", tags$b("Save & Next"), "when you're done with each video."),
            tags$li("If a video seems really irrelvant, click ", tags$b("Mark Irrelevant"), 
                    ". If the video appears broken or is not loading correctly, click", tags$b("Mark Link Broken"), "."),
            tags$li("Use the original TikTok link (below) if you need full context/comments. Make sure you're signed into your TikTok account.")
          )
      )
  ),
  
  # Main content: left = video, right = questions
  div(class = "container-fluid",
      fluidRow(
        column(
          width = 7, class = "player-col",
          div(class = "player-sticky",
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
    ss      = NULL,
    data    = NULL,
    idx_pos = 1L,
    ready   = FALSE
  )
  
  
  observeEvent(input$load_list, {
    req(input$coder_id)
    showNotification("Loading your list…", type = "message", duration = 2)
    
    rv$ss <- SHEET_URL
    ensure_tabs(rv$ss)
    
    # pull assignments for this coder
    rv$data <- read_assignments_for(rv$ss, input$coder_id)
    
    # figure out where to start: cursor if present, else first remaining item
    cur <- read_cursor(rv$ss, input$coder_id)
    completed_idx <- read_completed_idx(rv$ss, input$coder_id)
    
    if (nrow(cur) > 0 && !is.na(cur$index_order) && cur$index_order > 0) {
      pos <- which(rv$data$index_order == cur$index_order)[1]
      rv$idx_pos <- ifelse(length(pos) == 1, pos, 1L)
    } else {
      remaining <- setdiff(rv$data$index_order, completed_idx)
      if (length(remaining) == 0) {
        rv$idx_pos <- 1L
        showNotification("All assigned videos are completed. 🎉", type = "message")
      } else {
        next_idx_order <- min(remaining)
        rv$idx_pos <- which(rv$data$index_order == next_idx_order)[1]
        set_cursor(rv$ss, input$coder_id, next_idx_order, rv$data$video_id[[rv$idx_pos]])
      }
    }
    
    rv$ready <- TRUE
  })
  
  # Current row (safe single-row tibble)
  cur_row <- reactive({
    req(rv$ready)
    rv$data[rv$idx_pos, , drop = FALSE]
  })
  
  # compute next position from sheet state (uses completed rows)
  next_position <- function() {
    completed_idx <- read_completed_idx(rv$ss, input$coder_id)
    remaining <- setdiff(rv$data$index_order, completed_idx)
    if (length(remaining) == 0) return(NA_integer_)
    target <- min(remaining)
    which(rv$data$index_order == target)[1]
  }
  
  # reset all question inputs to blank/empty
  reset_question_inputs <- function() {
    lapply(question_spec, function(q){
      switch(q$type,
             radio    = updateRadioButtons(session, q$id, selected = character(0)),
             text     = updateTextInput(session, q$id, value = ""),
             textArea = updateTextAreaInput(session, q$id, value = ""),
             checkbox = updateCheckboxGroupInput(session, q$id, selected = character(0)),
             select   = updateSelectInput(session, q$id, selected = ""),
             NULL
      )
    })
    invisible(TRUE)
  }
  
  # For UI text bits
  output$position <- renderText({
    req(rv$ready)
    paste0("Item ", rv$idx_pos, " of ", nrow(rv$data))
  })
  
  output$current_info <- renderText({
    req(rv$ready)
    r <- cur_row()
    paste0("(index ", r$index_order, " • video ", r$video_id, ")")
  })
  
  # Embed the video
  output$embed <- renderUI({
    req(rv$ready)
    vid <- cur_row()$video_id
    validate(need(!is.na(vid), "This row doesn't have a parsable video ID (expected .../video/VIDEOID)."))
    make_tiktok_iframe(vid)
  })
  
  # Questionnaire UI
  output$questionnaire <- renderUI({
    req(rv$ready)
    r <- cur_row()
    tagList(
      div(style = "margin-bottom:.5rem; font-size:.9rem; word-break:break-all;",
          strong("Original: "),
          tags$a(href = r$canonical_url, target = "_blank", r$canonical_url)
      ),
      lapply(question_spec, function(q){
        switch(q$type,
               radio    = radioButtons(q$id, q$label, choices = q$choices, inline = TRUE,
                                       selected = character(0)),  # start blank
               text     = textInput(q$id, q$label),
               textArea = textAreaInput(q$id, q$label, rows = 4),
               checkbox = checkboxGroupInput(q$id, q$label, choices = q$choices),
               select   = selectInput(q$id, q$label, choices = q$choices),
               div(class = "text-danger", sprintf("Unknown question type: %s", q$type))
        )
      })
    )
  })
  
  # Javascript to allow unchecking buttons in the questionnaire
  runjs("
    $(document).on('click', '.shiny-input-radiogroup input[type=radio]', function(){
      var $this = $(this);
      if ($this.data('waschecked')) {
        $this.prop('checked', false).data('waschecked', false).trigger('change');
      } else {
        $('input[name=\"' + $this.attr('name') + '\"]').data('waschecked', false);
        $this.data('waschecked', true);
      }
    });
  ")
  
  # Collect answers 
  gather_responses <- function(){
    ids <- vapply(question_spec, function(q) q$id, character(1))
    vals <- lapply(question_spec, function(q){
      v <- input[[q$id]]
      if (is.null(v)) "" else if (is.character(v)) paste(v, collapse = ", ") else as.character(v)
    })
    out <- tibble::as_tibble_row(setNames(vals, ids))
    out[, ids, drop = FALSE]
  }
  
  # Move to next item (updates cursor + resets inputs)
  advance_to_next <- function(){
    req(rv$ready)
    pos <- next_position()
    if (is.na(pos)) {
      clear_cursor(rv$ss, input$coder_id)
      showNotification("Saved. All done! 🎉", type = "message")
      return(invisible(FALSE))
    }
    rv$idx_pos <- pos
    r <- cur_row()
    set_cursor(rv$ss, input$coder_id, r$index_order, r$video_id)
    reset_question_inputs()
    invisible(TRUE)
  }
  
  
  # Save full response and advance
  save_response <- function(){
    req(rv$ready)
    r <- cur_row()
    
    meta <- tibble::tibble(
      timestamp      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      coder_id       = input$coder_id %||% "",
      index_order    = as.character(r$index_order),  # keep consistent type with flag writes
      URL            = r$URL,
      user_id        = r$user_id,
      video_id       = r$video_id,
      relevant_video = NA_character_,                # left blank when saving full survey
      link_active    = NA_character_
    )
    
    payload <- dplyr::bind_cols(meta, gather_responses())
    
    tryCatch({
      googlesheets4::sheet_append(ss = rv$ss, data = payload, sheet = RESPONSES_TAB)
      advance_to_next()
      TRUE
    }, error = function(e){
      showNotification(paste("Write error:", e$message), type = "error", duration = 8)
      FALSE
    })
  }
  
  
  observeEvent(input$save_and_next, {
    if (isTRUE(save_response())) {
      showNotification("Saved.", type = "message", duration = 1.5)
    }
  }, ignoreInit = TRUE)
  
  # Quick flag rows (irrelevant / link broken) and advance; answer columns remain blank
  save_flag_and_advance <- function(flag_col, flag_value){
    req(rv$ready)
    r <- cur_row()
    
    payload <- tibble::tibble(
      timestamp      = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      coder_id       = input$coder_id %||% "",
      index_order    = as.character(r$index_order),
      URL            = r$URL,
      user_id        = r$user_id,
      video_id       = r$video_id,
      relevant_video = if (identical(flag_col, "relevant_video")) flag_value else NA_character_,
      link_active    = if (identical(flag_col, "link_active"))    flag_value else NA_character_
    )
    
    tryCatch({
      googlesheets4::sheet_append(ss = rv$ss, data = payload, sheet = RESPONSES_TAB)
      advance_to_next()
      TRUE
    }, error = function(e){
      showNotification(paste("Write error:", e$message), type = "error", duration = 8)
      FALSE
    })
  }
  
  observeEvent(input$mark_irrelevant, {
    if (isTRUE(save_flag_and_advance("relevant_video", "No"))) {
      showNotification("Marked irrelevant and advanced.", type = "message")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$mark_link_broken, {
    if (isTRUE(save_flag_and_advance("link_active", "No"))) {
      showNotification("Marked link broken and advanced.", type = "message")
    }
  }, ignoreInit = TRUE)
  
  # Clear cursor on disconnect
  session$onSessionEnded(function() {
    if (!isTruthy(input$coder_id)) return(invisible())
    try(clear_cursor(rv$ss, input$coder_id), silent = TRUE)
  })
}

# Add this line when adding new questions?
##migrate_responses_header(SHEET_URL)

shinyApp(ui, server)


