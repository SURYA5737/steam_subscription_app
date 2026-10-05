# ============================================================
#  STEAM GAMING PLATFORM - SUBSCRIPTION ANALYTICS ENGINE
#  Run in RStudio: open app.R and click "Run App"
#  Required packages: shiny, ggplot2, dplyr
#    install.packages(c("shiny", "ggplot2", "dplyr"))
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)

# ---------- Configuration ----------
CSV_FILE <- "steam_subscriptions.csv"

TIERS <- c("Base Standard", "Pro Access", "Premium Vault",
           "Elite Developer", "Ultimate VIP")

REQUIRED_COLS <- c("Gamer_ID", "Subscription_Tier",
                   "Monthly_Price_INR", "Server_Region")

TIER_COLORS <- c(
  "Base Standard"   = "#00e5ff",
  "Pro Access"      = "#39ff14",
  "Premium Vault"   = "#ffd600",
  "Elite Developer" = "#ff6d00",
  "Ultimate VIP"    = "#d500f9"
)

# ---------- Safe data loading with fallback ----------
fallback_data <- function() {
  data.frame(
    Gamer_ID          = c("FB001", "FB002", "FB003", "FB004", "FB005"),
    Subscription_Tier = TIERS,
    Monthly_Price_INR = c(199, 399, 699, 1199, 1999),
    Server_Region     = c("Mumbai", "Delhi", "Chennai", "Bengaluru", "Singapore"),
    stringsAsFactors  = FALSE
  )
}

load_subscription_data <- function(path) {
  result <- list(data = NULL, source = "csv", message = NULL)
  
  if (file.exists(path)) {
    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(df)) {
      result$message <- paste0("Could not read '", path, "'. Showing built-in demo data.")
    } else if (!all(REQUIRED_COLS %in% names(df))) {
      result$message <- paste0("'", path, "' is missing required columns (",
                               paste(setdiff(REQUIRED_COLS, names(df)), collapse = ", "),
                               "). Showing built-in demo data.")
    } else {
      df <- df[, REQUIRED_COLS]
      df$Monthly_Price_INR <- suppressWarnings(as.numeric(df$Monthly_Price_INR))
      df <- df[!is.na(df$Monthly_Price_INR) & df$Subscription_Tier %in% TIERS, ]
      if (nrow(df) > 0) {
        result$data <- df
        return(result)
      }
      result$message <- paste0("'", path, "' has no valid rows. Showing built-in demo data.")
    }
  } else {
    result$message <- paste0("File '", path, "' was not found. Showing built-in demo data.")
  }
  
  result$data   <- fallback_data()
  result$source <- "fallback"
  result
}

loaded <- load_subscription_data(CSV_FILE)

# ---------- Gaming dark theme CSS ----------
gaming_css <- "
body { background-color: #0b0e17; color: #e0e6f0; font-family: 'Segoe UI', Roboto, sans-serif; }
h2.title { color: #39ff14; text-shadow: 0 0 12px rgba(57,255,20,.6);
           letter-spacing: 2px; text-transform: uppercase; margin-bottom: 4px; }
.subtitle { color: #7a869a; margin-bottom: 20px; }
.panel-dark { background: #131826; border: 1px solid #1f2a44; border-radius: 10px;
              padding: 18px; margin-bottom: 18px; box-shadow: 0 0 18px rgba(0,229,255,.06); }
label { color: #00e5ff !important; font-weight: 600; letter-spacing: .5px; }
.form-control { background-color: #0b0e17; color: #e0e6f0; border: 1px solid #00e5ff; border-radius: 6px; }
.form-control:focus { border-color: #39ff14; box-shadow: 0 0 10px rgba(57,255,20,.5);
                      background-color: #0b0e17; color: #fff; }
.best-card { background: linear-gradient(135deg, #1a2238, #0f1424); border: 2px solid #39ff14;
             border-radius: 12px; padding: 22px; text-align: center;
             box-shadow: 0 0 25px rgba(57,255,20,.35); }
.best-card .badge-label { color: #39ff14; font-size: 13px; letter-spacing: 3px; text-transform: uppercase; }
.best-card .best-name { font-size: 34px; font-weight: 800; margin: 8px 0; text-shadow: 0 0 14px currentColor; }
.best-card .best-money { font-size: 22px; color: #ffd600; font-weight: 700; }
.best-card .best-share { color: #9fb0c8; margin-top: 4px; }
.metric-card { background: #131826; border: 1px solid #1f2a44; border-radius: 10px;
               padding: 14px; text-align: center; margin-bottom: 14px; }
.metric-card .m-label { color: #7a869a; font-size: 12px; text-transform: uppercase; letter-spacing: 1.5px; }
.metric-card .m-value { color: #00e5ff; font-size: 24px; font-weight: 700; }
.alert-warn { background: #3a2a00; border: 1px solid #ffd600; color: #ffd600;
              border-radius: 8px; padding: 10px 14px; margin-bottom: 16px; }
table { color: #e0e6f0 !important; width: 100%; }
table th { color: #39ff14; border-bottom: 1px solid #1f2a44 !important; }
table td { border-top: 1px solid #1a2238 !important; padding: 6px 8px; }
.empty-note { color: #7a869a; font-style: italic; text-align: center; padding: 30px; }
"

# ---------- UI ----------
ui <- fluidPage(
  tags$head(tags$style(HTML(gaming_css))),
  
  div(h2("Steam Subscription Analytics Engine", class = "title"),
      div("Live revenue intelligence across all membership tiers", class = "subtitle")),
  
  uiOutput("data_warning"),
  
  fluidRow(
    column(4,
           div(class = "panel-dark",
               textInput("search", "Enter Gamer, Tier, or Region Query",
                         value = "", placeholder = "e.g. G1005, Ultimate VIP, Mumbai"),
               helpText(style = "color:#7a869a;", "Results update as you type.")
           ),
           uiOutput("best_tier_card"),
           br(),
           uiOutput("summary_metrics")
    ),
    
    column(8,
           div(class = "panel-dark",
               h4("Revenue Share by Tier (%)", style = "color:#00e5ff;"),
               plotOutput("pct_plot", height = "380px")
           ),
           div(class = "panel-dark",
               h4("Matching Records", style = "color:#00e5ff;"),
               tableOutput("records_table")
           )
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  base_data <- reactive({ loaded$data })
  
  # Real-time smart search across Gamer_ID, Tier and Region
  filtered <- reactive({
    df <- base_data()
    q  <- trimws(input$search %||% "")
    if (nchar(q) == 0) return(df)
    
    hit <- grepl(q, df$Gamer_ID, ignore.case = TRUE, fixed = TRUE) |
      grepl(q, df$Subscription_Tier, ignore.case = TRUE, fixed = TRUE) |
      grepl(q, df$Server_Region, ignore.case = TRUE, fixed = TRUE)
    df[hit, , drop = FALSE]
  })
  
  # Revenue and percentage distribution for tiers active on screen
  tier_summary <- reactive({
    df <- filtered()
    if (nrow(df) == 0) return(NULL)
    
    out <- df %>%
      group_by(Subscription_Tier) %>%
      summarise(Revenue = sum(Monthly_Price_INR), Members = n(), .groups = "drop")
    
    out$Percent <- out$Revenue / sum(out$Revenue) * 100
    out$Subscription_Tier <- factor(out$Subscription_Tier, levels = TIERS)
    out[order(out$Subscription_Tier), ]
  })
  
  output$data_warning <- renderUI({
    if (!is.null(loaded$message)) div(class = "alert-warn", loaded$message)
  })
  
  # Best tier indicator (highest total money on active rows)
  output$best_tier_card <- renderUI({
    s <- tier_summary()
    if (is.null(s)) {
      return(div(class = "best-card",
                 div("Best Performing Tier", class = "badge-label"),
                 div("No data", class = "best-name", style = "color:#7a869a;"),
                 div("No records match your query.", class = "best-share")))
    }
    
    top      <- s[s$Revenue == max(s$Revenue), ]
    names_ch <- as.character(top$Subscription_Tier)
    color    <- TIER_COLORS[[names_ch[1]]]
    
    div(class = "best-card",
        div("\u2605 Best Performing Tier \u2605", class = "badge-label"),
        div(paste(names_ch, collapse = " & "), class = "best-name", style = paste0("color:", color, ";")),
        div(paste0("\u20B9 ", format(top$Revenue[1], big.mark = ","), " total revenue"), class = "best-money"),
        div(sprintf("%.1f%% of visible platform revenue \u2022 %d members",
                    top$Percent[1], top$Members[1]), class = "best-share"),
        if (length(names_ch) > 1) div("(tie on revenue)", class = "best-share")
    )
  })
  
  output$summary_metrics <- renderUI({
    df <- filtered()
    fluidRow(
      column(6, div(class = "metric-card",
                    div("Gamers", class = "m-label"),
                    div(format(nrow(df), big.mark = ","), class = "m-value"))),
      column(6, div(class = "metric-card",
                    div("Total Revenue", class = "m-label"),
                    div(paste0("\u20B9", format(sum(df$Monthly_Price_INR), big.mark = ",")),
                        class = "m-value")))
    )
  })
  
  # Percentage bar chart
  output$pct_plot <- renderPlot({
    s <- tier_summary()
    validate(need(!is.null(s), "No records match your query."))
    
    ggplot(s, aes(x = Subscription_Tier, y = Percent, fill = Subscription_Tier)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.1f%%", Percent)),
                vjust = -0.6, color = "#e0e6f0", fontface = "bold", size = 5) +
      scale_fill_manual(values = TIER_COLORS) +
      scale_y_continuous(limits = c(0, max(s$Percent) * 1.2),
                         labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "Share of Total Revenue") +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "#131826", color = NA),
        panel.background = element_rect(fill = "#131826", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#1f2a44"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "#9fb0c8"),
        axis.title = element_text(color = "#9fb0c8")
      )
  }, bg = "#131826")
  
  output$records_table <- renderTable({
    df <- filtered()
    if (nrow(df) == 0) return(data.frame(Message = "No matching records."))
    df$Monthly_Price_INR <- format(df$Monthly_Price_INR, big.mark = ",")
    df
  }, striped = FALSE, hover = TRUE, spacing = "s")
}

# Null-coalescing helper (for older R versions)
`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)