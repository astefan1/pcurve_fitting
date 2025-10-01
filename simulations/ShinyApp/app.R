library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)

# depending on whether the app is run from RStudio or from the server,
# the working directory may be different, so we try two locations
try({
  load(file="simulations/sim-results/simres_all.RData")
}, silent=TRUE)
try({
  load(file="simres_all.RData")
}, silent=TRUE)

# --------------------------------------------------------------------
# Fallback: create a small mock `simres` if it doesn't exist
# --------------------------------------------------------------------
if (!exists("simres", inherits = FALSE)) {
  stop("No simres object!")
}

# --------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------
choices_num_chr <- function(x) as.character(sort(unique(na.omit(x))))
choices_with_any <- function(x) {
  ch <- sort(unique(x))
  c("Any", as.character(ch))
}
choices_without_any <- function(x) {
  sort(unique(x))
}
mid_choice_chr <- function(x_chr) {
  if (length(x_chr) == 0) return(NA_character_)
  x_chr[ceiling(length(x_chr) / 2)]
}

# Dummy plotting function (replace with your own)
plot_fun <- function(df) {
  p_vars <- paste0("p", 1:5)
  have <- all(p_vars %in% names(df))
  if (!have) {
    return(ggplot() + theme_minimal() +
             labs(title = "p1..p5 not found in data") +
             theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank()))
  }
  if (nrow(df) == 0) {
    return(ggplot() + theme_minimal() +
             labs(title = "No rows match the current filters") +
             theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank()))
  }
  if (nrow(df) > 1) {
    return(ggplot() + theme_minimal() +
             labs(title = "More than 1 row matches the current filters.") +
             theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank()))
  }

  df_sel <- df %>%
    summarise(across(all_of(p_vars), ~mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value")
  df_sel$p_bin <- str_sub(df_sel$metric, 2, 2)

  df_sel %>%
    ggplot(aes(x = p_bin, y = value, group=1)) +
    geom_line() +
    geom_point() +
    #geom_col() +
    theme_minimal() +
    labs(x = NULL, y = "% of p-values", title = "p-curve") +
    ylim(0, 1)
}

# Precompute discrete choices (as characters) for sliders 1–6
ch_nvar <- choices_num_chr(simres$nvar)
ch_r <- choices_num_chr(simres$r)
ch_d <- choices_num_chr(simres$d)
ch_propHacker <- choices_num_chr(simres$prop_Hacker)
ch_propH1 <- choices_num_chr(simres$prop_H1)
ch_het <- choices_num_chr(simres$het)

# --------------------------------------------------------------------
# UI
# --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Simulation Explorer for hacked p-curves"),
  sidebarLayout(
    sidebarPanel(
      # The controlling select — its value determines which sliders are visible
      selectInput("type", "p-hacking type",
                  choices = choices_without_any(simres$type),
                  selected = "multDV"),

      sliderTextInput("d", "Effect size d under H1",
                      choices = ch_d,
                      selected = mid_choice_chr(ch_d),
                      grid = TRUE),
        sliderTextInput("prop_Hacker", "Proportion of p-hackers",
                      choices = ch_propHacker,
                      selected = mid_choice_chr(ch_propHacker),
                      grid = TRUE),
        sliderTextInput("prop_H1", "Probability of H1",
                      choices = ch_propH1,
                      selected = mid_choice_chr(ch_propH1),
                      grid = TRUE),
        sliderTextInput("het", "Heterogeneity under H1 effect sizes",
                      choices = ch_het,
                      selected = mid_choice_chr(ch_het),
                      grid = TRUE),

      conditionalPanel(
        condition = "input.type == 'multDV'",
        sliderTextInput("nvar", "Number of DVs to choose from",
                      choices = ch_nvar,
                      selected = mid_choice_chr(ch_nvar),
                      grid = TRUE),
        sliderTextInput("r", "Correlation r between DVs",
                      choices = ch_r,
                      selected = mid_choice_chr(ch_r),
                      grid = TRUE),
        selectInput("strategy", "Selection strategy",
                  choices = choices_without_any(simres$strategy), selected = "1")
      ),

      conditionalPanel(
        condition = "input.type == 'optStop'",
        sliderTextInput("nmin", "n_min",
                      choices = choices_without_any(simres$nmin),
                      selected = min(simres$nmin, na.rm=TRUE),
                      grid = TRUE),
        sliderTextInput("nmax", "n_max",
                      choices = choices_without_any(simres$nmax),
                      selected = max(simres$nmax, na.rm=TRUE),
                      grid = TRUE),
        sliderTextInput("stepsize", "stepsize",
                      choices = choices_without_any(simres$stepsize),
                      selected = min(simres$stepsize, na.rm=TRUE),
                      grid = TRUE),
      ),
        
      tags$hr(),
      actionButton("reset", "Reset filters")
    ),
    mainPanel(
      plotOutput("plot", height = "420px"),
      tags$br(),
      verbatimTextOutput("nrows")
    )
  )
)

# --------------------------------------------------------------------
# Server
# --------------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateSliderTextInput(session, "nvar", selected = mid_choice_chr(ch_nvar))
    updateSliderTextInput(session, "r", selected = mid_choice_chr(ch_r))
    updateSliderTextInput(session, "d", selected = mid_choice_chr(ch_d))
    updateSliderTextInput(session, "prop_Hacker", selected = mid_choice_chr(ch_propHacker))
    updateSliderTextInput(session, "prop_H1", selected = mid_choice_chr(ch_propH1))
    updateSliderTextInput(session, "het", selected = mid_choice_chr(ch_het))

    updateSelectInput(session, "strategy", selected = "Any")
    updateSelectInput(session, "condition", selected = "Any")
    updateSelectInput(session, "type", selected = "Any")
    updateSelectInput(session, "nmin", selected = "Any")
    updateSelectInput(session, "nmax", selected = "Any")
    updateSelectInput(session, "stepsize", selected = "Any")
  })

  # Filtered data according to UI
  filtered <- reactive({
    req(input$nvar, input$r, input$d, input$prop_Hacker, input$prop_H1, input$het)

    # Convert sliderTextInput selections to numeric for filtering
    sel_nvar <- as.integer(input$nvar)
    sel_r <- as.numeric(input$r)
    sel_d <- as.numeric(input$d)
    sel_propHacker <- as.numeric(input$prop_Hacker)
    sel_propH1 <- as.numeric(input$prop_H1)
    sel_het <- as.numeric(input$het)

    if (input$type == "multDV") {
      df <- simres %>%
        filter(
          type == "multDV",
          nvar == sel_nvar,
          dplyr::near(r, sel_r, tol = 1e-12),
          dplyr::near(d, sel_d, tol = 1e-12),
          dplyr::near(prop_Hacker, sel_propHacker, tol = 1e-12),
          dplyr::near(prop_H1, sel_propH1, tol = 1e-12),
          dplyr::near(het, sel_het, tol = 1e-12),
          strategy == as.integer(input$strategy)
        )
    }

    if (input$type == "optStop") {
      df <- simres %>%
        filter(
          type == "optStop",          
          dplyr::near(d, sel_d, tol = 1e-12),
          dplyr::near(prop_Hacker, sel_propHacker, tol = 1e-12),
          dplyr::near(prop_H1, sel_propH1, tol = 1e-12),
          dplyr::near(het, sel_het, tol = 1e-12),
          nmin == as.integer(input$nmin),
          nmax == as.integer(input$nmax),
          stepsize == as.integer(input$stepsize)
        )
    }

    df
  })

  output$plot <- renderPlot({
    plot_fun(filtered())
  })

  output$nrows <- renderText({
    paste0("Rows after filtering: ", nrow(filtered()))
  })
}

# --------------------------------------------------------------------
shinyApp(ui, server)