library(shiny)
library(dplyr)
library(tidyr)
library(munsell)
library(S7)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(rio)
library(shinyjs)

# The two empirical reference p-curves
pcurves <- import("avg_pcurve.csv")
pcurves_long <- pcurves %>%
  pivot_longer(cols = starts_with("p"), names_to = "metric", values_to = "value") %>%
  mutate(p_bin = str_sub(metric, 2, 2))

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


#' @param reference A data frame with columns `p_bin` and `value` for the reference p-curve (optional)
plot_fun <- function(df, reference=NA) {
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

  p1 <- df_sel %>%
    ggplot(aes(x = p_bin, y = value, group=1)) +
    geom_line(color="blue", linewidth=1) +
    scale_x_discrete(labels = c("1" = ".01", "2" = ".02", "3" = ".03", "4" = ".04", "5" = ".05")) +
    theme_minimal() +
    labs(x = "p-value bin", y = "% of p-values", title = "p-curve") +
    ylim(0, 1)

  if (!all(is.na(reference))) {
    p1 <- p1 + geom_line(data = reference, aes(x = p_bin, y = value), color = "black", linetype = "solid", linewidth = 1)
  }

  p1
}

# Precompute discrete choices (as characters) for sliders 1–6
ch_nvar <- choices_num_chr(simres$nvar)
ch_r <- choices_num_chr(simres$r)
ch_d <- choices_num_chr(simres$d)
ch_propHacker <- choices_num_chr(simres$prop_Hacker)
ch_propH1 <- choices_num_chr(simres$prop_H1)
ch_het <- choices_num_chr(simres$het)
reference_choices <- choices_without_any(pcurves_long$dataset)

# --------------------------------------------------------------------
# UI
# --------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
  Shiny.addCustomMessageHandler('updateSliderText', function(msg) {
    var slider = $('#' + msg.id).data('ionRangeSlider');
    slider.update({from: msg.index});
  });
")),
  titlePanel("Simulation Explorer for hacked p-curves"),
  sidebarLayout(
    sidebarPanel(

      selectInput("reference_line", "Reference p-curve",
                  choices = c("None", reference_choices),
                  selected = "None"),
      
      # The controlling select — its value determines which sliders are visible
      selectInput("type", "p-hacking type",
                  choices = c("Multiple DVs" = "multDV", "Optional Stopping" = "optStop",
                              "No p-hacking, true effects" = "multDV_perfect",
                              "No p-hacking, null effects" = "multDV_H0"),
                  selected = "multDV"),

      conditionalPanel(
        condition = "input.type == 'multDV' || input.type == 'optStop'",
        sliderTextInput("d", "Effect size d under H1",
                        choices = ch_d,
                        selected = mid_choice_chr(ch_d),
                        grid = TRUE),
        sliderTextInput("prop_Hacker", "Proportion of p-hackers",
                        choices = ch_propHacker[ch_propHacker != "0"],
                        selected = mid_choice_chr(ch_propHacker[ch_propHacker != "0"]),
                        grid = TRUE),
        sliderTextInput("prop_H1", "Probability of H1",
                        choices = ch_propH1,
                        selected = mid_choice_chr(ch_propH1),
                        grid = TRUE),
        sliderTextInput("het", "Heterogeneity under H1 effect sizes",
                        choices = ch_het,
                        selected = mid_choice_chr(ch_het),
                        grid = TRUE)
      ),

      conditionalPanel(
        condition = "input.type == 'multDV'",
        sliderTextInput("nvar", "Number of DVs to choose from",
                        choices = ch_nvar[ch_nvar != "1"],
                        selected = mid_choice_chr(ch_nvar[ch_nvar != "1"]),
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
      
      conditionalPanel(
        condition = "input.type == 'multDV_perfect'",
        sliderTextInput("d_perfect", "Effect size d under H1",
                        choices = ch_d[ch_d != "0"],
                        selected = mid_choice_chr(ch_d[ch_d != "0"]),
                        grid = TRUE),
        sliderTextInput("het_perfect", "Heterogeneity under H1 effect sizes",
                        choices = ch_het,
                        selected = mid_choice_chr(ch_het),
                        grid = TRUE),
        sliderTextInput("prop_H1_perfect", "Probability of H1",
                        choices = ch_propH1[ch_propH1 != "0"],
                        selected = mid_choice_chr(ch_propH1[ch_propH1 != "0"]),
                        grid = TRUE),
        disabled(sliderTextInput("prop_Hacker_perfect", "Proportion of p-hackers (fixed at 0)",
                                 choices = "0",
                                 selected = "0",
                                 grid = TRUE))
      ),
      
      conditionalPanel(
        condition = "input.type == 'multDV_H0'",
        disabled(sliderTextInput("d_H0", "Effect size d (fixed at 0)",
                                 choices = "0", selected = "0", grid = TRUE)),
        disabled(sliderTextInput("het_H0", "Heterogeneity (fixed at 0)",
                                 choices = "0", selected = "0", grid = TRUE)),
        disabled(sliderTextInput("prop_H1_H0", "Probability of H1 (fixed at 0)",
                                 choices = "0", selected = "0", grid = TRUE)),
        disabled(sliderTextInput("prop_Hacker_H0", "Proportion of p-hackers (fixed at 0)",
                                 choices = "0", selected = "0", grid = TRUE))
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
    updateSelectInput(session, "reference_line", selected = "None")
  })
  
  # When prop_H1 is set to 0, force d and het to 0
  trigger_propH1 <- reactiveVal(0)
  trigger_d <- reactiveVal(0)
  ignore_next_propH1 <- reactiveVal(FALSE)
  ignore_next_d <- reactiveVal(FALSE)
  
  observeEvent(trigger_propH1(), {
    if (trigger_propH1() > 0)
      session$sendCustomMessage("updateSliderText", list(
        id = "prop_H1",
        index = which(ch_propH1 == "0.1") - 1
      ))
  }, ignoreInit = TRUE)
  
  observeEvent(trigger_d(), {
    if (trigger_d() > 0)
      session$sendCustomMessage("updateSliderText", list(
        id = "d",
        index = which(ch_d == "0.1") - 1
      ))
  }, ignoreInit = TRUE)
  
  observeEvent(input$prop_H1, {
    if (ignore_next_propH1()) { ignore_next_propH1(FALSE); return() }
    if (input$prop_H1 == "0") {
      ignore_next_d(TRUE)
      updateSliderTextInput(session, "d", selected = "0")
      updateSliderTextInput(session, "het", selected = "0")
    } else if (input$d == "0") {
      ignore_next_d(TRUE)
      trigger_d(trigger_d() + 1)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$d, {
    if (ignore_next_d()) { ignore_next_d(FALSE); return() }
    if (input$d == "0") {
      ignore_next_propH1(TRUE)
      updateSliderTextInput(session, "prop_H1", selected = "0")
      updateSliderTextInput(session, "het", selected = "0")
    } else if (input$prop_H1 == "0") {
      ignore_next_propH1(TRUE)
      trigger_propH1(trigger_propH1() + 1)
    }
  }, ignoreInit = TRUE)

  # Filtered data according to UI
  filtered <- reactive({
    req(input$type)

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
    
    if (input$type == "multDV_perfect") {
      req(input$d_perfect, input$prop_H1_perfect, input$het_perfect)
      df <- simres %>%
        filter(
          condition == "multDV_perfect",
          dplyr::near(d, as.numeric(input$d_perfect), tol = 1e-12),
          dplyr::near(prop_H1, as.numeric(input$prop_H1_perfect), tol = 1e-12),
          dplyr::near(het, as.numeric(input$het_perfect), tol = 1e-12),
          strategy == as.integer(input$strategy)
        )
    }
    
    if (input$type == "multDV_H0") {
      df <- simres %>%
        filter(condition == "multDV_H0") %>%
        slice(1)
      return(df)
    }
    
    df
  })


  reference_line_data <- reactive({
    req(input$reference_line)

    if (identical(input$reference_line, "None")) {
      return(NA)
    }

    pcurves_long %>%
      filter(dataset == input$reference_line) %>%
      select(p_bin, value)
  })

  output$plot <- renderPlot({
    plot_fun(filtered(), reference_line_data())
  })

  output$nrows <- renderText({
    paste0("Rows after filtering: ", nrow(filtered()))
  })
}

# --------------------------------------------------------------------
shinyApp(ui, server)