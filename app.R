library(shiny)
library(readxl)
library(nortest)
library(qcc)
library(DT)
library(dplyr)
library(tidyr)

# Function to Validate and Obtain Numeric Variables:
get_numeric_vars <- function(df, selected_vars) {
  validate(need(!is.null(selected_vars), "No variables selected."))
  validate(need(all(selected_vars %in% names(df)), "Selected variables do not exist in the dataset."))
  
  num_vars <- selected_vars[sapply(df[selected_vars], is.numeric)]
  validate(need(length(num_vars) > 0, "Please select at least one numeric variable."))
  return(num_vars)
}

options(shiny.maxRequestSize = 200*1024^2)

# Define UI
ui <- fluidPage(
  titlePanel(
    windowTitle = "QualyTools",
    title = div(img(src = "logo.png", height = "70px"), "Quality Control Tools")
  ),
  
  # Layout principal
  sidebarLayout(
    sidebarPanel(
      # Primeira seção: Carregar Dados
      tags$div(style = "border: 2px solid #007BFF; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
               fileInput("file", "Upload file", accept = c(".csv", ".xlsx")),
               radioButtons("separator", "Separator (for CSV):",
                            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ",")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Histogram and Normality'",
        selectizeInput("variables", "Select variables for histogram:", choices = NULL, multiple = FALSE),
        sliderInput("Classes", "Number of classes:", min = 1, max = 50, value = 12),
        numericInput("lic", "Lower Specification Limit (LSL):", value = NULL),
        numericInput("lsc", "Specification Limit (SL):", value = NULL),
        numericInput("ls", "Upper Specification Limit (USL):", value = NULL),
        checkboxInput("include_normal", "Include normality curve", value = TRUE),
        checkboxInput("include_tolerance", "Include specification limits", value = TRUE)
      ),
      actionButton("add_row", "Add Row"),
      actionButton("add_col", "Add Column"),
      actionButton("delete_row", "Delete Selected Row"),
      selectInput("column_to_delete", "Select Column to Delete", choices = NULL),  
      actionButton("delete_col", "Delete Selected Column"),
      br(), br(),
      selectInput("column_to_rename", "Select Column to Rename", choices = NULL),
      textInput("new_col_name", "New Column Name"),
      actionButton("rename_col", "Rename Column"),
      downloadButton("download_data", "Download Updated Data")
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs", selected ="data",
                  tabPanel("Data",
                           conditionalPanel(condition = "output.isFileUploaded",
                                            DTOutput("file_contents")  
                           ),
                           conditionalPanel(
                             condition = "output.isFileUploaded == false",  # Show editable table if no file uploaded
                             DTOutput("editable_table")  # Display the editable table
                           )
                  ),
                  tabPanel("Histogram and Normality",
                           verbatimTextOutput("concat_summary"),
                           plotOutput("histPlot"),
                           plotOutput("qqPlot"),
                           verbatimTextOutput("lillieforsTest"),
                           verbatimTextOutput("shapirotest")
                  ),
                  tabPanel("Scatter",
                           selectizeInput("scatter_x", "Select variable for X axis:", choices = NULL),
                           selectizeInput("scatter_y", "Select variable for Y axis:", choices = NULL),
                           plotOutput("scatterPlot"),
                           verbatimTextOutput("spearmantest")
                  ),
                  tabPanel("Pareto Chart",
                           selectizeInput("pareto_vars", "Select variable for Pareto chart:", choices = NULL),
                           plotOutput("paretoPlot")
                  ),
                  tabPanel("Control Charts & Summary",
                           fluidRow(
                             column(12,
                                    h4("Summary Table"),
                                    uiOutput("var_select_ui"),
                                    uiOutput("obs_select_ui"), DTOutput("summary_table"),
                                    br(),
                                    plotOutput("controlChart")
                             )
                           ),
                           
                           hr(),
                           br(),
                           
                           fluidRow(
                             column(6, plotOutput("xbar_chart")),
                             column(6, plotOutput("control_chart"))
                           )
                  ),
                  
                  tabPanel("Capability Analysis",  
                           selectizeInput("capability_variable", "Select variable for capability analysis:", choices = NULL),
                           numericInput("lie", "Lower Specification Limit (LSL):", value = NULL),
                           numericInput("target", "Target Value:", value = NA, step = 0.01),
                           numericInput("les", "Upper Specification Limit (USL):", value = NULL),
                           numericInput("subgroup_size_capability", "Subgroup Size:", value = 5, min = 2, step = 1),
                           verbatimTextOutput("capability_summary")  
                  ),
                  
                  tabPanel("License",
                           h3("Creative Commons Attribution-NonCommercial 4.0 International License"),
                           p("This app is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License."),
                           
                           tags$ul(
                             tags$li("Attribution: You must give appropriate credit, provide a link to the license, and indicate if changes were made."),
                             tags$li("NonCommercial: You may not use the material for commercial purposes."),
                             tags$li("No Additional Restrictions: You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.")
                           ),
                           
                           tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode",
                                  "Read the Full License (Click Here)",
                                  target = "_blank"),
                           
                           tags$code("https://creativecommons.org/licenses/by-nc/4.0/legalcode")
                  ),
      )
    )
  )
)  

# Define server logic
server <- function(input, output, session) {
  file_data <- reactiveVal(NULL)
  
  observe({
    df <- get_data()  
    updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_x", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_y", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "control_variable", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "capability_variable", choices = names(df), server = TRUE)
  })
  
  # Base data
  file_data <- reactiveVal(NULL) 
  initial_data <- reactiveVal(data.frame(
    Month = c("Jan", "Feb", "Mar", "Apr", "May"),
    Items_Produced = c(5000, 5500, 5200, 5100, 5400),
    Defective_Items = c(120, 130, 110, 100, 125),
    Defect_Type = c("Scratches", "Cracks", "Incorrect Size", "Scratches", "Cracks"),
    Scratches = c(40, 20, 30, 40, 20),
    Cracks = c(10, 30, 20, 10, 40),
    Incorrect_Size = c(70, 80, 60, 50, 65)
  ))
  
  # Update column select Input choices
  observe({
    df <- if (is.null(file_data())) {
      initial_data()  
    } else {
      file_data()
    }
    
    updateSelectInput(session, "column_to_rename", choices = names(df))
    updateSelectInput(session, "column_to_delete", choices = names(df))
  })
  
  # Add row
  observeEvent(input$add_row, {
    df <- initial_data()
    new_row <- as.list(rep(NA, ncol(df)))
    names(new_row) <- names(df)
    df <- rbind(df, new_row)
    initial_data(df)
  })
  
  # Add column
  observeEvent(input$add_col, {
    df <- initial_data()
    base_name <- "NewColumn"
    existing_names <- names(df)
    i <- 1
    new_col_name <- paste0(base_name, i)
    while (new_col_name %in% existing_names) {
      i <- i + 1
      new_col_name <- paste0(base_name, i)
    }
    df[[new_col_name]] <- as.numeric(NA)  # Explicitly define as numeric
    initial_data(df)
  })
  
  # Delete selected row
  observeEvent(input$delete_row, {
    req(input$editable_table_rows_selected)
    df <- initial_data()
    row_to_delete <- input$editable_table_rows_selected
    df <- df[-row_to_delete, , drop = FALSE]
    initial_data(df)
  })
  
  # Delete selected column
  observeEvent(input$delete_col, {
    req(input$column_to_delete)
    df <- initial_data()
    col_to_delete <- input$column_to_delete
    df <- df[,!(names(df) %in% col_to_delete), drop = FALSE]
    initial_data(df)
  })
  
  # Rename column
  observeEvent(input$rename_col, {
    req(input$column_to_rename, input$new_col_name)
    df <- initial_data()
    names(df)[names(df) == input$column_to_rename] <- input$new_col_name
    initial_data(df)
  })
  
  # Logic to handle file upload
  observe({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    print(paste("File extension:", file_ext))  # Debug file extension
    print(paste("Separator:", input$separator))  # Debug separator
    tryCatch({
      df <- switch(file_ext,
                   "csv" = read.csv(input$file$datapath, sep = input$separator),
                   "xlsx" = read_excel(input$file$datapath),
                   stop("Unsupported file format.")
      )
      file_data(df)
    }, error = function(e) {
      showNotification("Error loading the file. Check the format and separator.", type = "error")
      print(e)  # Debugging error message
    })
  })
  
  # Dynamically determine the data source for charts and other outputs
  get_data <- reactive({
    df <- if (!is.null(file_data())) {
      file_data()  # Use uploaded file data
    } else {
      initial_data()  # Use editable table data
    }
    
    # Ensure manually added columns are numeric
    for (col in names(df)) {
      if (startsWith(col, "NewColumn")) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    return(df)  # Return the processed data
  })
  # Set `isFileUploaded` based on whether `file_data()` is NULL
  output$isFileUploaded <- reactive({
    return(!is.null(file_data()))
  })
  
  # Set `isFileUploaded` based on whether `file_data()` is NULL
  outputOptions(output, "isFileUploaded", suspendWhenHidden = FALSE)  # Ensure this updates dynamically
  
  # Render the uploaded file contents (if a file is uploaded)
  output$file_contents <- renderDT({
    df <- get_data()
    datatable(df, editable = TRUE)
  })
  
  # Render the editable table (when no file is uploaded)
  output$editable_table <- renderDT({
    df <- get_data()
    numeric_columns <- which(sapply(df, is.numeric)) - 1  # Adjust for zero-based indexing
    updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
    datatable(df, editable = TRUE, selection = "single", options = list(
      pageLength = 5,
      paging = FALSE,
      searching = TRUE,
      info = FALSE,
      dom = 'ftip',
      server = FALSE, 
      columnDefs = list(
        list(className = 'dt-right', targets = numeric_columns)  # Align all columns to the right
      )
    ))
  })          
  
  # Edit cell function
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    df <- initial_data()
    df[info$row, info$col] <- DT::coerceValue(info$value, as.numeric(df[[info$col]]))
    initial_data(df)
  })
  
  output$concat_summary <- renderPrint({
    df <- get_data()
    req(input$variables)
    selected_var <- input$variables
    summary(df[[selected_var]])
  })
  
  # Histogram with Normality Curve
  output$histPlot <- renderPlot({
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    xlim_values <- range(c(input$lic, input$ls, concatenated_values), na.rm = TRUE)
    bins <- seq(min(concatenated_values), max(concatenated_values), length.out = input$Classes + 1)
    
    hist_data <- hist(concatenated_values, breaks = bins, col = 'lightblue', border = 'grey',
                      xlab = "Data", main = "Histogram of Selected Variables",
                      ylab = "Frequency", freq = TRUE, xlim = xlim_values)
    
    if (input$include_normal) {
      x <- seq(min(concatenated_values), max(concatenated_values), length.out = 100)
      y <- dnorm(x, mean = mean(concatenated_values), sd = sd(concatenated_values))
      y <- y * length(concatenated_values) * diff(hist_data$breaks)[1]
      lines(x, y, col = "black", lwd = 2)
    }
    
    if (input$include_tolerance) {
      if (!is.null(input$lic)) abline(v = input$lic, col = "red", lwd = 2, lty = 2)
      if (!is.null(input$lsc)) abline(v = input$lsc, col = "green", lwd = 2, lty = 2)
      if (!is.null(input$ls)) abline(v = input$ls, col = "red", lwd = 2, lty = 4)
    }
  })
  
  # QQ Plot
  output$qqPlot <- renderPlot({
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    qqnorm(concatenated_values, main = "QQ Plot of Selected Variables", pch = 19, col = "lightblue")
    qqline(concatenated_values, col = "red", lwd = 2)
  })
  
  # Statistical Tests
  output$lillieforsTest <- renderPrint({
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    lillie.test(concatenated_values)
  })
  
  output$shapirotest <- renderPrint({
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    shapiro.test(concatenated_values)
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    df <- get_data()
    req(df, input$scatter_x, input$scatter_y)
    
    plot(df[[input$scatter_x]], df[[input$scatter_y]],
         xlab = input$scatter_x, ylab = input$scatter_y,
         main = paste("Scatter Plot of", input$scatter_x, "vs", input$scatter_y),
         pch = 19, col = "blue")
    
    model <- lm(df[[input$scatter_y]] ~ df[[input$scatter_x]])
    abline(model, col = "red", lwd = 2)
  })
  
  output$spearmantest <- renderPrint({
    df <- get_data()
    req(df, input$scatter_x, input$scatter_y)
    x <- df[[input$scatter_x]]
    y <- df[[input$scatter_y]]
    
    validate(need(is.numeric(x), "The selected X variable must be numeric."))
    validate(need(is.numeric(y), "The selected Y variable must be numeric."))
    
    cor.test(df[[input$scatter_x]], df[[input$scatter_y]], method = "spearman")
  })
  
  # Pareto Chart
  output$paretoPlot <- renderPlot({
    df <- get_data()
    req(df, input$pareto_vars)
    selected_var <- input$pareto_vars
    validate(need(selected_var %in% names(df), "The selected variable does not exist in the dataset."))
    
    # Contagem e ordenação decrescente
    freq_table <- table(df[[selected_var]])
    
    freq_table <- freq_table[order(freq_table, decreasing = TRUE)]
    
    # Cálculo da frequência acumulada
    cum_freq <- cumsum(freq_table)
    
    # Definição dos limites do eixo y
    y1 <- c(0, max(cum_freq) * 1.04)  # 4% extra no topo
    y2 <- c(0, 104)  # Eixo percentual acumulado
    
    # Armazena as configurações atuais do gráfico e ajusta margens
    oldpar <- par(mar = c(5, 4, 4, 4) + 0.1)
    
    # Criação do barplot
    bp <- barplot(freq_table, ylim = y1, ylab = "Frequency", col = "steelblue", las = 2, cex.names = 0.8)
    
    # Adiciona a linha do percentual acumulado
    points(bp[, 1], cum_freq, type = "b", col = "red", lwd = 2, pch = 19)
    
    # Adiciona o eixo secundário (percentual acumulado)
    y2lab <- pretty(c(0, 100))  # Define intervalos "bonitos" para o eixo
    y2at <- y2lab / 100 * max(cum_freq)  # Ajusta para a escala dos dados
    axis(4, at = y2at, labels = paste0(y2lab, "%"))
    mtext("Cumulative Percentage (%)", side = 4, line = 3)
    
    # Restaura os parâmetros gráficos originais
    par(oldpar)
  })
  
  # Função para validar e processar os dados
  validate_and_process <- function(data, variable_name, subgroup_size) {
    variable <- data[[variable_name]]
    
    # Validar se a variável é numérica
    validate(need(is.numeric(variable), "The selected variable must be numeric"))
    # Validar tamanho dos dados
    validate(need(length(variable) >= subgroup_size, "The amount of data must be greater than or equal to the size of the subgroup."))
    # Ajustar tamanho dos dados para ser divisível pelo tamanho do subgrupo
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    
    # Criar matriz de subgrupos
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    
    return(subgroups)            
    
  }            
  # UI for selectors
  output$var_select_ui <- renderUI({
    req(get_data())
    selectInput("sample_col", "Select Sample Size Variable (e.g. Total_Produced):",
                choices = names(get_data()), selected = "Total_Produced")
  })
  
  output$obs_select_ui <- renderUI({
    req(get_data())
    selectInput("obs_cols", "Select Observation Variables (e.g. Scratches, Cracks, etc.):",
                choices = names(get_data()), multiple = TRUE,
                selected = c("Scratches", "Cracks", "Incorrect_Size"))
  })
  
  # Processed data for p-chart
  processed_data <- reactive({
    df <- get_data()
    req(input$sample_col, input$obs_cols)
    
    if (!"Month" %in% names(df)) {
      df$Month <- paste0("Obs", seq_len(nrow(df)))
    }
    
    df_long <- df %>%
      select(Month, all_of(input$sample_col), all_of(input$obs_cols)) %>%
      pivot_longer(cols = all_of(input$obs_cols), names_to = "Defect_Type", values_to = "Defect_Count") %>%
      mutate(
        Group = paste(Month, Defect_Type, sep = "-"),
        Sample_Size = rep(df[[input$sample_col]], each = length(input$obs_cols)),
        Observation = paste("Observation", match(Defect_Type, input$obs_cols))
      )
    
    df_long
  })
  
  # Proportion Control Chart (p-chart)
  output$controlChart <- renderPlot({
    df <- processed_data()
    qcc_obj <- qcc(
      data = df$Defect_Count,
      sizes = df$Sample_Size,
      type = "p",
      labels = df$Group,
      title = "Proportion Control Chart",
      xlab = "Observation Group",
      ylab = "Proportion Defective"
    )
    plot(qcc_obj)
  })
  
  # Summary Table of control chart
  output$summary_table <- renderDT({
    df <- processed_data()
    summary <- data.frame(
      Group = df$Group,
      Observation = df$Observation,
      Defect_Type = df$Defect_Type,
      Defect_Count = df$Defect_Count,
      Sample_Size = df$Sample_Size,
      Proportion_Defective = round(df$Defect_Count / df$Sample_Size, 4)
    )
    datatable(summary, options = list(pageLength = 10))
  })
  
  # Dynamic variable selection for x-bar chart
  observe({
    updateSelectizeInput(session, "control_variable", choices = names(get_data()), server = TRUE)
  })
  
  # X-bar Chart with Range or SD
  output$xbar_chart <- renderPlot({
    req(input$control_variable)
    df <- get_data()
    var_data <- df[[input$control_variable]]
    
    req(length(var_data) >= input$subgroup_size)
    
    grouped <- split(var_data, ceiling(seq_along(var_data) / input$subgroup_size))
    means <- sapply(grouped, mean)
    
    qcc_obj <- qcc(grouped, type = "xbar", std.dev = ifelse(input$chart_type == "S", "SD", "RANGE"))
    plot(qcc_obj)
  })
  
  # Secondary chart (Range or SD)
  output$control_chart <- renderPlot({
    req(input$control_variable)
    df <- get_data()
    var_data <- df[[input$control_variable]]
    
    req(length(var_data) >= input$subgroup_size)
    
    grouped <- split(var_data, ceiling(seq_along(var_data) / input$subgroup_size))
    
    qcc_obj <- qcc(grouped, type = input$chart_type)
    plot(qcc_obj)
  })
  
  # Análise de Capacidade
  output$capability_summary <- renderPrint({
    df <- get_data()
    req(df, input$capability_variable, input$lie, input$les, input$subgroup_size_capability, input$target)
    
    # Validação dos limites e target
    if (input$lie >= input$les) stop("The lower limit (LIE) must be less than the upper limit (LES).")
    if (is.na(input$target)) stop("Please, define the target value.")
    if (input$target < input$lie || input$target > input$les) stop("The target value must be between the lower limit (LIE) and the upper limit (LES).")
    
    # Criar subgrupos
    subgroups <- create_subgroups(df, input$capability_variable, as.numeric(input$subgroup_size_capability))
    
    # Análise de capacidade
    qcc_obj <- qcc(subgroups, type = "xbar", nsigmas = 3, plot = FALSE)
    capability_analysis <- qcc::process.capability(qcc_obj, spec.limits = c(input$lie, input$les), target = input$target)
    
    # Imprimir toda a análise de capacidade
    print(capability_analysis)
  })
  
}

shinyApp(ui = ui, server = server) 
