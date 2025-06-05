library(shiny)
library(shinythemes)
library(readxl)
library(nortest)
library(qcc)
library(DT)

# Translation Table as List 
translations <- list(
  en = list(
    search = "search:",
    Month = "Month",
    Items_Produced = "Items Produced",
    Defective_Items = "Defective Items",
    Defect_Type = "Defect Type",
    Scratches = "Scratches",
    Cracks = "Cracks",
    Incorrect_Size = "Incorrect Size",
    "Incorrect Size" = "Incorrect Size",
    Browse = "Browse",
    "No file selected" = " No File Selected",
    upload_file = "Upload File",
    separator = "Separator (for CSV):",
    comma = "Comma",
    semicolon = "Semicolon",
    tab = "Tab",
    add_row = "Add Row",
    add_col = "Add Column",
    delete_row = "Delete Selected Row",
    delete_col = "Delete Selected Column",
    column_to_delete = "Select Column to Delete",
    column_to_rename = "Select Column to Rename",
    new_col_name = "New Column Name",
    rename_col = "Rename Column",
    download_data = "Download Updated Data",
    data_tab = "Data",
    hist_tab = "Histogram and Normality",
    classes = "Number of classes:",
    lic = "Lower Specification Limit (LSL):",
    lsc = "Specification Limit (SL):",
    ls = "Upper Specification Limit (USL):",
    include_normal = "Include normality curve",
    include_tolerance = "Include specification limits",
    concat_summary = "Summary",
    scatter_tab = "Scatter",
    scatter_x = "Select variable for X axis:",
    scatter_y = "Select variable for Y axis:",
    pareto_tab = "Pareto Chart",
    pareto_vars = "Select variable for Pareto chart:",
    control_tab = "Control Chart",
    control_variable = "Select variable for control chart:",
    subgroup_size = "Subgroup Size:",
    chart_type = "Select Control Chart type:",
    mean_deviation = "Mean-Deviation",
    mean_range = "Mean-Range",
    capability_tab = "Capability Analysis",
    capability_variable = "Select variable for capability analysis:",
    lie = "Lower Specification Limit (LSL):",
    les = "Upper Specification Limit (USL):",
    target = "Target Value:",
    subgroup_size_capability = "Subgroup Size:",
    license_tit = "License",
    license_title = "Creative Commons Attribution-NonCommercial 4.0 International License",
    license_text = "This app is licensed under the Creative Commons Attribution-NonCommercial 4.0 International License.",
    license_li1 = "Attribution: You must give appropriate credit, provide a link to the license, and indicate if changes were made.",
    license_li2 = "NonCommercial: You may not use the material for commercial purposes.",
    license_li3 = "No Additional Restrictions: You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.",
    license_link = "Read the Full License (Click Here)"
  ),
  it = list(
    Month = "Mese",
    search = "Cerca:",
    Items_Produced = "Articoli Prodotti",
    Defective_Items = "Articoli Difettosi",
    Defect_Type = "Tipo di Difetto",
    Scratches = "Graffi",
    Cracks = "Crepe",
    Incorrect_Size = "Dimensione Erratta",
    "Incorrect Size" = "Dimensione Erratta",
    Browse = "Naviga",
    No_file_selected= "Nessun file selezionato",
    upload_file = "Carica File",
    separator = "Separatore (per CSV):",
    comma = "Virgola",
    semicolon = "Punto e virgola",
    tab = "Tabulazione",
    add_row = "Aggiungi riga",
    add_col = "Aggiungi colonna",
    delete_row = "Elimina riga selezionata",
    delete_col = "Elimina colonna selezionata",
    column_to_delete = "Seleziona colonna da eliminare",
    column_to_rename = "Seleziona colonna da rinominare",
    new_col_name = "Nuovo nome della colonna",
    rename_col = "Rinomina colonna",
    download_data = "Scarica dati aggiornati",
    data_tab = "Dati",
    hist_tab = "Istogramma e normalità",
    classes = "Numero di classi:",
    lic = "Limite di specifica inferiore (LSL):",
    lsc = "Limite di specifica (SL):",
    ls = "Limite di specifica superiore (USL):",
    include_normal = "Includi curva di normalità",
    include_tolerance = "Includi limiti di specifica",
    concat_summary = "Sommario",
    scatter_tab = "Dispersione",
    scatter_x = "Seleziona variabile per l'asse X:",
    scatter_y = "Seleziona variabile per l'asse Y:",
    pareto_tab = "Diagramma di Pareto",
    pareto_vars = "Seleziona variabile per il diagramma di Pareto:",
    control_tab = "Diagramma di controllo",
    control_variable = "Seleziona variabile per il diagramma di controllo:",
    subgroup_size = "Dimensione sottogruppo:",
    chart_type = "Seleziona tipo di diagramma di controllo:",
    mean_deviation = "Media-Deviazione",
    mean_range = "Media-Intervallo",
    capability_tab = "Analisi di capacità",
    capability_variable = "Seleziona variabile per l’analisi di capacità:",
    lie = "Limite di specifica inferiore (LSL):",
    les = "Limite di specifica superiore (USL):",
    target = "Valore target:",
    subgroup_size_capability = "Dimensione sottogruppo:",
    license_tit = "Licenza",
    license_title = "Licenza Creative Commons Attribuzione-NonCommerciale 4.0 Internazionale",
    license_text = "Questa app è concessa in licenza sotto la Licenza Creative Commons Attribuzione-NonCommerciale 4.0 Internazionale.",
    license_li1 = "Attribuzione: Devi dare il giusto credito, fornire un link alla licenza e indicare se sono state apportate modifiche.",
    license_li2 = "NonCommerciale: Non puoi utilizzare il materiale per scopi commerciali.",
    license_li3 = "Nessuna restrizione aggiuntiva: Non puoi applicare termini legali o misure tecnologiche che limitino legalmente gli altri di fare ciò che la licenza permette.",
    license_link = "Leggi la Licenza Completa (Clicca Qui)"
  ),
  pt = list(
    search = "Pesquisar:",
    Month = "Mês",
    Items_Produced = "Itens Produzido",
    Defective_Items = "Itens Defeituosos",
    Defect_Type = "Tipo de Defeito",
    Scratches = "	Arranhões",
    Cracks = "Rachaduras",
    Incorrect_Size = "Tamanho Incorreto",
    "Incorrect Size" = "Tamanho Incorreto",
    Browse = "Navegar",
    No_file_selected = "Nenhum Arquivo Selecionado",
    upload_file = "Carregar Arquivo",
    separator = "Separador (para CSV):",
    comma = "Vírgula",
    semicolon = "Ponto e vírgula",
    tab = "Tabulação",
    add_row = "Adicionar linha",
    add_col = "Adicionar coluna",
    delete_row = "Excluir linha selecionada",
    delete_col = "Excluir coluna selecionada",
    column_to_delete = "Selecionar coluna para excluir",
    column_to_rename = "Selecionar coluna para renomear",
    new_col_name = "Novo nome da coluna",
    rename_col = "Renomear coluna",
    download_data = "Baixar dados atualizados",
    data_tab = "Dados",
    hist_tab = "Histograma e Normalidade",
    classes = "Número de classes:",
    lic = "Limite inferior de especificação (LSL):",
    lsc = "Limite de especificação (SL):",
    ls = "Limite superior de especificação (USL):",
    include_normal = "Incluir curva de normalidade",
    include_tolerance = "Incluir limites de especificação",
    concat_summary = "Sumário",
    scatter_tab = "Dispersão",
    scatter_x = "Selecionar variável para o eixo X:",
    scatter_y = "Selecionar variável para o eixo Y:",
    pareto_tab = "Gráfico de Pareto",
    pareto_vars = "Selecionar variável para gráfico de Pareto:",
    control_tab = "Gráfico de Controle",
    control_variable = "Selecionar variável para gráfico de controle:",
    subgroup_size = "Tamanho do subgrupo:",
    chart_type = "Selecionar tipo de gráfico de controle:",
    mean_deviation = "Média-Desvio",
    mean_range = "Média-Alcance",
    capability_tab = "Análise de Capacidade",
    capability_variable = "Selecionar variável para análise de capacidade:",
    lie = "Limite inferior de especificação (LSL):",
    les = "Limite superior de especificação (USL):",
    target = "Valor alvo:",
    subgroup_size_capability = "Tamanho do subgrupo:",
    license_tit = "Licença",
    license_title = "Licença Creative Commons Atribuição-NoComercial 4.0 Internacional",
    license_text = "Este aplicativo está licenciado sob a Licença Creative Commons Atribuição-NãoComercial 4.0 Internacional.",
    license_li1 = "Atribuição: Você deve dar o devido crédito, fornecer um link para a licença e indicar se mudanças foram feitas.",
    license_li2 = "Não Comercial: Você não pode usar o material para fins comerciais.",
    license_li3 = "Nenhuma restrição adicional: Você não pode aplicar termos legais ou medidas tecnológicas que restrinjam legalmente outros de fazer o que a licença permite.",
    license_link = "Leia a Licença Completa (Clique Aqui)"
  )
)

# Helper translation function
tr <- function(key, lang) {
  out <- translations[[lang]][[key]]
  if(is.null(out)) key else out
}

options(shiny.maxRequestSize = 200*1024^2)

ui <- fluidPage(
  theme = shinytheme("readable"),
  selectInput("selected_language", "Language", choices = c("it","en","pt"), selected = "en"),
  uiOutput("main_ui"),
)

server <- function(input, output, session) {
  get_lang <- reactive({ input$selected_language })
  observe({
    lang <- get_lang()
    session$sendCustomMessage(
      "updateFileInputLang",
      list(
        browse = tr("Browse", lang),
        no_file_selected = tr("No_file_selected", lang)
      )
    )
  })
  
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
  
  get_data <- reactive({
    df <- if (!is.null(file_data())) {
      file_data()
    } else {
      initial_data()
    }
    for (col in names(df)) {
      if (startsWith(col, "NewColumn")) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    df
  })
  
  get_numeric_vars <- function(df, selected_vars, lang) {
    validate(need(!is.null(selected_vars), tr("No variables selected.", lang)))
    validate(need(all(selected_vars %in% names(df)), tr("Selected variables do not exist in the dataset.", lang)))
    num_vars <- selected_vars[sapply(df[selected_vars], is.numeric)]
    validate(need(length(num_vars) > 0, tr("Please select at least one numeric variable.", lang)))
    return(num_vars)
  }
  
  output$main_ui <- renderUI({
    lang <- get_lang()
    fluidPage(
      titlePanel(
        windowTitle = "QualiSigma",
        title = div(img(src = "logo.png", height = "70px"), tr("QualiSigma", lang))
      ),
      sidebarLayout(
        sidebarPanel(
          tags$div(style = "border: 2px solid #007BFF; border-radius: 5px; padding: 10px; margin-bottom: 30px;",
                   fileInput("file", tr("upload_file", lang), accept = c(".csv", ".xlsx")),
                   radioButtons("separator", tr("separator", lang),
                                choices = setNames(c(",", ";", "\t"),
                                                   c(tr("comma", lang),
                                                     tr("semicolon", lang),
                                                     tr("tab", lang))),
                                selected = ","
                   ),
                   conditionalPanel(
                     condition = "input.tabs == 'histogram'",
                     selectizeInput("variables", tr("Select variables for histogram:", lang), choices = NULL, multiple = FALSE),
                     sliderInput("Classes", tr("classes", lang), min = 1, max = 50, value = 12),
                     numericInput("lic", tr("lic", lang), value = NULL),
                     numericInput("lsc", tr("lsc", lang), value = NULL),
                     numericInput("ls", tr("ls", lang), value = NULL),
                     checkboxInput("include_normal", tr("include_normal", lang), value = TRUE),
                     checkboxInput("include_tolerance", tr("include_tolerance", lang), value = TRUE)
                   ),
                   conditionalPanel(
                     condition = "input.tabs == 'data'",
                     actionButton("add_row", tr("add_row", lang), style = "margin-right: 10px;"),
                     actionButton("add_col", tr("add_col", lang),  style = "margin-right: 10px;"),
                     actionButton("delete_row", tr("delete_row", lang)),
                     br(), br(),
                     selectInput("column_to_delete", tr("column_to_delete", lang), choices = NULL),
                     actionButton("delete_col", tr("delete_col", lang)),
                     br(), br(),
                     selectInput("column_to_rename", tr("column_to_rename", lang), choices = NULL),
                     textInput("new_col_name", tr("new_col_name", lang)),
                     actionButton("rename_col", tr("rename_col", lang)),
                     downloadButton("download_data", tr("download_data", lang))
                   )
          )
        ),
        mainPanel(
          tabsetPanel(id = "tabs", selected = "data",
                      tabPanel(tr("data_tab", lang), value = "data",
                               conditionalPanel(condition = "output.isFileUploaded",
                                                DTOutput("file_contents")
                               ),
                               conditionalPanel(
                                 condition = "output.isFileUploaded == false",
                                 DTOutput("editable_table")
                               )
                      ),
                      tabPanel(tr("hist_tab", lang), value = "histogram",
                               verbatimTextOutput("concat_summary"),
                               plotOutput("histPlot"),
                               plotOutput("qqPlot"),
                               verbatimTextOutput("lillieforsTest"),
                               verbatimTextOutput("shapirotest")
                      ),
                      tabPanel(tr("scatter_tab", lang), value = "scatter",
                               selectizeInput("scatter_x", tr("scatter_x", lang), choices = NULL),
                               selectizeInput("scatter_y", tr("scatter_y", lang), choices = NULL),
                               plotOutput("scatterPlot"),
                               verbatimTextOutput("spearmantest")
                      ),
                      tabPanel(tr("pareto_tab", lang), value = "pareto",
                               selectizeInput("pareto_vars", tr("pareto_vars", lang), choices = NULL),
                               plotOutput("paretoPlot")
                      ),
                      tabPanel(tr("control_tab", lang), value = "control",
                               selectizeInput("control_variable", tr("control_variable", lang), choices = NULL),
                               numericInput("subgroup_size", tr("subgroup_size", lang), value = 5, min = 2, step = 1),
                               selectizeInput("chart_type",
                                              tr("chart_type", lang),
                                              choices = setNames(c("S", "R"),
                                                                 c(tr("mean_deviation", lang), tr("mean_range", lang))),
                                              selected = "S"),
                               DTOutput("control_subgroup_table"),
                               br(),
                               plotOutput("xbar_chart"),
                               plotOutput("control_chart")
                      ),
                      tabPanel(tr("capability_tab", lang), value = "capability",
                               selectizeInput("capability_variable", tr("capability_variable", lang), choices = NULL),
                               numericInput("lie", tr("lie", lang), value = NULL),
                               numericInput("target", tr("target", lang), value = NA, step = 0.01),
                               numericInput("les", tr("les", lang), value = NULL),
                               numericInput("subgroup_size_capability", tr("subgroup_size_capability", lang), value = 5, min = 2, step = 1),
                               verbatimTextOutput("capability_summary")
                      ),
                      tabPanel(tr("license_tit", lang), value = "license",
                               h3(tr("license_title", lang)),
                               p(tr("license_text", lang)),
                               tags$ul(
                                 tags$li(tr("license_li1", lang)),
                                 tags$li(tr("license_li2", lang)),
                                 tags$li(tr("license_li3", lang))
                               ),
                               tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode",
                                      tr("license_link", lang),
                                      target = "_blank"),
                               tags$code("https://creativecommons.org/licenses/by-nc/4.0/legalcode")
                      )
          )
        )
      )
    )
  })
  
  observeEvent(list(get_data(), input$selected_language), {
    df <- get_data()
    numeric_names <- names(df)[sapply(df, is.numeric)]
    updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_x", choices = numeric_names, server = TRUE)
    updateSelectizeInput(session, "scatter_y", choices = numeric_names, server = TRUE)
    updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "control_variable", choices = numeric_names, server = TRUE)
    updateSelectizeInput(session, "capability_variable", choices = numeric_names, server = TRUE)
    updateSelectInput(session, "column_to_delete", choices = names(df))
    updateSelectInput(session, "column_to_rename", choices = names(df))
  })
  
  observe({
    df <- if (is.null(file_data())) {
      initial_data()
    } else {
      file_data()
    }
    updateSelectInput(session, "column_to_rename", choices = names(df))
    updateSelectInput(session, "column_to_delete", choices = names(df))
  })
  
  observeEvent(input$add_row, {
    df <- initial_data()
    new_row <- as.list(rep(NA, ncol(df)))
    names(new_row) <- names(df)
    df <- rbind(df, new_row)
    initial_data(df)
  })
  
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
    df[[new_col_name]] <- as.numeric(NA)
    initial_data(df)
  })
  
  observeEvent(input$delete_row, {
    req(input$editable_table_rows_selected)
    df <- initial_data()
    row_to_delete <- input$editable_table_rows_selected
    df <- df[-row_to_delete, , drop = FALSE]
    initial_data(df)
  })
  
  observeEvent(input$delete_col, {
    req(input$column_to_delete)
    df <- initial_data()
    col_to_delete <- input$column_to_delete
    df <- df[, !(names(df) %in% col_to_delete), drop = FALSE]
    initial_data(df)
  })
  
  observeEvent(input$rename_col, {
    req(input$column_to_rename, input$new_col_name)
    df <- initial_data()
    names(df)[names(df) == input$column_to_rename] <- input$new_col_name
    initial_data(df)
  })
  
  observe({
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    tryCatch({
      df <- switch(file_ext,
                   "csv" = read.csv(input$file$datapath, sep = input$separator),
                   "xlsx" = read_excel(input$file$datapath),
                   stop("Unsupported file format.")
      )
      file_data(df)
    }, error = function(e) {
      showNotification("Error loading the file. Check the format and separator.", type = "error")
    })
  })
  
  output$isFileUploaded <- reactive({
    return(!is.null(file_data()))
  })
  outputOptions(output, "isFileUploaded", suspendWhenHidden = FALSE)
  
  output$file_contents <- renderDT({
    df <- get_data()
    datatable(df, editable = TRUE)
  })
  
  output$editable_table <- renderDT({
    lang <- get_lang()
    df <- get_data()
    colnames(df) <- sapply(colnames(df), function(nm) tr(nm, lang))
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        df[[col]] <- sapply(df[[col]], function(val) tr(val, lang))
      }
    }
    
    numeric_columns <- which(sapply(df, is.numeric)) - 1
    datatable(df, editable = TRUE, selection = "single", options = list(
      pageLength = 5,
      paging = FALSE,
      searching = TRUE,
      info = FALSE,
      dom = 'ftip',
      server = FALSE,
      columnDefs = list(
        list(className = 'dt-right', targets = numeric_columns)
      ),
      language = list(search = tr("search", lang))
    )
    )
  })
  
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
  
  output$histPlot <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables, lang)
    concatenated_values <- unlist(df[num_vars])
    if(all(is.na(concatenated_values))) return()
    xlim_values <- range(c(input$lic, input$ls, concatenated_values), na.rm = TRUE)
    bins <- seq(min(concatenated_values, na.rm = TRUE), max(concatenated_values, na.rm = TRUE), length.out = input$Classes + 1)
    hist_data <- hist(concatenated_values, breaks = bins, col = 'lightblue', border = 'grey',
                      xlab = tr("data_tab", lang), main = tr("hist_tab", lang),
                      ylab = tr("concat_summary", lang), freq = TRUE, xlim = xlim_values)
    if (input$include_normal) {
      x <- seq(min(concatenated_values, na.rm = TRUE), max(concatenated_values, na.rm = TRUE), length.out = 100)
      y <- dnorm(x, mean = mean(concatenated_values, na.rm = TRUE), sd = sd(concatenated_values, na.rm = TRUE))
      y <- y * length(concatenated_values) * diff(hist_data$breaks)[1]
      lines(x, y, col = "black", lwd = 2)
    }
    if (input$include_tolerance) {
      if (!is.null(input$lic)) abline(v = input$lic, col = "red", lwd = 2, lty = 2)
      if (!is.null(input$lsc)) abline(v = input$lsc, col = "green", lwd = 2, lty = 2)
      if (!is.null(input$ls)) abline(v = input$ls, col = "red", lwd = 2, lty = 4)
    }
  })
  
  
  output$qqPlot <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables, lang)
    concatenated_values <- unlist(df[num_vars])
    if(all(is.na(concatenated_values))) return()
    qqnorm(concatenated_values, main = paste("QQ Plot -", tr("hist_tab", lang)), pch = 19, col = "lightblue")
    qqline(concatenated_values, col = "red", lwd = 2)
  })
  
  
  output$lillieforsTest <- renderPrint({
    lang <- get_lang()
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables, lang)
    concatenated_values <- unlist(df[num_vars])
    lillie.test(concatenated_values)
  })
  
  output$shapirotest <- renderPrint({
    lang <- get_lang()
    df <- get_data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables, lang)
    concatenated_values <- unlist(df[num_vars])
    shapiro.test(concatenated_values)
  })
  
  
  # generate scatterplot 
  output$scatterPlot <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$scatter_x, input$scatter_y)
    x <- df[[input$scatter_x]]
    y <- df[[input$scatter_y]]
    validate(
      need(is.numeric(x), "X must be numeric"),
      need(is.numeric(y), "Y must be numeric"),
      need(sum(!is.na(x)) > 0 && sum(!is.na(y)) > 0, "No numeric data to plot!")
    )
    plot(x, y,
         xlab = input$scatter_x, ylab = input$scatter_y,
         main = paste(tr("scatter_tab", lang), "-", input$scatter_x, "vs", input$scatter_y),
         pch = 19, col = "blue")
    model <- lm(y ~ x)
    abline(model, col = "red", lwd = 2)
  })
  
  output$spearmantest <- renderPrint({
    lang <- get_lang()
    df <- get_data()
    req(df, input$scatter_x, input$scatter_y)
    x <- df[[input$scatter_x]]
    y <- df[[input$scatter_y]]
    validate(need(is.numeric(x), tr("The selected X variable must be numeric.", lang)))
    validate(need(is.numeric(y), tr("The selected Y variable must be numeric.", lang)))
    cor.test(x, y, method = "spearman")
  })
  
  
  # Generate Pereto chart
  output$paretoPlot <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$pareto_vars)
    selected_var <- input$pareto_vars
    validate(need(selected_var %in% names(df), tr("The selected variable does not exist in the dataset.", lang)))
    freq_table <- table(df[[selected_var]])
    freq_table <- freq_table[order(freq_table, decreasing = TRUE)]
    cum_freq <- cumsum(freq_table)
    y1 <- c(0, max(cum_freq) * 1.04)
    y2 <- c(0, 104)
    oldpar <- par(mar = c(5, 4, 4, 4) + 0.1)
    bp <- barplot(freq_table, ylim = y1, ylab = tr("concat_summary", lang), col = "steelblue", las = 2, cex.names = 0.8)
    points(bp[, 1], cum_freq, type = "b", col = "red", lwd = 2, pch = 19)
    y2lab <- pretty(c(0, 100))
    y2at <- y2lab / 100 * max(cum_freq)
    axis(4, at = y2at, labels = paste0(y2lab, "%"))
    mtext("Cumulative Percentage (%)", side = 4, line = 3)
    par(oldpar)
  })
  
  # functions to validate the data 
  validate_and_process <- function(data, variable_name, subgroup_size, lang) {
    variable <- data[[variable_name]]
    validate(need(is.numeric(variable), tr("The selected variable must be numeric", lang)))
    validate(need(length(variable) >= subgroup_size, tr("The amount of data must be greater than or equal to the size of the subgroup.", lang)))
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
  }
  
  #functions to create the control chart 
  generate_control_chart <- function(subgroups, chart_type) {
    if (chart_type == "xbar") {
      qcc_chart <- qcc(data = subgroups, type = "xbar")
    } else if (chart_type == "S") {
      qcc_chart <- qcc(data = subgroups, type = "S")
    } else if (chart_type == "R") {
      qcc_chart <- qcc(data = subgroups, type = "R")
    } else {
      stop("Invalid chart type.")
    }
    qcc_chart
  }
  
  # Generate X-bar chart 
  output$xbar_chart <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$control_variable, input$subgroup_size)
    subgroups <- validate_and_process(df, input$control_variable, as.numeric(input$subgroup_size), lang)
    qcc_xbar <- generate_control_chart(subgroups, "xbar")
    plot(qcc_xbar)
  })
  
  #Generate S or R chart 
  output$control_chart <- renderPlot({
    lang <- get_lang()
    df <- get_data()
    req(df, input$control_variable, input$subgroup_size, input$chart_type)
    subgroups <- validate_and_process(df, input$control_variable, as.numeric(input$subgroup_size), lang)
    qcc_control <- generate_control_chart(subgroups, input$chart_type)
    plot(qcc_control)
  })
  
  #Generate subgroup table 
  create_subgroups <- function(data, variable_name, subgroup_size, lang) {
    variable <- data[[variable_name]]
    if (!is.numeric(variable)) stop(tr("The selected variable must be numeric.", lang))
    if (length(variable) < subgroup_size) stop(tr("The amount of data must be greater than or equal to the size of the subgroup.", lang))
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    if (nrow(subgroups) <= 1) stop(tr("More than one subgroup is required to perform the analysis.", lang))
    df_subgroups <- as.data.frame(subgroups)
    
    
    colnames(df_subgroups) <- paste0("observation ", seq_len(ncol(df_subgroups)))
    rownames(df_subgroups) <- paste0("group ", seq_len(nrow(df_subgroups)))
    return(df_subgroups)
  }
  output$control_subgroup_table <- DT::renderDT({
    req(input$control_variable, input$subgroup_size)
    df <- get_data()
    lang <- get_lang()
    # Use your create_subgroups function so it returns the desired table
    subgroup_table <- create_subgroups(df, input$control_variable, as.numeric(input$subgroup_size), lang)
    DT::datatable(subgroup_table, options = list(dom = 't', paging = FALSE), rownames = TRUE)
  })
  
  #capability ANalysis 
  output$capability_summary <- renderPrint({
    lang <- get_lang()
    df <- get_data()
    req(df, input$capability_variable, input$lie, input$les, input$subgroup_size_capability, input$target)
    if (input$lie >= input$les) stop(tr("The lower limit (LIE) must be less than the upper limit (LES).", lang))
    if (is.na(input$target)) stop(tr("Please, define the target value.", lang))
    if (input$target < input$lie || input$target > input$les) stop(tr("The target value must be between the lower limit (LIE) and the upper limit (LES).", lang))
    subgroups <- create_subgroups(df, input$capability_variable, as.numeric(input$subgroup_size_capability), lang)
    qcc_obj <- qcc(subgroups, type = "xbar", nsigmas = 3, plot = FALSE)
    capability_analysis <- qcc::process.capability(qcc_obj, spec.limits = c(input$lie, input$les), target = input$target)
    print(capability_analysis)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("updated_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- get_data()
      write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui = ui, server = server)
