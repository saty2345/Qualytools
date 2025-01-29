library(shiny)
library(readxl)
library(nortest)
library(qcc)
library(rsconnect)

# Função para validar e obter variáveis numéricas
get_numeric_vars <- function(df, selected_vars) {
  num_vars <- selected_vars[sapply(df[selected_vars], is.numeric)]
  validate(need(length(num_vars) > 0, "Por favor, selecione ao menos uma variável numérica."))
  return(num_vars)
}

# Atualizar entradas selectize
update_selectize_inputs <- function(session, df) {
  updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "scatter_x", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "scatter_y", choices = names(df), server = TRUE)
  updateSelectizeInput(session, "control_variable", choices = names(df), server = TRUE)
}

# Define UI
ui <- fluidPage(
  titlePanel(title = tagList(tags$img(src = "https://i.imgur.com/eoW5yzF.png", height = "70px"))),
  
  # Layout principal
  sidebarLayout(
    sidebarPanel(
      # Primeira seção: Carregar Dados
      tags$div(style = "border: 2px solid #007BFF; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
               fileInput("file", "Carregar arquivo", accept = c(".csv", ".xlsx")),
               radioButtons("separator", "Separador (para CSV):",
                            choices = c(Vírgula = ",", Ponto_E_Vírgula = ";", Tab = "\t"), selected = ",")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Histograma e Normalidade'",
        selectizeInput("variables", "Selecione as variáveis para o histograma:", choices = NULL, multiple = TRUE),
        tags$p("OBS: É Possível concatenar as variáveis."),
        sliderInput("Classes", "Número de classes:", min = 1, max = 50, value = 12),
        numericInput("lic", "Limite Inferior de Especificação (LIE):", value = NULL),
        numericInput("lsc", "Limite de Especificação (LE):", value = NULL),
        numericInput("ls", "Limite Superior de Especificação (LES):", value = NULL),
        checkboxInput("include_normal", "Incluir curva de normalidade", value = TRUE),
        checkboxInput("include_tolerance", "Incluir limites de especificação", value = TRUE)
      )
    ),
    
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Dados", tableOutput("file_contents")),
                  tabPanel("Histograma e Normalidade",
                           verbatimTextOutput("concat_summary"),
                           plotOutput("histPlot"),
                           plotOutput("qqPlot"),
                           verbatimTextOutput("lillieforsTest"),
                           verbatimTextOutput("shapirotest")
                  ),
                  tabPanel("Dispersão",
                           selectizeInput("scatter_x", "Selecione a variável para o eixo X:", choices = NULL),
                           selectizeInput("scatter_y", "Selecione a variável para o eixo Y:", choices = NULL),
                           plotOutput("scatterPlot"),
                           verbatimTextOutput("spearmantest")
                  ),
                  tabPanel("Gráfico de Pareto",
                           selectizeInput("pareto_vars", "Selecione a variável para o gráfico de Pareto:", choices = NULL),
                           plotOutput("paretoPlot")
                  ),
                  tabPanel("Carta de Controle",
                           selectizeInput("control_variable", "Selecione a variável para a carta de controle:", choices = NULL),
                           numericInput("subgroup_size", "Tamanho do Subgrupo:", value = 5, min = 2, step = 1),
                           selectizeInput("chart_type", 
                                          "Selecione o tipo de Carta de Controle:", 
                                          choices = c("Média-Desvio" = "S", "Média-Amplitude" = "R"),
                                          selected = "S"),
                           plotOutput("xbar_chart"),
                           plotOutput("control_chart")
                  ),
                  tabPanel("Análise de Capacidade",  
                           selectizeInput("capability_variable", "Selecione a variável para análise de capacidade:", choices = NULL),
                           numericInput("lie", "Limite Inferior de Especificação (LIE):", value = NULL),
                           numericInput("target", "Valor Alvo (Target):", value = NA, step = 0.01),
                           numericInput("les", "Limite Superior de Especificação (LES):", value = NULL),
                           numericInput("subgroup_size_capability", "Tamanho do Subgrupo:", value = 5, min = 2, step = 1),
                           verbatimTextOutput("capability_summary")  
                  ),
                  
                  tabPanel("Licença",
                           h3("Licença Creative Commons Atribuição-NãoComercial 4.0 Internacional"),
                           p("Este aplicativo é licenciado sob a Licença Creative Commons Atribuição-NãoComercial 4.0 Internacional."),
                           
                           tags$ul(
                             tags$li("Atribuição: Você deve dar o crédito adequado, fornecer um link para a licença e indicar se foram feitas alterações."),
                             tags$li("Não Comercial: O material não pode ser usado para fins comerciais."),
                             tags$li("Sem Restrições Adicionais: Não pode aplicar restrições legais ou medidas tecnológicas que limitem os direitos permitidos pela licença.")
                           ),
                          
                           tags$a(href = "https://creativecommons.org/licenses/by-nc/4.0/legalcode", 
                                  "Leia a Licença Completa (Clique Aqui)", 
                                  target = "_blank"),
                           
                           tags$code("https://creativecommons.org/licenses/by-nc/4.0/legalcode")
                           ) 
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  
  # Função para atualizar os inputs dinamicamente
  update_selectize_inputs <- function(session, df) {
    updateSelectizeInput(session, "variables", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_x", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "scatter_y", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "pareto_vars", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "control_variable", choices = names(df), server = TRUE)
    updateSelectizeInput(session, "capability_variable", choices = names(df), server = TRUE)
  }
  
  # Carregar arquivo
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      df <- switch(ext,
                   "csv" = read.csv(input$file$datapath, sep = input$separator),
                   "xlsx" = read_excel(input$file$datapath),
                   stop("Formato de arquivo não suportado."))
      update_selectize_inputs(session, df)
      return(df)
    }, error = function(e) {
      showNotification("Erro ao carregar o arquivo. Verifique o formato e o separador.", type = "error")
      return(NULL)
    })
  })
  
  # Exibir conteúdo do arquivo
  output$file_contents <- renderTable({
    data()
  }) 
  
  # Resumo: Número total de observações
  output$concat_summary <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    paste("Número total de observações:", length(concatenated_values))
  })
  
  # Histograma com curva de normalidade 
  output$histPlot <- renderPlot({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    xlim_values <- range(c(input$lic, input$ls, concatenated_values), na.rm = TRUE)
    bins <- seq(min(concatenated_values), max(concatenated_values), length.out = input$Classes + 1)
    
    hist_data <- hist(concatenated_values, breaks = bins, col = 'lightblue', border = 'grey',
                      xlab = "Dados", main = "Histograma das Variáveis Selecionadas",
                      ylab = "Frequência", freq = TRUE, xlim = xlim_values)
    
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
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    
    qqnorm(concatenated_values, main = "QQ Plot das Variáveis Selecionadas", pch = 19, col = "lightblue")
    qqline(concatenated_values, col = "red", lwd = 2)
  })
  
  # Testes estatísticos
  output$lillieforsTest <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    lillie.test(concatenated_values)
  })
  
  output$shapirotest <- renderPrint({
    df <- data()
    req(df, input$variables)
    num_vars <- get_numeric_vars(df, input$variables)
    concatenated_values <- unlist(df[num_vars])
    shapiro.test(concatenated_values)
  })
  
  # Gráfico de dispersão
  output$scatterPlot <- renderPlot({
    df <- data()
    req(df, input$scatter_x, input$scatter_y)
    validate(need(input$scatter_x %in% names(df), "Selecione uma variável válida para o eixo X."))
    validate(need(input$scatter_y %in% names(df), "Selecione uma variável válida para o eixo Y."))
    
    plot(df[[input$scatter_x]], df[[input$scatter_y]],
         main = "Gráfico de Dispersão",
         xlab = input$scatter_x,
         ylab = input$scatter_y,
         col = "blue", pch = 19)
    
    model <- lm(df[[input$scatter_y]] ~ df[[input$scatter_x]])
    abline(model, col = "red", lwd = 2)
  })
  
  output$spearmantest <- renderPrint({
    df <- data()
    req(df, input$scatter_x, input$scatter_y)
    x <- df[[input$scatter_x]]
    y <- df[[input$scatter_y]]
    cor.test(x, y, method = "spearman")
  })
  
  #PARETO
  output$paretoPlot <- renderPlot({
    df <- data()
    req(df, input$pareto_vars)
    
    selected_var <- input$pareto_vars
    validate(need(selected_var %in% names(df), "A variável selecionada não existe no dataset."))
    
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
    bp <- barplot(freq_table, ylim = y1, ylab = "Número de Casos", col = "steelblue", las = 2, cex.names = 0.8)
    
    # Adiciona a linha do percentual acumulado
    points(bp[, 1], cum_freq, type = "b", col = "red", lwd = 2, pch = 19)
    
    # Adiciona o eixo secundário (percentual acumulado)
    y2lab <- pretty(c(0, 100))  # Define intervalos "bonitos" para o eixo
    y2at <- y2lab / 100 * max(cum_freq)  # Ajusta para a escala dos dados
    axis(4, at = y2at, labels = paste0(y2lab, "%"))
    mtext("Participação Acumulada (%)", side = 4, line = 3)
    
    # Restaura os parâmetros gráficos originais
    par(oldpar)
  })
  
  
  
  # Função para validar e processar os dados
  validate_and_process <- function(data, variable_name, subgroup_size) {
    variable <- data[[variable_name]]
    
    # Validar se a variável é numérica
    validate(need(is.numeric(variable), "A variável selecionada deve ser numérica."))
    
    # Validar tamanho dos dados
    validate(need(length(variable) >= subgroup_size, "A quantidade de dados deve ser maior ou igual ao tamanho do subgrupo."))
    
    # Ajustar tamanho dos dados para ser divisível pelo tamanho do subgrupo
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    
    # Criar matriz de subgrupos
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    
    return(subgroups)
  }
  
  # Função para gerar e exibir gráficos de controle
  generate_control_chart <- function(subgroups, chart_type) {
    if (chart_type == "xbar") {
      qcc_chart <- qcc(data = subgroups, type = "xbar")
    } else if (chart_type == "S") {
      qcc_chart <- qcc(data = subgroups, type = "S")
    } else if (chart_type == "R") {
      qcc_chart <- qcc(data = subgroups, type = "R")
    } else {
      stop("Tipo de gráfico inválido.")
    }
    
    return(qcc_chart)
  }
  
  # Gerar a Carta de Controle X-bar
  output$xbar_chart <- renderPlot({
    req(data(), input$control_variable, input$subgroup_size)
    
    # Obter dados e criar subgrupos
    subgroups <- validate_and_process(data(), input$control_variable, as.numeric(input$subgroup_size))
    
    # Gerar carta X-bar
    qcc_xbar <- generate_control_chart(subgroups, "xbar")
    
    # Plotar a carta X-bar
    plot(qcc_xbar)
  })
  
  # Gerar a Carta de Controle S ou R
  output$control_chart <- renderPlot({
    req(data(), input$control_variable, input$subgroup_size, input$chart_type)
    
    # Obter dados e criar subgrupos
    subgroups <- validate_and_process(data(), input$control_variable, as.numeric(input$subgroup_size))
    
    # Gerar carta de controle complementar
    qcc_control <- generate_control_chart(subgroups, input$chart_type)
    
    # Plotar o gráfico de controle complementar
    plot(qcc_control)
  })
  
  # Função auxiliar para criar subgrupos
  create_subgroups <- function(data, variable_name, subgroup_size) {
    variable <- data[[variable_name]]
    
    # Validações simples
    if (!is.numeric(variable)) stop("A variável selecionada deve ser numérica.")
    if (length(variable) < subgroup_size) stop("A quantidade de dados deve ser maior ou igual ao tamanho do subgrupo.")
    
    trimmed_variable <- head(variable, floor(length(variable) / subgroup_size) * subgroup_size)
    subgroups <- matrix(trimmed_variable, ncol = subgroup_size, byrow = TRUE)
    
    if (nrow(subgroups) <= 1) stop("É necessário mais de um subgrupo para realizar a análise.")
    
    return(subgroups)
  }
  
  # Análise de Capacidade
  output$capability_summary <- renderPrint({
    req(data(), input$capability_variable, input$lie, input$les, input$subgroup_size_capability, input$target)
    
    # Validação dos limites e target
    if (input$lie >= input$les) stop("O limite inferior (LIE) deve ser menor que o limite superior (LES).")
    if (is.na(input$target)) stop("Por favor, defina o valor alvo (Target).")
    if (input$target < input$lie || input$target > input$les) stop("O valor alvo (Target) deve estar entre o limite inferior (LIE) e o limite superior (LES).")
    
    # Criar subgrupos
    subgroups <- create_subgroups(data(), input$capability_variable, as.numeric(input$subgroup_size_capability))
    
    # Análise de capacidade
    qcc_obj <- qcc(subgroups, type = "xbar", nsigmas = 3, plot = FALSE)
    capability_analysis <- qcc::process.capability(qcc_obj, spec.limits = c(input$lie, input$les), target = input$target)
    
    # Imprimir toda a análise de capacidade
    print(capability_analysis)
  })
  
  
  
}
# Run app
shinyApp(ui, server)