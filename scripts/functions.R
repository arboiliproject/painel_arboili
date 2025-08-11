# ─────────────────────────────────────────────────────────────
# Este script carrega a funções para funcionamento do dashboard
# ─────────────────────────────────────────────────────────────

#' plot_gt
#' 
#' Gera um gráfico interativo com série temporal combinando dados de busca do Google Trends (GT)
#' e notificações oficiais de doenças (SINAN/SIVEP) para uma localidade e doença específicas.
#' 
#' @param x Data frame contendo os dados combinados de GT e notificações, com colunas `SE_SIN_PRI`, `n` e `type`.
#' @param topic Nome do tópico buscado no Google Trends (ex: "Febre").
#' @param disease Nome da variável da doença (ex: "casos_dengue", "casos_srag").
#' @param geo Sigla da unidade geográfica (ex: "BR", "SP", "RJ").
#' @param UF Data frame contendo nomes completos das UFs (com colunas `SIGLA` e `NOME`).
#' @param show_bg Booleano. Se `TRUE`, adiciona faixas de epidemia ao fundo do gráfico (para dengue em UFs).
#' @param block_epi Data frame com períodos epidêmicos, contendo `inicio`, `fim`, `ymin`, `ymax` e `epidemia`.
#'
#' @return Objeto `plotly` com o gráfico gerado.

plot_gt <- function(x, topic, disease, geo, UF, show_bg, block_epi) { 
  
  # Determina o banco de origem da notificação
  base_name <- ifelse(any(grepl("dengue|chik|arbo", disease)), "SINAN", "SIVEP")  
  
  # Mapeia o nome da variável para um nome legível
  disease_name = case_when(
    disease == "casos_dengue"       ~ "Dengue",
    disease == "casos_chik"         ~ "Chikungunya",
    disease == "casos_arbo"         ~ "Dengue + Chikungunya",
    disease == "casos_srag"         ~ "SRAG",
    disease == "casos_covid"        ~ "COVID-19",
    disease == "casos_influenza"    ~ "Influenza",
    disease == "casos_outros_srag"  ~ "SRAG (Outros)",
    disease == "casos_na_srag"      ~ "SRAG (Sem Classificação)"
  )
  
  # Renomeia o tipo para exibição no gráfico
  x$type <- recode(x$type, 
                   "n" = paste0(disease_name, " (", base_name, ")"), 
                   "value" = paste0(topic, " (GT)"))
  
  # Cria objeto base do gráfico
  p <- plot_ly()
  
  # Define o intervalo do eixo X (datas)
  x_range <- range(x$SE_SIN_PRI, na.rm = TRUE)
  
  # Adiciona a série temporal com dois tipos: notificações e buscas
  p <- p %>%
    add_trace(
      data = x,
      x = ~SE_SIN_PRI, 
      y = ~n, 
      color = ~type, 
      colors = c('#1a80bb', '#ea801c'),  # azul e laranja
      type = 'scatter', 
      mode = 'lines',
      text = ~paste(type),   # Texto de hover
      hoverinfo = 'text',
      height = 400
    ) %>%
    layout(
      # Título do gráfico: nome da UF ou "Brasil"
      title = ifelse(geo != "Brasil", 
                     UF %>% filter(SIGLA == geo) %>% pull(NOME), 
                     "Brasil"),
      xaxis = list(title = "", showline = TRUE, linewidth = 1, linecolor = 'black', range = x_range),
      yaxis = list(title = "", showline = TRUE, linewidth = 1, linecolor = 'black', range = c(0, 110)),
      legend = list(
        title = list(text = ""),
        x = 1.05, y = 1
      ),
      showlegend = TRUE
    ) %>%
    # Ajusta legenda
    layout(
      legend = list(
        itemsizing = 'constant',
        traceorder = 'normal',
        font = list(size = 14),
        title = list(text = ''),
        x = 1.05, y = 1,
        xanchor = 'left'
      )
    )
  
  # Se for dengue e uma UF (não Brasil), adiciona faixas de epidemia ao fundo
  if (show_bg & disease == "casos_dengue" & geo != "Brasil") {
    for (i in seq_len(nrow(block_epi))) {
      bloco <- block_epi[i, ]
      
      # Adiciona uma forma preenchida (polígono) representando a faixa
      p <- add_trace(p,
                     type = "scatter", mode = "none", showlegend = FALSE,
                     x = c(bloco$inicio, bloco$fim, bloco$fim, bloco$inicio),
                     y = c(bloco$ymin, bloco$ymin, bloco$ymax, bloco$ymax),
                     fill = "toself",
                     fillcolor = if (bloco$epidemia) "lightblue" else "white",
                     hoverinfo = "none",
                     line = list(width = 0))
    }
    
    # Reforça o traçado da linha principal após desenhar os blocos
    p <- p %>%
      add_trace(
        data = x,
        x = ~SE_SIN_PRI, 
        y = ~n, 
        color = ~type, 
        colors = c('#1a80bb', '#ea801c'),
        type = 'scatter', 
        mode = 'lines',
        text = ~paste(type),
        hoverinfo = 'text',
        height = 400,
        showlegend = FALSE
      )
  }
  
  return(p)
}

#' xcf_plot
#'
#' Gera um gráfico interativo (via plotly) da função de correlação cruzada (CCF)
#' entre duas séries temporais, permitindo identificar lags com correlação significativa.
#'
#' @param x Vetor numérico com a série 1 (eixo X).
#' @param y Vetor numérico com a série 2 (eixo Y).
#' @param title Título do gráfico (default: "Cross Correlation").
#' @param height_value Altura do gráfico em pixels (default: 400).
#'
#' @return Gráfico interativo `plotly` com barras de correlação por defasagem (lag).

xcf_plot <- function(x, y, title = "Cross Correlation", height_value = 400){
  
  # Guarda os vetores de entrada (poderia incluir um ts() se necessário)
  df_x <- x
  df_y <- y
  
  # Calcula a função de correlação cruzada (sem plotar)
  ccf.object <- ccf(df_x, df_y, plot = FALSE)
  
  # Cria tabela com os lags e correlações, para plotar com ggplot
  output_table <- cbind(
    lag = ccf.object$lag,          # defasagem temporal
    x.corr = ccf.object$acf        # valor da correlação cruzada
  ) %>%
    as_tibble() %>%
    mutate(cat = ifelse(x.corr > 0, "green", "red"))  # Categorização visual (não usada diretamente)
  
  # Define o valor mínimo do eixo Y, limitando em -0.2 se não houver correlação negativa forte
  min_cor <- min(ccf(df_x, df_y, plot = FALSE)$acf)
  min_cor <- ifelse(min_cor < -0.2, min_cor, -0.2)
  
  # Gera gráfico de barras com ggplot
  output_table %>%
    ggplot(aes(
      x = lag, 
      y = x.corr, 
      text = paste0(
        "Correlation: ", round(x.corr, 2),
        "<br>Lag: ", lag)
    )) +
    geom_bar(stat = "identity", aes(fill = cat)) +  # Barras coloridas conforme sinal da correlação
    scale_fill_manual(values = c("steelblue", "#cc0000")) +  # Azul positivo, vermelho negativo
    ylab("Cross correlation") +
    scale_y_continuous(limits = c(-1, 1)) +  # Limite fixo para facilitar comparações
    theme_bw() + 
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10)
    ) +
    ggtitle(title) -> p  # Armazena em objeto p
  
  # Retorna gráfico como plotly interativo com tooltip customizado
  ggplotly(p, height = height_value, tooltip = "text")
}

#' match_time_series
#'
#' Prepara e cruza duas séries temporais — uma de notificações de doenças e outra de
#' buscas no Google Trends — para o mesmo local e termo/sintoma, com base em data.
#'
#' @param disease_data Dataframe contendo a série temporal de casos (ex: `dis_ts`)
#' @param disease_geo Local de interesse (UF ou "Brasil") para os dados de doença
#' @param disease_name Nome da coluna com o número de casos (ex: "casos_dengue")
#' @param trends_data Dataframe com as buscas do Google Trends (ex: `gt_ts`)
#' @param trends_geo Local de interesse para dados do Trends (UF ou "Brasil")
#' @param trends_name Nome do termo buscado (ex: "Febre", "Dengue")
#'
#' @return Dataframe contendo: data (SE_SIN_PRI), número de casos e valor de busca no GT

match_time_series <- function(disease_data, disease_geo = "Brasil", disease_name,
                              trends_data, trends_geo = "Brasil", trends_name) {
  
  # Filtra os dados do Google Trends para o local (geo) e termo (topic) desejados
  selected_gt_ts <- trends_data %>%
    filter(geo == trends_geo) %>%
    filter(termo == trends_name) %>%
    dplyr::select(date, value)  # Mantém somente colunas de interesse
  
  # Filtra os dados de doença para o local desejado
  # Seleciona a coluna SE_SIN_PRI (semana epidemiológica) e a variável de interesse
  match_tbl <- disease_data %>% 
    filter(UF == disease_geo) %>%
    dplyr::select(one_of(c("SE_SIN_PRI", disease_name))) %>%
    
    # Faz o join com os dados do GT, juntando por data
    left_join(selected_gt_ts, by = c("SE_SIN_PRI" = "date")) %>%
    
    # Remove linhas com valores ausentes
    drop_na()
  
  # Retorna o dataframe com as duas séries alinhadas no tempo
  return(match_tbl)
}

#' get_cc_metrics
#'
#' Calcula a correlação cruzada entre duas séries temporais pareadas (casos e buscas).
#' Retorna o valor máximo da correlação, o lag correspondente, e o p-valor aproximado.
#'
#' @param match_tbl Dataframe com as duas séries temporais já pareadas (ex: retorno da `match_time_series`)
#'
#' @return Um dataframe com 3 colunas:
#'   - max_correlation: valor máximo da correlação cruzada
#'   - lag: defasagem temporal associada a esse pico
#'   - p_value: significância estatística da correlação

get_cc_metrics <- function(match_tbl) {
  
  # Extrai as duas colunas de interesse do data frame
  ts1 <- match_tbl[, 2]  # Série temporal de casos
  ts2 <- match_tbl[, 3]  # Série temporal de buscas
  
  # Calcula a correlação cruzada (sem plotar)
  ccf_result <- ccf(ts1, ts2, plot = FALSE)
  
  # Identifica o índice da maior correlação
  ind.max <- which.max(ccf_result$acf)
  max.cor <- ccf_result$acf[ind.max]
  lag.opt <- ccf_result$lag[ind.max]
  
  # Calcula p-valor aproximado para a correlação observada
  p <- 2 * (1 - pnorm(abs(max.cor), mean = 0, sd = 1 / sqrt(ccf_result$n.used)))
  
  # Retorna como dataframe
  result <- data.frame(
    max_correlation = max.cor,
    lag = lag.opt,
    p_value = p
  )
  
  return(result)
}

#' plot_correlations
#'
#' Gera gráfico de barras com correlação máxima entre um termo de busca e diversas doenças
#' para uma localidade específica. Ideal para investigar um único estado ou o Brasil.
#'
#' @param data Dataframe com os resultados de correlação (ex: correlation_results)
#' @param nome_doenca Nome da variável de doença (ex: "casos_dengue")
#' @param geo_name Localidade (UF ou "Brasil")
#' @param titulo_doenca Nome que será exibido no título do gráfico (opcional)
#'
#' @return Objeto ggplot
plot_correlations = function(data, nome_doenca, geo_name, titulo_doenca = "") {
  
  dados = data %>%
    filter(doenca == nome_doenca & geo == geo_name) %>%
    replace_na(list(max_correlation = 0)) %>%
    mutate(
      lag_sign = ifelse(max_correlation < 0, -0.03, 0.03),  # Direção do deslocamento do label do lag
      sig = ifelse(p_value > 0.01, "p > 0.01", "p <= 0.01"), # Nível de significância
      sig = factor(sig, levels = c("p > 0.01", "p <= 0.01"))
    ) %>%
    arrange(desc(termo))  # Ordena termos no eixo y
  
  lag_s = dados$lag_sign
  
  g = dados %>%
    ggplot(aes(
      x = max_correlation, y = termo, fill = sig,
      text = paste0("Correlacao: ", round(max_correlation, 2),
                    "<br>Lag:", lag,
                    "<br>p-valor: ", format(round(p_value, 4), nsmall = 4))
    )) +
    geom_col() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkgrey") +
    geom_text(aes(label = lag), nudge_x = lag_s) +
    scale_fill_manual("Significancia", values = c("#F22300", "steelblue"), drop = FALSE) +
    scale_x_continuous(limits = c(-0.6, 1.2)) +
    geom_vline(xintercept = 0) +
    xlab("Correlacao máxima") +
    ylab("") +
    theme_minimal() +
    labs(title = paste0("Casos: ", titulo_doenca, " x Localidade: ", geo_name)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}
#' plot_correlations2
#'
#' Gera gráfico de barras com a correlação máxima entre uma doença e um único termo de busca,
#' comparando entre diferentes unidades federativas (UFs).
#'
#' @param data Dataframe com resultados de correlação
#' @param nome_doenca Nome da variável de doença (ex: "casos_dengue")
#' @param termo_nome Nome do termo buscado (ex: "Febre")
#' @param titulo_doenca Nome que será exibido no título do gráfico (opcional)
#'
#' @return Objeto ggplot
plot_correlations2 = function(data, nome_doenca, termo_nome, titulo_doenca = "") {
  
  dados = data %>%
    filter(doenca == nome_doenca & termo == termo_nome) %>%
    replace_na(list(max_correlation = 0)) %>%
    mutate(
      lag_sign = ifelse(max_correlation < 0, -0.03, 0.03),
      sig = ifelse(p_value > 0.01, "p > 0.01", "p <= 0.01"),
      sig = factor(sig, levels = c("p > 0.01", "p <= 0.01")),
      geo = factor(geo, levels = 
                     c(sort(c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", 
                              "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", 
                              "PR", "SC", "RS", "MS", "MT", "GO", "DF")),"Brasil"),
                   ordered = TRUE)
    )
  
  lag_s = dados$lag_sign
  
  g = dados %>%
    ggplot(aes(
      x = max_correlation, y = geo, fill = sig,
      text = paste0("Correlacao: ", round(max_correlation, 2),
                    "<br>Lag:", lag,
                    "<br>p-valor: ", format(round(p_value, 4), nsmall = 4))
    )) +
    geom_col() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkgrey") +
    geom_text(aes(label = lag), nudge_x = lag_s) +
    scale_fill_manual("Significancia", values = c("#F22300", "steelblue"), drop = FALSE) +
    scale_x_continuous(limits = c(-0.6, 1.2)) +
    geom_vline(xintercept = 0) +
    xlab("Correlacao máxima") +
    ylab("") +
    theme_minimal() +
    labs(title = paste0("Casos: ", titulo_doenca, " x Tópico de busca: ", termo_nome)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}

#' plot_correlations3
#'
#' Gera gráfico de barras comparando a correlação de diferentes doenças com um mesmo termo de busca
#' para uma única localidade.
#'
#' @param data Dataframe com resultados de correlação
#' @param geo_name Nome da UF ou "Brasil"
#' @param termo_nome Nome do termo buscado (ex: "Dor de cabeça")
#'
#' @return Objeto ggplot
plot_correlations3 = function(data, geo_name, termo_nome) {
  
  dados = data %>%
    filter(geo == geo_name & termo == termo_nome) %>%
    replace_na(list(max_correlation = 0)) %>%
    mutate(
      lag_sign = ifelse(max_correlation < 0, -0.03, 0.03),
      sig = ifelse(p_value > 0.01, "p > 0.01", "p <= 0.01"),
      sig = factor(sig, levels = c("p > 0.01", "p <= 0.01")),
      doenca = factor(doenca,
                      levels = rev(c("casos_dengue", "casos_chik", "casos_arbo",
                                     "casos_srag", "casos_covid", "casos_influenza",
                                     "casos_outros_srag", "casos_na_srag")),
                      labels = rev(c("Dengue", "Chikungunya", "Dengue + Chikungunya",
                                     "SRAG", "COVID-19", "Influenza", 
                                     "SRAG (Outros)", "SRAG (Sem Classificação)")),
                      ordered = TRUE)
    )
  
  lag_s = dados$lag_sign
  
  g = dados %>%
    ggplot(aes(
      x = max_correlation, y = doenca, fill = sig,
      text = paste0("Correlacao: ", round(max_correlation, 2),
                    "<br>Lag:", lag,
                    "<br>p-valor: ", format(round(p_value, 4), nsmall = 4))
    )) +
    geom_col() +
    geom_vline(xintercept = 1, linetype = "dashed", color = "darkgrey") +
    geom_text(aes(label = lag), nudge_x = lag_s) +
    scale_fill_manual("Significancia", values = c("#F22300", "steelblue"), drop = FALSE) +
    scale_x_continuous(limits = c(-0.6, 1.2)) +
    geom_vline(xintercept = 0) +
    xlab("Correlacao máxima") +
    ylab("") +
    theme_minimal() +
    labs(title = paste0("Localidade: ", geo_name, " x Tópico de busca: ", termo_nome)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}

#### Compare topics
#' plot_query_comparison
#'
#' Gera um gráfico interativo comparando os valores de interesse de vários termos relacionados 
#' a um mesmo tópico principal do Google Trends, para uma localidade e intervalo de tempo definidos.
#'
#' Essa função é útil para visualizar como diferentes variações de um mesmo termo (sinônimos, grafias, 
#' combinações) se comportam ao longo do tempo em um dado território. Ideal para validação e escolha 
#' do termo principal a ser utilizado nas análises.
#'
#' @param gt_data Dataframe contendo os dados do Google Trends já organizados, com colunas: `termo`, `date`, `geo`, `value`.
#' @param main_topic_ String com o nome do tópico principal (ex: "febre dengue").
#' @param geo_name Nome da localidade (ex: "Brasil", "SP", "CE", etc.). Default: "Brasil".
#' @param date_range Vetor de duas datas (`c(data_inicial, data_final)`) definindo o período a ser visualizado.
#' @param topic_label Dataframe com metadados dos termos associados ao tópico principal, com colunas:
#' - `termo`: nome do tópico principal
#' - `label`: nome amigável para exibição no gráfico
#' - `keyword2`: termo buscado originalmente (opcional, descartado aqui)
#'
#' @return Objeto `plotly` interativo, com linhas representando diferentes termos associados ao mesmo tópico.
#'
#' @details 
#' A função associa cada termo alternativo (sinônimo ou variação) a um grupo único por data, 
#' permitindo identificar visualmente o comportamento e a contribuição de cada termo.
#' 
#' @examples
#' plot_query_comparison(gt_data = gt_ts_compare,
#'                       main_topic_ = "febre dengue",
#'                       geo_name = "Brasil",
#'                       date_range = c(as.Date("2022-01-01"), as.Date("2022-12-31")),
#'                       topic_label = topic_metadata)

plot_query_comparison <- function(gt_data, main_topic_, geo_name = "Brasil", date_range, topic_label) {
  
  # Filtra os rótulos para o tópico principal e cria uma variável de agrupamento por linha
  topic_label_coded = topic_label %>%
    filter(termo == main_topic_) %>%
    dplyr::mutate(group = 1:n()) %>%
    select(-termo, -keyword2)
  
  # Filtra os dados do Google Trends no intervalo e localidade escolhidos, e associa os rótulos
  pdata = gt_data %>% 
    filter(date >= date_range[1] & date <= date_range[2]) %>%
    filter(geo == geo_name & termo == main_topic_) %>%
    group_by(date) %>%
    dplyr::mutate(group = 1:n()) %>%
    ungroup() %>%
    left_join(topic_label_coded, by= "group")
  
  # Cria gráfico interativo com linhas para cada termo alternativo
  g <- pdata %>%
    ggplot(aes(x= date, y = value, color = label)) +
    geom_line() +
    geom_point(size = 0.01, aes(text = paste0(label,": ",value,
                                              "<br>SE: ",format(date, format = "%d %b %Y")))) +
    theme_minimal(base_size = 16) +
    xlab("") + ylab("") +
    scale_color_discrete("Termo de busca") +
    theme(legend.position = "bottom")
  
  # Torna o gráfico interativo com ggplotly
  ggplotly(g, tooltip = "text", height = 600) %>%
    layout(legend = list(orientation = 'v', x = 0.45, y = -.6))
}

#' plot_top_results
#'
#' Gera um gráfico interativo com os principais resultados associados a um termo de busca 
#' do Google Trends, exibindo os tópicos relacionados ou pesquisas principais para um local, 
#' período e termo selecionados.
#'
#' Essa função permite explorar os **assuntos mais relevantes (topics)** ou **pesquisas mais populares (queries)** 
#' relacionadas a um termo específico, conforme os filtros definidos pelo usuário.
#'
#' @param data Dataframe com os dados de resultados do Google Trends (geralmente `gt_results_topic` ou `gt_results_query`), contendo colunas como `keyword`, `geo`, `time`, `value`, `topicTitle` ou `topSearches`.
#' @param time_filter Filtro de tempo. Pode ser uma data específica (ex: `"2023-10"`) ou `"Todos [ano]"` para agregação anual (ex: `"Todos 2023"`).
#' @param geo_filter Código ou nome da localidade (ex: `"Brasil"`, `"SP"`, `"CE"` etc.).
#' @param keyword_filter Termo de busca principal que originou os resultados.
#' @param plot_type Tipo de visualização: `"topic"` para assuntos relacionados (`topicTitle`) ou qualquer outro valor (ex: `"query"`) para pesquisas principais (`topSearches`).
#'
#' @return Um gráfico `plotly` interativo, com barras horizontais mostrando os principais tópicos ou pesquisas associadas.
#'
#' @details 
#' A função detecta automaticamente se deve agrupar os dados por ano ou usar um mês específico com base no `time_filter`. 
#' O gráfico gerado mostra a popularidade (valor agregado) de cada item, com tooltip interativo.
#'
#' @examples
#' plot_top_results(data = gt_results_topic,
#'                  time_filter = "Todos 2023",
#'                  geo_filter = "Brasil",
#'                  keyword_filter = "dengue",
#'                  plot_type = "topic")
#' 
#' plot_top_results(data = gt_results_query,
#'                  time_filter = "2023-06",
#'                  geo_filter = "RJ",
#'                  keyword_filter = "febre dengue",
#'                  plot_type = "query")

plot_top_results <- function(data, time_filter, geo_filter, keyword_filter, plot_type) {
  
  # Caso o gráfico seja do tipo 'topic'
  if(plot_type == "topic") {
    
    # Agrega por ano se o filtro de tempo for do tipo "Todos 202X"
    if(grepl("Todos", time_filter)) {
      data2 <- data %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        dplyr::mutate(ano = substr(time, 1, 4)) %>%
        filter(ano == substr(time_filter, 1 , 4)) %>%
        group_by(topicTitle) %>%
        summarise(value = sum(value, na.rm = TRUE))
    } else {
      # Filtra por mês específico
      data2 <- data %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        filter(time == time_filter)
    }
    
    # Cria gráfico para assuntos relacionados
    p <- data2 %>%
      filter(value != 0) %>%
      ggplot(aes(x = value, y = fct_rev(fct_reorder(topicTitle, desc(value))),
                 text = paste0("Assunto: ", topicTitle, "<br>Valor: ", value))) +
      geom_col(fill = '#0067C0') +
      scale_x_continuous(expand = c(0, 1)) +
      labs(title = "Assuntos relacionados") +
      theme_minimal(base_size = 12) +
      xlab("") + ylab("") +
      theme(axis.line.y = element_line(),
            panel.grid.major.y = element_blank(),
            plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), "inches"))
    
  } else {
    
    # Caso o gráfico seja do tipo 'query' (pesquisas populares)
    if(grepl("Todos", time_filter)) {
      data2 <- data %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        dplyr::mutate(ano = substr(time, 1, 4)) %>%
        filter(ano == substr(time_filter, 1 , 4)) %>%
        group_by(topSearches) %>%
        summarise(value = sum(value, na.rm = TRUE))
    } else {
      # Filtra por mês específico
      data2 <- data %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        filter(time == time_filter)
    }
    
    # Cria gráfico para pesquisas relacionadas
    p <- data2 %>%
      filter(value != 0) %>%
      ggplot(aes(x = value, y = fct_rev(fct_reorder(topSearches, desc(value))),
                 text = paste0("Pesquisa: ", topSearches, "<br>Valor: ", value))) +
      geom_col(fill = '#0067C0') +
      scale_x_continuous(expand = c(0, 1)) +
      labs(title = "Pesquisas relacionadas") +
      theme_minimal(base_size = 12) +
      xlab("") + ylab("") +
      theme(axis.line.y = element_line(),
            panel.grid.major.y = element_blank(),
            plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), "inches"))
    
  }
  
  # Torna o gráfico interativo com tooltip
  ggplotly(p, tooltip = 'text', height = 600)
}

#' get_table_top_results
#'
#' Gera uma tabela com os principais resultados relacionados a um termo de busca do Google Trends,
#' com base nos filtros de localidade, período e tipo de resultado.
#'
#' A função retorna uma tabela com os **assuntos mais relevantes (topics)** ou as **pesquisas mais populares (queries)** 
#' associadas ao termo principal, possibilitando uma visualização tabular dos dados que também podem ser utilizados 
#' para exportação, análise adicional ou exibição em dashboards.
#'
#' @param dados Dataframe com os dados brutos do Google Trends (ex: `gt_results_topic` ou `gt_results_query`), contendo colunas como `keyword`, `geo`, `time`, `value`, `topicTitle` ou `topSearches`.
#' @param time_filter Filtro de tempo no formato `"YYYY-MM"` para mês específico ou `"Todos YYYY"` para agregar resultados do ano inteiro.
#' @param geo_filter Código ou nome da localidade (ex: `"Brasil"`, `"SP"`, `"CE"`).
#' @param keyword_filter Termo de busca principal selecionado.
#' @param plot_type Tipo de resultado: `"topic"` para assuntos relacionados (`topicTitle`) ou outro valor para pesquisas populares (`topSearches`).
#'
#' @return Um `data.frame` com duas colunas: título do tópico/pesquisa e valor total. Os resultados são ordenados do maior para o menor valor.
#'
#' @details 
#' A função reconhece automaticamente se o filtro de tempo refere-se a um período anual ou mensal, 
#' agregando os valores conforme necessário. Resultados com valor igual a zero são descartados.
#'
#' @examples
#' # Para obter a tabela de tópicos mais buscados em 2023 para "dengue" no Brasil:
#' get_table_top_results(dados = gt_results_topic,
#'                       time_filter = "Todos 2023",
#'                       geo_filter = "Brasil",
#'                       keyword_filter = "dengue",
#'                       plot_type = "topic")
#'
#' # Para obter pesquisas relacionadas em junho de 2023:
#' get_table_top_results(dados = gt_results_query,
#'                       time_filter = "2023-06",
#'                       geo_filter = "RJ",
#'                       keyword_filter = "febre dengue",
#'                       plot_type = "query")
get_table_top_results <- function(dados, time_filter, geo_filter, keyword_filter, plot_type) {
  
  if(plot_type == "topic") {
    
    # Caso agregue por ano
    if(grepl("Todos", time_filter)) {
      data2 <- dados %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        dplyr::mutate(ano = substr(time, 1, 4)) %>%
        filter(ano == substr(time_filter, 1 , 4)) %>%
        group_by(topicTitle) %>%
        summarise(value = sum(value, na.rm = TRUE))
    } else {
      # Caso selecione um mês específico
      data2 <- dados %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        filter(time == time_filter)
    }
    
    # Geração da tabela para tópicos
    tb <- data2 %>%
      filter(value != 0) %>%
      dplyr::select(topicTitle, value) %>%
      dplyr::arrange(desc(value))
    
  } else {
    
    # Caso agregue por ano
    if(grepl("Todos", time_filter)) {
      data2 <- dados %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        dplyr::mutate(ano = substr(time, 1, 4)) %>%
        filter(ano == substr(time_filter, 1 , 4)) %>%
        group_by(topSearches) %>%
        summarise(value = sum(value, na.rm = TRUE))
    } else {
      # Caso selecione um mês específico
      data2 <- dados %>%
        filter(keyword == keyword_filter) %>%
        filter(geo == geo_filter) %>%
        filter(time == time_filter)
    }
    
    # Geração da tabela para pesquisas populares
    tb <- data2 %>%
      filter(value != 0) %>%
      dplyr::select(topSearches, value) %>%
      dplyr::arrange(desc(value))
  }
  
  return(tb)
}

#' plot_top_results_time
#'
#' Gera um gráfico de barras empilhadas com a evolução temporal dos principais **assuntos relacionados** a um termo de busca do Google Trends, agrupados por mês.
#'
#' A função plota dinamicamente a evolução dos valores mensais dos assuntos mais buscados (topics) para um termo específico e uma localidade. É possível ocultar assuntos genéricos rotulados como `"Other"`, geralmente representando entradas não classificadas.
#'
#' @param data Dataframe contendo os dados de tópicos relacionados (tipicamente retornados por `gtrendsR`), com colunas como `keyword`, `geo`, `time`, `value`, `topicTitle` e `is_code`.
#' @param geo_filter Código da localidade a ser filtrada (ex: `"Brasil"`, `"SP"`, `"RJ"`).
#' @param keyword_filter Termo de busca principal para filtrar os dados.
#' @param hide_others Lógico. Se `TRUE`, oculta os assuntos não codificados (classificados como `"Other"`). Padrão: `FALSE`.
#'
#' @return Um gráfico interativo (`ggplotly`) de barras empilhadas com a evolução mensal dos assuntos relacionados ao termo de busca.
#'
#' @details 
#' - A função converte o campo `time` (mês) em formato `Date`.
#' - Assuntos que não possuem `is_code == TRUE` são agrupados sob o rótulo `"Other"`.
#' - Caso `hide_others = TRUE`, o gráfico exclui a categoria `"Other"` e ordena os assuntos por frequência.
#'
#' @examples
#' # Exemplo de uso:
#' plot_top_results_time(data = gt_results_topic,
#'                       geo_filter = "Brasil",
#'                       keyword_filter = "dengue",
#'                       hide_others = TRUE)
#'
#' # Visualizando com a categoria 'Other' incluída:
#' plot_top_results_time(data = gt_results_topic,
#'                       geo_filter = "SP",
#'                       keyword_filter = "febre",
#'                       hide_others = FALSE)

plot_top_results_time <- function(data, geo_filter, keyword_filter, hide_others = FALSE) {
  
  # Filtra dados por termo e localidade
  data2 = data %>%
    filter(keyword == keyword_filter) %>%
    filter(geo == geo_filter) %>%
    mutate(time = as.Date(paste0(time,"-01")),  # transforma "YYYY-MM" em data
           topicTitle = ifelse(is_code, topicTitle, "Other"))  # marca como "Other" se não for código
  
  # Define rótulos ordenados
  plot_labels = sort(unique(data2$topicTitle))
  plot_labels <- plot_labels[plot_labels != "Other"]  # remove "Other" para ordenação
  
  # Oculta ou inclui a categoria "Other"
  if(hide_others) {
    data2 <- data2 %>%
      filter(topicTitle != "Other") %>%
      mutate(topicTitle = factor(topicTitle, levels = plot_labels, ordered = T))
    
  } else {
    data2 <- data2 %>%
      mutate(topicTitle = factor(topicTitle, levels = c(plot_labels, "Other"), ordered = T))
  }
  
  # Agrega valores por mês e assunto, plota gráfico
  p <- data2 %>%
    group_by(time, topicTitle) %>%
    summarise(values = sum(value)) %>%
    ggplot(aes(x = time, y = values, fill = topicTitle,
               text = paste0(topicTitle, "<br>Valor: ", values))) +
    geom_col() +
    scale_fill_discrete("Assunto relacionado") +
    scale_y_continuous(expand = c(0, 1)) +
    scale_x_date(date_breaks = "months", date_labels = "%b %y") +
    labs(title = "Assuntos relacionados") +
    theme_minimal(base_size = 12) +
    xlab("") + ylab("") +
    theme(axis.line.x = element_line(),
          panel.grid.major.x = element_blank(),
          plot.margin = unit(c(0.1, 0.3, 0.1, 0.1), "inches"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ggplotly(p, tooltip = 'text', height = 500)
}