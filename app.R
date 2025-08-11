library(shiny)
library(shinyWidgets)
library(bslib)

source("scripts/pre_load.R")
source("scripts/functions.R")

# Define UI for application that draws a histogram
ui <- page_navbar(title = "Projeto Arboili",
              id = "nav",
             # theme = shinythemes::shinytheme("cerulean"),
              fillable = TRUE,
    # Sidebar with a slider input for number of bins 
      sidebar = sidebar(
        width = 300,
        
        conditionalPanel(
          "input.nav == 1",
          
          selectInput(
            "doenca",
            "Escolha a doença:",
            choices = list("Dengue" = "casos_dengue", 
                           "Chikungunya" = "casos_chik",
                           "Dengue e Chikungunya" = "casos_arbo", 
                           "SRAG" = "casos_srag", 
                           "COVID-19" = "casos_covid", 
                           "Influenza" = "casos_influenza",
                           "SRAG (Outros)" = "casos_outros_srag",
                           "SRAG (Sem Classificação)" = "casos_na_srag"),
            selected = "Dengue"
          ),
          
          selectizeInput(
            "topic", 
            "Selecionar tópico de busca:",
            choices = termos_busca,
            selected = "Dengue"
          ),
          
          selectizeInput(
            "location", 
            "Selecionar localidade:",
            choices = c("Brasil",sort(UFs$SIGLA)),
            selected = "Brasil"
          ),
          
          dateRangeInput("daterange", "Datas:",
                         start  = min(gt_ts$date),
                         end    = max_date,
                         min    = min(dis_ts$SE_SIN_PRI),
                         max    = max_date,
                         format = "dd/mm/yy",
                         separator = " - "),
          
          downloadLink('downloadData', 'Salvar tabela')
        ), # Conditional panel 1
        
        conditionalPanel(
          "input.nav == 2",
          
      conditionalPanel(
        condition = "input.plotChoice == 'plot1'",
          
          selectInput(
            "doenca",
            "Escolha a doença:",
            choices = list("Dengue" = "casos_dengue", 
                           "Chikungunya" = "casos_chik",
                           "Dengue e Chikungunya" = "casos_arbo", 
                           "SRAG" = "casos_srag", 
                           "COVID-19" = "casos_covid", 
                           "Influenza" = "casos_influenza",
                           "SRAG (Outros)" = "casos_outros_srag",
                           "SRAG (Sem Classificação)" = "casos_na_srag"),
            selected = "Dengue"
          ),
          
          selectizeInput(
            "location", 
            "Selecionar localidade:",
            choices = c("Brasil",sort(UFs$SIGLA)),
            selected = "Brasil"
          )

        ), # Conditional panel 2.1
        
      conditionalPanel(
        condition = "input.plotChoice == 'plot2'",
          
          selectInput(
            "doenca",
            "Escolha a doença:",
            choices = list("Dengue" = "casos_dengue", 
                           "Chikungunya" = "casos_chik",
                           "Dengue e Chikungunya" = "casos_arbo", 
                           "SRAG" = "casos_srag", 
                           "COVID-19" = "casos_covid", 
                           "Influenza" = "casos_influenza",
                           "SRAG (Outros)" = "casos_outros_srag",
                           "SRAG (Sem Classificação)" = "casos_na_srag"),
            selected = "Dengue"
          ),
          
          selectizeInput(
            "topic", 
            "Selecionar tópico de busca:",
            choices = termos_busca,
            selected = "Dengue"
          )
          
        ), # Conditional panel 2.2
      
      conditionalPanel(
        condition = "input.plotChoice == 'plot3'",
        
        selectizeInput(
          "location", 
          "Selecionar localidade:",
          choices = c("Brasil",sort(UFs$SIGLA)),
          selected = "Brasil"
        ),
        
        selectizeInput(
          "topic", 
          "Selecionar tópico de busca:",
          choices = termos_busca,
          selected = "Dengue"
        )
        
      ), # Conditional panel 2.3
      
      #
      conditionalPanel(
        condition = "input.plotChoice == 'plot4'",
        
        selectInput(
          "doenca_cor",
          "Escolha a doença:",
          choices = list("Dengue" = "casos_dengue", 
                         "Chikungunya" = "casos_chik",
                         "COVID-19" = "casos_covid", 
                         "Influenza" = "casos_influenza"),
          selected = "Dengue"
        ),
        
        selectInput(
          "topic_cor", 
          "Selecionar conjunto de tópicos:",
          choices = c("Todos","Doenças","Sintomas","Sintomas específicos","Sintomas compartilhados","Sintomas de outras doenças"),
          selected = "Todos"
        ),
        
        sliderInput("slider_lag", label = "Filtre o intervalo de lag", min = -10, 
                    max = 10, value = c(-5, 0)),
        
        checkboxInput("cor_checkbox", label = "Mostrar gradiente de correlação", value = FALSE)
        
        
      ) # Conditional panel 2.4
      #
      
      ), # Conditional panel 2
      
      conditionalPanel(
        condition = "input.nav == 3",
      
        dateRangeInput("date_gt", "Datas:",
                       start  = date_range_gt[1],
                       end    = date_range_gt[2],
                       min    = date_range_gt[1],
                       max    = date_range_gt[2],
                       format = "dd/mm/yy",
                       separator = " - "),
        
      selectizeInput(
        "topic_gt", 
        "Selecionar tópico:",
        choices = sort(unique(gt_ts_compare$termo)),
        selected = "Alteração"
      ),
      
      selectizeInput(
        "location_gt", 
        "Selecionar localidade:",
        choices = c("Brasil",sort(UFs$SIGLA)),
        selected = "Brasil"
      )
    
      ),  # Conditional panel 3
      
      
      conditionalPanel(
        "input.nav == 4",
        
        selectInput(
          "plot_type",
          "Escolha a visualização:",
          choices = list("Assuntos relacionados" = "topic", 
                         "Pesquisas relacionadas" = "query"),
          selected = "Assuntos relacionados"
        ),              
        
        selectInput(
          "keyword_filter",
          "Escolha a doença:",
          choices = keywords_related,
          selected = "Dengue"
        ),
        
        selectizeInput(
          "geo_filter", 
          "Selecionar localidade:",
          choices = c("Brasil",sort(UFs$SIGLA)),
          selected = "Brasil"
        ),
        
        selectizeInput(
          "year_filter", 
          "Selecionar Ano:",
          choices = anos_related,
          selected = max(anos_related)
        ),
        
        selectizeInput(
          "month_filter", 
          "Selecionar Mês:",
          choices = c("Todos", meses),
          selected = "Todos"
        )
      )
      ), #Sidebar
    
    # Script para colapsar a sidebar nas abas 0 e 5
    tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleSidebar', function(state) {
      const sidebar = document.querySelector('.bslib-page-sidebar');
      if(sidebar){
        if(state === 'close'){
          sidebar.classList.add('collapsed');
        } else {
          sidebar.classList.remove('collapsed');
        }
      }
    });

    // Observa mudanças na aba ativa e envia mensagem para o Shiny
    $(document).on('shiny:inputchanged', function(event) {
      if(event.name === 'nav') {
        if(event.value === 0 || event.value === 5){
          Shiny.setInputValue('sidebar_state', 'close');
        } else {
          Shiny.setInputValue('sidebar_state', 'open');
        }
      }
    });
  ")),
    
    
    nav_panel(
      title = "Projeto Arboili",
      value = 0,
      fluidRow(
        column(12,
               h2(tags$strong("Projeto Arboili"), style = "text-align: center;"),  # Título centralizado
               
               # Imagem centralizada
               tags$img(src = "BR.png", height = "300px", width = "400px",
                        style = "display: block; margin-left: auto; margin-right: auto;"),
               
               br(),
               
               p("Arboviroses, como dengue e chikungunya, e síndromes respiratórias agudas grave (SRAG) representam importantes desafios para a saúde pública, devido à sua rápida disseminação e potencial de sobrecarga dos sistemas de saúde."),
               
               h5(tags$strong("Google Trends e Vigilância Epidemiológica")), # Título em negrito
               p("O Google Trends tem se mostrado uma ferramenta útil para vigilância epidemiológica, ao disponibilizar dados de frequências relativas de buscas na internet. No Brasil, estudos já demonstraram sua eficácia na previsão de influenza e dengue, especialmente por permitir o monitoramento de termos relacionados a sintomas e consultas populares."),
               
               p("Este dashboard foi desenvolvido para explorar o potencial do Google Trends no acompanhamento de arboviroses, síndromes respiratórias e possíveis novos surtos. Aqui, você poderá visualizar padrões de busca, identificar termos relacionados e avaliar tendências que podem apoiar ações de vigilância e resposta em saúde pública.")
        )
      )
    ),
    
    
    nav_panel('Comparação de Casos Notificados e Buscas do GoogleTrends',
              value = 1,
              navset_card_underline(
                title = "",
                nav_panel("Interesse ao Longo do Tempo",
                          
                          card(
                            height = 600,
                            full_screen = TRUE,
                            # Botão de informação (posicionado no canto superior direito do card)
                            tags$div(
                              style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                              actionLink("info_1a", label = NULL, icon = icon("info-circle"), 
                                         style = "color: #007BC2; font-size: 20px;")
                            ),
                            
                            checkboxInput(inputId = "show_epidemic",
                                          label = "Destacar meses epidêmicos", 
                                          FALSE),
                            
                            plotlyOutput("tsPlot"),
                            #div(style = "margin-bottom: -120px;"), 
                          )
                ),
                
                nav_panel("CCF",
                          card(
                            full_screen = TRUE,
                            height = "200px",
                            
                            # Botão de informação (posicionado no canto superior direito do card)
                            tags$div(
                              style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                              actionLink("info_1b", label = NULL, icon = icon("info-circle"), 
                                         style = "color: #007BC2; font-size: 20px;")
                            ),
                            
                            p(""),
                            
                            fluidRow(
                              column(width = 1),
                              column(width = 8,
                                     plotlyOutput("ccfPlot")
                              )
                            )
                          )
                ),
                
                nav_panel("Tabela",
                          card(
                            height = 800,
                            full_screen = TRUE,
                            
                            # Botão de informação (posicionado no canto superior direito do card)
                            tags$div(
                              style = "position: absolute; top: 10px; right: 25px; z-index: 10;",
                              actionLink("info_1c", label = NULL, icon = icon("info-circle"), 
                                         style = "color: #007BC2; font-size: 20px;")
                            ),
                            
                            p(""),
                            
                            fluidRow(
                              column(width = 1),
                              column(width = 8,
                                     DT::DTOutput("tabela")
                              )
                            )
                          )
                ) # Tabela
              ) # card_underline
    ), # NavPanel 1
    
    nav_panel(
      title = "Correlações entre Séries Temporais",
      value = 2,
      card(
        height = 800,
        full_screen = TRUE,
        
        # Botão de informação (posicionado no canto superior direito do card)
        tags$div(
          style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
          actionLink("info_corr", label = NULL, icon = icon("info-circle"), 
                     style = "color: #007BC2; font-size: 20px;")
        ),
        
        p(),
        
      radioGroupButtons(
        inputId = "plotChoice",
        choices = list("Casos X Localidades (comparar Termos no GTrends)" = "plot1", 
                       "Casos X Tópicos (comparar UFs)" = "plot2",
                       "Localidades X Tópicos (comparar Casos)" = "plot3",
                       "Localidades X Tópicos (Correlações)" = "plot4"),
        status = "primary"
      ),
      
      #Conditional panel to display the selected plot
      conditionalPanel(
        condition = "input.plotChoice == 'plot1'",
        p(),
        plotlyOutput("plot1", height = "850px")
      ),

      conditionalPanel(
        condition = "input.plotChoice == 'plot2'",
        p(),
        plotlyOutput("plot2", height = "650px")
      ),

      conditionalPanel(
        condition = "input.plotChoice == 'plot3'",
        p(),
        plotlyOutput("plot3", height = "650px")
      ),
      
      conditionalPanel(
        condition = "input.plotChoice == 'plot4'",
        p(),
        plotlyOutput("compare_correlations", height = "750px")
      )
      )
    ), #NavPanel 2
    
    nav_panel(title = "Comparação de buscas do Google Trends",
              value = 3,
              navset_card_underline(
                nav_panel(title = "Gráfico",
                          card(
                            height = 600,
                            full_screen = TRUE,
                            # Botão de informação (posicionado no canto superior direito do card)
                            tags$div(
                              style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                              actionLink("info_3a", label = NULL, icon = icon("info-circle"), 
                                         style = "color: #007BC2; font-size: 20px;")
                            ),
                            
                            p(""),
                            
                            fluidRow(
                              column(width = 1),
                              column(width = 10,
                                     plotlyOutput("plot_topic_comparison")
                              ) # column
                            ) # fluidRow
                          ) # card
                          
                ), #nav_panel
                
                nav_panel(title = "Tabela de termos alternativos",
                          card(
                            height = 600,
                            full_screen = TRUE,
                            # Botão de informação (posicionado no canto superior direito do card)
                            tags$div(
                              style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                              actionLink("info_3b", label = NULL, icon = icon("info-circle"), 
                                         style = "color: #007BC2; font-size: 20px;")
                            ),
                            
                            p(""),
                            
                            dataTableOutput("tabela_s1"),
                            
                            tags$p("* Código FreebaseID inexistente para este tópico", 
                                   style = "font-size: 14px; margin: 2 0 0 0;"),
                            tags$p("** Termos alternativos não foram incluídos para este tópico", 
                                   style = "font-size: 14px; margin: 0;")
                          ) # card
                ) # nav_panel
              ) # navset_card_underline
    ),  #NavPanel 3
    
    
    nav_panel(
      title = "Principais buscas",
      value = 4,
      
      navset_card_tab(title = "",
                   #   height = 650,
                      full_screen = TRUE,
                      nav_panel(
                        "Gráfico",
                        card(
                        height = 750,
                        
                        tags$div(
                          style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                          actionLink("info_4a", label = NULL, icon = icon("info-circle"), 
                                     style = "color: #007BC2; font-size: 15px;")
                        ),
                        p(),
                        
                        fluidRow(
                          column(width = 1),
                          column(width = 8,
                                 plotlyOutput("plot_top_related")
                          )
                        )
                        )
                      ), 
                      
                      nav_panel(
                        "Tabela",
                        card(
                          height = 750,
                          
                          tags$div(
                            style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                            actionLink("info_4b", label = NULL, icon = icon("info-circle"), 
                                       style = "color: #007BC2; font-size: 15px;")
                          ),
                          p(),
                          
                        fluidRow(
                          column(width = 1),
                          column(width = 10,
                                 DT::DTOutput("table_top_related")
                          )
                        )
                        )
                      ),
                      
                      nav_panel(
                        "Plot mensal",
                        card(
                          height = 750,
                          
                          tags$div(
                            style = "position: absolute; top: 10px; right: 15px; z-index: 10;",
                            actionLink("info_4c", label = NULL, icon = icon("info-circle"), 
                                       style = "color: #007BC2; font-size: 15px;")
                          ),
                          p(),
                          
                        fluidRow(
                          column(width = 1),
                          column(width = 10,
                                 checkboxInput(inputId = "hide_others",
                                               label = "Ocultar outras buscas", 
                                               FALSE),
                                 plotlyOutput("plot_top_related_time")
                          )
                        )
                        )
                      ),
      )
    ),
    
    nav_panel(
      title = "Equipe",
      value = 6,  # Use um valor que ainda não tenha sido usado
      fluidRow(
        column(12,
               
               # Pessoa 2
               fluidRow(
                 column(3,
                        tags$img(src = "perfil_CC.gif", width = "40%", style = "border-radius: 10px;")
                 ),
                 column(9,
                        tags$h4("Cláudia T. Codeço"),
                        tags$p("PROCC/Fiocruz"),
                        tags$p("Coordenadora do Projeto"),
                        tags$a(href = "http://lattes.cnpq.br/1929576902623348", 
                               target = "_blank", "Currículo Lattes")
                 )
               ),
               tags$hr(),
               
               # Pessoa 1
               fluidRow(
                 column(3,
                        tags$img(src = "perfil_AA.jpeg", width = "40%", style = "border-radius: 10px;")
                 ),
                 column(9,
                        tags$h4("Alexandra R. M. Almeida"),
                        tags$p("PROCC/Fiocruz"),
                        tags$p("Coordenadora Adjunta do Projeto"),
                        tags$a(href = "http://lattes.cnpq.br/8167370958071286", 
                               target = "_blank", "Currículo Lattes")
                 )
               ),
               tags$hr(),
               
               # Pessoa 3
               fluidRow(
                 column(3,
                        tags$img(src = "perfil_MEB.jpeg", width = "40%", style = "border-radius: 10px;")
                 ),
                 column(9,
                        tags$h4("Marcelo E. Borges"),
                        tags$p("PROCC/Fiocruz"),
                        tags$p("Pesquisador/desenvolvedor do dashboard"),
                        tags$a(href = "http://lattes.cnpq.br/4504018862767268", 
                               target = "_blank", "Currículo Lattes")
                 )
               ),
               tags$hr(),
               
               # Pessoa 4
               fluidRow(
                 column(3,
                        tags$img(src = "perfil_DMBO.gif", width = "40%", style = "border-radius: 10px;")
                 ),
                 column(9,
                        tags$h4("Dalila M. B. Oliveira"),
                        tags$p("Instituto Oswaldo Cruz, Fiocruz"),
                        tags$p("Pesquisadora"),
                        tags$a(href = "http://lattes.cnpq.br/9746873482395128", 
                               target = "_blank", "Currículo Lattes")
                 )
               )
        )
      )
    ),

    nav_panel(
      title = "Sobre",
      fluidRow(
        column(12, 
               h4("Fontes de Dados"),
               tags$ul(
                 tags$li("Os dados de contagem de casos de Dengue e Chikungunya foram obtidos do SINAN."),
                 tags$li("Os dados para SRAGs foram obtidos do SIVEP.")
               ),
               h4("Índice de Popularidade"),
               p("Os números de busca no Google Trends não representam os números absolutos do volume de pesquisa, pois os dados são normalizados e apresentados em uma escala de 0-100. Cada ponto no gráfico é dividido pelo ponto mais alto de popularidade durante o período selecionado. Para os dados agregados do Brasil, realiza-se a soma das contagens de todas as Unidades Federativas."),
               
               h4("Detalhes dos Casos Reportados"),
               p("O número de casos mostrado no gráfico também não representa o número absoluto de casos. Ele é um valor normalizado com base no valor máximo do Índice de Popularidade para o período e local selecionados, permitindo a comparação de tendências de busca para os termos escolhidos."),
               p("Os números de casos reportados estão agregados pela data de início da semana epidemiológica, com base na data dos primeiros sintomas."),
               
               h4("Regras de Classificação para Dados do SINAN e SRAG"),
               tags$ul(
                 tags$li("Para casos do SINAN, foram removidos casos em que a Classificação Final é igual a 5 (Descartado)."),
                 tags$li(strong("SRAG: "), "Total de casos."),
                 tags$li(strong("COVID-19: "), "Classificação Final igual a 5, ou resultado de PCR ou Antígeno positivos para COVID-19."),
                 tags$li(strong("Influenza: "), "Classificação Final igual a 1, ou resultado de PCR ou Antígeno positivos para Influenza."),
                 tags$li(strong("SRAG (Outros): "), "Classificação Final igual a 2, 3 ou 4."),
                 tags$li(strong("SRAG (Sem Classificação): "), "Classificação Final não preenchida (NA).")
               )
        )
      )
      
    ) # NavPanel 5
     
) #ui


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rt_doenca <- reactive({
    rtd = epi_tbl |>  filter(location == input$location)
  })
  
  epidemia_blocos <- reactive({ 

    rt_doenca() %>%
      rename(epidemia = epi_m3) |>
      select(se, epidemia) %>%
      arrange(se) %>%
      mutate(grupo = cumsum(epidemia != lag(epidemia, default = first(epidemia)))) %>%
      #group_by(ano, grupo, epidemia) %>%
      group_by(grupo, epidemia) %>%
      summarise(inicio = min(se), fim = max(se), .groups = "drop") |>
      ungroup() |>
      mutate(fim = fim + 30,
             ymin = 0,
             ymax = 100)
    
  })
  
  
  ts_table <- reactive({
    
    req(input$doenca, input$topic, cancelOutput = T)
    
    columns_dis <- c("SE_SIN_PRI", "UF", input$doenca)
    
    topic_regex <- paste0("^",input$topic,"$")
    
    ts_tbl <- dis_ts %>%
      select(any_of(columns_dis)) %>%
      filter(UF == input$location)
      
    gt_tbl <- gt_ts %>%
      filter(geo == input$location & grepl(topic_regex, termo))
      #filter(geo == "Brasil" & grepl("^Dengue$", termo))
    
    dis_gt_ts <- ts_tbl %>%
      left_join(gt_tbl, by = c("SE_SIN_PRI" = "date", "UF" = "geo")) %>%
      rename(n = input$doenca) %>% 
      select(-UF, -termo)
    
    dis_gt_ts
  })
  
  ts_table_export <- reactive({
    
    dis_gt_ts_export <- ts_table() %>%
      filter(SE_SIN_PRI >= input$daterange[1] & SE_SIN_PRI <= input$daterange[2]) %>%
      arrange(desc(SE_SIN_PRI))
    
    dis_gt_ts_export
  })
  
  ts_table_date <- reactive({
    
    dis_gt_ts_date <- ts_table_export() %>%
      mutate(n = n/max(n, na.rm = TRUE)*max(value, na.rm = TRUE)) %>%
      pivot_longer(cols = c("n","value"), names_to = "type", values_to = "n")
    
    dis_gt_ts_date
  })
  
  output$tabela <- DT::renderDT(
    datatable(ts_table_export(),
    colnames = c("Semana Epidemiológica", "Casos", "Índice de Interesse"),
    options = list(pageLength = 12,
                   dom = 'tipr'))
    )
  
  correlation_results_dt <- reactive({correlation_results %>%
      filter(doenca == input$doenca) %>%
      filter(termo  == input$topic)  %>% 
      mutate(max_correlation  = round(max_correlation , 3),
               p_value  = round(max_correlation, 4),
               )
    })
  
  output$tabela2 <- DT::renderDT(
    datatable(correlation_results_dt(),
      colnames = c("Doenca", "termo", "UF", "Correlacao maxima", "Lag", "p valor"),
    options = list(pageLength = 28,
                   dom = 'tipr'))
  )
  
  output$tsPlot <- renderPlotly({

  plot_gt(x = ts_table_date(), 
              topic = input$topic, 
              disease = input$doenca, 
              geo = input$location,
              UF = UFs,
              show_bg = input$show_epidemic, 
              block_epi = epidemia_blocos())
    
    })
  
  output$ccfPlot <- renderPlotly({

    match_tbl <- match_time_series(disease_data = dis_ts,
                                   trends_data = gt_ts,
                                   disease_name = input$doenca,
                                   trends_name = input$topic,
                                   disease_geo = input$location,
                                   trends_geo = input$location)
    
    match_tbl <- match_tbl %>% filter(SE_SIN_PRI >= input$daterange[1] & SE_SIN_PRI <= input$daterange[2]) 
    
    ts1 <- match_tbl[,2]
    ts2 <- match_tbl[,3]
  
    xcf_plot(x = ts1, y = ts2, title = "", height_value = 470)
  })

  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      topic_title <- stringi::stri_trans_general(str = tolower(gsub(" ", "_", input$topic)), 
                         id = "Latin-ASCII")
      paste('data_', input$doenca,"_",topic_title,"_",input$location, '.csv', sep='')
    },
    
    content = function(con) {
      export_table_final <- ts_table_export()
      colnames(export_table_final) <- c("semana_epidemiologica","casos","indice_popularidade")
      export_table_final <- export_table_final %>% 
        mutate(doenca = input$doenca,
               topico = input$topic,
               localizacao = input$location)
      
      fwrite(export_table_final, con)
    }
  )
  
  output$plot1 <- renderPlotly({
    
    titulo_doenca = switch(input$doenca, "casos_dengue" = "Dengue", 
           "casos_chik" = "Chikungunya",
           "casos_arbo" = "Dengue e Chikungunya", 
           "casos_srag" = "SRAG", 
           "casos_covid" = "COVID-19", 
           "casos_influenza" = "Influenza",
           "casos_outros_srag" = "SRAG (Outros)",
           "casos_na_srag" = "SRAG (Sem Classificação)")
    
    ggplotly(plot_correlations(data = correlation_results, nome_doenca = input$doenca, 
                      geo_name = input$location, titulo_doenca = titulo_doenca), tooltip = "text")
  })
 
  output$plot2 <- renderPlotly({
    
    titulo_doenca = switch(input$doenca, "casos_dengue" = "Dengue", 
                           "casos_chik" = "Chikungunya",
                           "casos_arbo" = "Dengue e Chikungunya", 
                           "casos_srag" = "SRAG", 
                           "casos_covid" = "COVID-19", 
                           "casos_influenza" = "Influenza",
                           "casos_outros_srag" = "SRAG (Outros)",
                           "casos_na_srag" = "SRAG (Sem Classificação)")
    
    ggplotly(plot_correlations2(data = correlation_results, nome_doenca = input$doenca, 
                       termo_nome = input$topic, titulo_doenca = titulo_doenca), tooltip = "text")
  })

  output$plot3 <- renderPlotly({
    
    ggplotly(plot_correlations3(data = correlation_results, geo_name = input$location, 
                       termo_nome = input$topic), tooltip = "text")
  })
  
  output$plot_topic_comparison <- renderPlotly({
    
    ggplotly(plot_query_comparison(gt_data = gt_ts_compare, 
                                   main_topic_ = input$topic_gt, 
                                   geo_name = input$location_gt,
                                   date_range = input$date_gt,
                                   topic_label = topic_comparison_tbl))
    
  })
  
  output$plotUI <- renderUI({
    if (input$plotChoice == "plot1") {
      plotlyOutput("plot1", height = "850px")
    } else if (input$plotChoice == "plot2") {
      plotlyOutput("plot2", height = "650px")
    } else if (input$plotChoice == "plot3") {
      plotlyOutput("plot3", height = "650px")
    }
  })
  
  output$plot_top_related <- renderPlotly({
    
    if(input$plot_type == "topic") { 
    top_related_data <- gt_results_topic
    } else {
      top_related_data <- gt_results_query
    }
    
    if(input$geo_filter == "Brasil") {
      geo_filter_value <- "BR"
      } else {
        geo_filter_value <- paste0("BR-",input$geo_filter)
      }
    
    time_filter_value = paste0(input$year_filter, "-", input$month_filter)
    
    plot_top_results(data = top_related_data, 
                     time_filter = time_filter_value, 
                     geo_filter = geo_filter_value, 
                     keyword_filter = input$keyword_filter,
                     plot_type = input$plot_type) 

  })
  
  
table_top_results_tbl <-  reactive({
  
    if(input$plot_type == "topic") { 
      top_related_data <- gt_results_topic
    } else {
      top_related_data <- gt_results_query
    }
    
    if(input$geo_filter == "Brasil") {
      geo_filter_value <- "BR"
    } else {
      geo_filter_value <- paste0("BR-",input$geo_filter)
    }
    
    time_filter_value = paste0(input$year_filter, "-", input$month_filter)
    
    get_table_top_results(dados = top_related_data, 
                      time_filter = time_filter_value, 
                      geo_filter = geo_filter_value, 
                      keyword_filter = input$keyword_filter,
                      plot_type = input$plot_type)
  })
  
column_name <-  reactive({
  if(input$plot_type == "topic") { 
    cname <- "Assuntos relacionados"
  } else {
    cname <- "Pesquisas relacionadas"
  }
  cname
})
  
  output$table_top_related <- DT::renderDT({
    
    datatable(data = table_top_results_tbl(),
    colnames = c(column_name(), "Valor"),
    options = list(pageLength = 12,
                  dom = 'tipr'))
    
  })

  output$plot_top_related_time <- renderPlotly({
    
    if(input$geo_filter == "Brasil") {
      geo_filter_value <- "BR"
    } else {
      geo_filter_value <- paste0("BR-",input$geo_filter)
    }
    
    plot_top_results_time(data = gt_s, 
                     geo_filter = geo_filter_value, 
                     keyword_filter = input$keyword_filter,
                     hide_others = input$hide_others)
  })
 
  observeEvent(input$info_1a, {
    showModal(modalDialog(
      title = "Informações:",
      p(strong("Interesse ao longo do tempo:")),
      p("Mostra a relação entre as séries temporais de casos notificados para arboviroses ou SRAG e o índice de interesse por buscas no Google Trends, conforme o 'tópico', localidade e período selecionados."),
      
      p("O índice do Google Trends é uma métrica normalizada que varia de 0 a 100, onde 100 representa o pico de popularidade do termo no período e local escolhidos. Um valor de 50 indica metade desse interesse, enquanto 0 corresponde a volume muito baixo ou inexistente de buscas."),
      
      p("Os números de casos também foram normalizados, com o objetivo de permitir uma comparação direta entre as tendências das duas séries, facilitando a identificação de padrões de comportamento e possíveis associações temporais."),
      
      p(strong("Destacar meses epidêmicos (somente dengue):"), 
        " ao ativar esta opção, o gráfico exibe quais meses foram classificados como epidêmicos com base nos dados de incidência de dengue em cada Unidade Federativa (UF). 
  A definição utiliza a média móvel central de 3 semanas — considerando uma semana anterior e uma posterior — aplicada à série de incidência semanal. 
  Um mês é considerado epidêmico se ao menos 3 semanas dentro dele apresentarem valores da média móvel acima do limiar epidêmico,
  calculado de acordo com o método ", em("Moving Epidemic Method (MEM)"), 
        ". Considera somente datas após o ano 2020. Esse método é descrito em detalhes na seguinte referência:"),
      
      p(tags$a(href = "https://pubmed.ncbi.nlm.nih.gov/40549823/", 
               "Moving Epidemic Method (MEM) para detecção de períodos epidêmicos – veja estudo publicado no European Journal of Public Health (2023)", 
               target = "_blank")),
      
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_1b, {
    showModal(modalDialog(
      title = "Informações:",
      p(strong("CCF:")),
      p("Gráfico de Cross-correlation function (CCF), ou Função de Correlação Cruzada, entre as séries temporais de casos notificados da doença selecionada e o índice de interesse por buscas no Google Trends para o tópico escolhido. A CCF avalia a correlação entre as duas séries ao longo de diferentes defasagens (lags), permitindo identificar se o interesse por buscas antecede, coincide ou segue as notificações de casos. Esta análise considera todo o período disponível (últimos 5 anos), possibilitando verificar padrões históricos na relação entre buscas e casos."),
      p(strong("Interpretação dos lags:"), " valores de lag negativos indicam que o interesse por buscas precede os casos notificados; lag igual a zero indica correlação simultânea; e valores positivos indicam que os casos precedem o aumento no interesse por buscas."),
      
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_1c, {
    showModal(modalDialog(
      title = "Informações:",
      p(strong("Tabela:")),
      p("Apresenta os valores tabulados de interesse ao longo do tempo, assim como o número de casos por semana epidemiológica, conforme o 'tópico', localidade e período selecionados."),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_corr, {
    showModal(modalDialog(
      title = "Informações:",
      p("Os gráficos acima mostram, para cada comparação, a barra representando o valor da correlação máxima (em valor absoluto) e, no rótulo, o lag (defasagem temporal) no qual essa correlação é encontrada. O p-valor associado a essa correlação é calculado por meio de um teste de Pearson entre as duas séries ajustadas pelo lag identificado. Esses gráficos permitem avaliar quais pares de variáveis apresentam maior associação entre si e em que momento essa relação ocorre no tempo. São apresentadas três perspectivas de análise:"),
      tags$ul(
        tags$li(strong("Casos X Localidades:"), " compara diferentes tópicos do Google Trends (eixo Y) em relação aos casos da doença selecionada."),
        tags$li(strong("Casos X Tópicos:"), " compara diferentes Unidades Federativas (UFs) e o Brasil (eixo Y) em relação ao mesmo tópico de busca."),
        tags$li(strong("Localidades X Tópicos:"), " compara casos de diferentes doenças (eixo Y) em relação ao mesmo tópico de busca e localidade selecionados."),
        tags$li(strong("Localidades X Tópicos: (correlação)"), " compara casos de diferentes doenças (eixo Y) em relação ao conjunto de tópicos de busca escolhidos (doenças, todos os sintomas, sintomas específicos da doença, sintomas compartilhados com as outras doenças, ou sintomas de outras doenças. O mapa de calor mostra o valor de maior correlação e defasagem associada.")
      ),
      p(strong("Interpretação:"), 
        " barras mais altas indicam maior correlação (em valor absoluto), enquanto o lag informa o momento em que essa relação é mais forte. 
  Lags negativos sugerem que o interesse por buscas antecede os casos notificados; lag igual a zero indica correlação simultânea; 
  e lags positivos sugerem que os casos precedem o aumento no interesse por buscas."),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_3a, {
    showModal(modalDialog(
      title = "Informações:",
      p(strong("Interesse ao longo do tempo:")),
      p("Os gráficos abaixo apresentam a comparação dos resultados de interesse de buscas no Google Trends para o tópico selecionado. Essa comparação pode ser feita de duas formas: considerando apenas o termo específico (utilizando seus códigos de Freebase ID, quando disponíveis) ou incluindo todos os termos associados ao tópico, mostrados na categoria 'Todos', que abrange variações de nomes populares e relacionados."),
      p("O Freebase ID é um identificador único atribuído a um conceito ou entidade na antiga base de conhecimento Freebase (mantida pelo Google), utilizado pelo Google Trends para agrupar diferentes termos que se referem ao mesmo tópico, independentemente da forma como são escritos ou traduzidos."),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_3b, {
    showModal(modalDialog(
      title = "Informações:",
      tags$div(
        p(strong("Lista detalhada de tópicos, termos de busca, códigos Freebase ID e suas variações utilizados neste dashboard.")),
        p("Esta tabela apresenta os tópicos monitorados (como doenças e sintomas), seus nomes em português, os códigos ", 
          strong("Freebase ID"), " correspondentes (quando disponíveis) e os termos alternativos ou populares relacionados, que também são considerados nas consultas do Google Trends."),
        tags$ul(
          tags$li(em("O código Freebase ID é um identificador único utilizado pelo Google Trends para agrupar diferentes termos que se referem ao mesmo conceito, independentemente da forma como são escritos ou traduzidos.")),
          tags$li(em("Os termos alternativos listados incluem variações linguísticas e expressões coloquiais comumente usadas pela população, ampliando a cobertura das buscas analisadas."))
        )
      ),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  
  observeEvent(input$info_4a, {
    showModal(modalDialog(
      title = "Informações:",
      tags$div(
        p(strong("O gráfico acima apresenta os 'Related Topics' (Tópicos Relacionados) fornecidos pelo Google Trends, de acordo com os parâmetros selecionados (doença, localidade, ano e mês).")),
        
        p("Esses tópicos são divididos em duas categorias:"),
        
        tags$ul(
          tags$li(strong("Top Topics (Assuntos relacionados): "), 
                  "representam conceitos ou entidades que o Google agrupa como semanticamente ligados ao tópico principal pesquisado."),
          tags$li(strong("Top Queries (Pesquisas relacionadas): "), 
                  "são termos ou frases específicos que os usuários também buscaram em conjunto com o tópico principal.")
        ),
        
        p("No gráfico, o eixo ", strong("Y"), " exibe os tópicos ou termos relacionados, enquanto o eixo ", strong("X"), 
          " mostra o índice de interesse (de 0 a 100), calculado pelo Google com base no volume relativo de buscas naquele mês e local."),
        
        p(em("'Top' no contexto do Google Trends"), 
          " refere-se aos tópicos ou buscas com maior volume de pesquisa relacionados ao tema escolhido, em um determinado período e região. 
     Esse índice é normalizado para que 100 represente o pico de popularidade dentro do conjunto de dados analisado."),
        
        p("Essa visualização permite identificar padrões de associação e temas emergentes de interesse na população, 
     auxiliando na compreensão do comportamento de busca relacionado à vigilância em saúde.")
      ),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_4b, {
    showModal(modalDialog(
      title = "Informações:",
      tags$div(
        p(strong("A tabela acima apresenta os 'Related Topics' (Tópicos Relacionados) fornecidos pelo Google Trends, de acordo com os parâmetros selecionados: doença, localidade, ano e mês.")),
        
        p("Esses dados são divididos em duas categorias principais:"),
        
        tags$ul(
          tags$li(strong("Top Topics (Assuntos relacionados): "), 
                  "conceitos ou entidades que o Google identifica como semanticamente relacionados ao tópico principal buscado."),
          tags$li(strong("Top Queries (Pesquisas relacionadas): "), 
                  "termos ou frases que os usuários também pesquisaram em associação ao tópico principal.")
        ),
        
        p("A tabela possui duas colunas:"),
        tags$ul(
          tags$li(strong("Assunto ou Pesquisa relacionada:"), " mostra o nome do tópico ou termo relacionado."),
          tags$li(strong("Valor:"), " exibe o índice de interesse atribuído pelo Google, em uma escala de 0 a 100.")
        ),
        
        p(em("'Top' no contexto do Google Trends"), 
          " refere-se aos assuntos ou pesquisas que tiveram maior volume de buscas em relação ao tópico selecionado naquele período e região."),
        
        p("Esse índice é normalizado, ou seja, ", strong("100"), " representa o pico de popularidade dentro do conjunto de dados, 
     enquanto valores menores indicam interesse proporcionalmente menor."),
        
        p("Essas informações ajudam a identificar os principais temas ou sintomas que despertaram interesse público em determinados momentos, 
     apoiando estratégias de comunicação em saúde e vigilância epidemiológica.")
      ),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  observeEvent(input$info_4c, {
    showModal(modalDialog(
      title = "Informações:",
      tags$div(
        p(strong("O gráfico apresenta a evolução mensal do índice de interesse de busca para os 'Top Topics' (Assuntos Relacionados) ao longo do tempo, de acordo com a doença, localidade e intervalo selecionados.")),
        
        p("No eixo ", strong("X"), " está representado o tempo (por mês), e no eixo ", strong("Y"), " os valores do índice de busca, normalizados em uma escala de 0 a 100."),
        
        p("Os tópicos relacionados são classificados em três categorias principais:"),
        tags$ul(
          tags$li(strong("Sintomas específicos:"), " termos que representam manifestações clínicas diretamente associadas à doença selecionada."),
          tags$li(strong("Sintoma:"), " quando o próprio termo genérico 'sintoma' aparece como tópico de busca relevante."),
          tags$li(strong("Outros assuntos:"), " incluem tópicos diversos relacionados à doença, como medidas preventivas, tratamentos, instituições de saúde ou dúvidas frequentes.")
        ),
        
        p("A opção ", strong("‘Ocultar outras buscas’"), " permite filtrar visualmente o gráfico, removendo a categoria de ", em("outros assuntos"), 
          ", de modo a facilitar a análise das tendências de interesse apenas nos sintomas ou no termo 'sintoma'."),
        
        p("Essa visualização possibilita compreender como o interesse do público por diferentes aspectos da doença se comporta ao longo do tempo, 
     e pode revelar padrões importantes para a comunicação em saúde e a vigilância epidemiológica.")
      ),
      tags$style(HTML(".modal-dialog { max-width: 1200px; }")), # define a largura
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  output$tabela_s1 <- renderDT({
    
    tabela_s1 <- data.frame(
      Topic_type = c(
        "Doença","Doença","Doença","Doença",
        "Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma",
        "Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma","Sintoma"
      ),
      Topic_Portuguese = c(
        "Chikungunya","COVID-19","Dengue","Influenza/gripe",
        "Alteração de olfato ou paladar","Artralgia","Artrite","Dor no peito","Calafrio","Disfunção cognitiva",
        "Tosse","Diarreia","Dispneia","Fadiga","Febre",
        "Cefaleia","Perda de olfato","Perda de olfato ou paladar","Perda do paladar","Rigidez muscular",
        "Mialgia","Náusea","Dor retro-orbital","Erupção cutânea","Dor de garganta","Vômito"
      ),
      Freebase_ID = c(
        "/m/01__7l","*","/m/09wsg","/m/0cycc",
        "/m/01d3gn and /m/04czcv_","/m/021hck","/m/0t1t","/m/02np4v","*","*",
        "/m/01b_21","/m/0f3kl","/m/01cdt5","/m/01j6t0","/m/0cjf0",
        "/m/0j5fv","/m/0m7pl","/m/0m7pl and /m/05sfr2","/m/05sfr2","*",
        "/m/013677","/m/0gxb2","*","*","/m/0b76bty","/m/012qjw"
      ),
      Alternative_terms = c(
        "**",
        "COVID-19, covid-19, covid, sarscov2, covid 19, covid19",
        "**",
        "**",
        "boca amarga",
        "dor na junta, dor no corpo, dor nas articulações, dor na munheca, dor articular, quebradeira, derrubada, dor nos ossos",
        "inchaço nas juntas, inchaço nas dobras, inchaço, inchaço articular",
        "aflição no peito, pressão no peito, aperto no peito",
        "tremedeira, arrepio, frio",
        "abobado, lesado, tontura, leseira, confuso, pensamento lento, abestado, leso, abobalhado, disfunção cognitiva",
        "tosse, pigarro",
        "caganeira, dor de barriga, desarranjo, intestino solto, piriri",
        "falta de ar, dificuldade de respirar, sem ar",
        "cansaço, moleza, fraqueza, corpo mole, piema",
        "corpo esquentando, suadeira, corpo quente",
        "dor de cabeça, enxaqueca",
        "**",
        "não sentir gosto e cheiro",
        "**",
        "braços ou pernas endurecidas, braço endurecido, braços endurecidos, perna endurecida, pernas endurecidas, juntas duras, corpo travado, entrevado, dificuldade em movimentar",
        "dor no corpo, dor muscular, dor nas pernas",
        "estômago embrulhado, enjôo, estômago ruim, tonteira, vontade de vomitar, sensação de cabeça girando",
        "dor ocular, dor atrás dos olhos, dor no olho, vista doendo, olhos ardidos",
        "calombos na pele, mancha na pele, caroço, coceira, perebas, brotoeja, ferida na pele, irritação, bolha no corpo, pipoco de pele, furunco, carnegão, pintas, vermelhidão, erupção cutânea",
        "garganta doída",
        "**"
      ),
      stringsAsFactors = FALSE
    )
    
    datatable(
      data = tabela_s1,
      colnames = c("Categoria", "Assunto", "Freebase ID", "Termos alternativos"),
      options = list(
        pageLength = 9,
        dom = 'tipfr',   # 'f' adiciona a caixa de busca
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros por página",
          info = "Mostrando de _START_ até _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 registros",
          infoFiltered = "(filtrado de _MAX_ registros no total)",
          paginate = list("previous" = "Anterior", "next" = "Próximo"),
          zeroRecords = "Nenhum registro encontrado"
        )
      )
    )
  })
  
  cor_tab <- reactive({
    
    column_names_dis <- c("Dengue" = "casos_dengue", 
                          "Chikungunya" = "casos_chik",
                          "COVID.19" = "casos_covid", 
                          "Influenza" = "casos_influenza")
    
    col_title_dis = names(column_names_dis[column_names_dis == input$doenca_cor])
    
    shared_symptom_dis <- shared_symptom |>
      rename(topic_code = col_title_dis) |>
      select(topic, topic_code) |> 
      arrange(desc(topic_code), topic) |>
      mutate(topic_order = 1:n())
    
    cor_tab = correlation_results |>
      filter(doenca == input$doenca_cor) |>
      mutate(
        original_correlation = round(max_correlation, 2),
        max_correlation = ifelse(lag > input$slider_lag[2] | lag <= input$slider_lag[1], 0, original_correlation),
        cor_interval = cut(max_correlation,
                           right = FALSE,
                           breaks = c(-1.0, -0.5,  0.0, 0.5,  1.0),
                           #breaks = c(-1.0, -0.5,  0.0, 0.01,  0.5,  1.0),
                           # breaks = seq(-1, 1, by = 0.5),  # Breaks at intervals of 0.2
                           include.lowest = TRUE,           # Include -1 in the first interval
                           ordered_result = TRUE
        ),
        comma_pos = regexpr(",", cor_interval),
        cor_value = as.numeric(substr(cor_interval, 2, comma_pos-1))) |>
      left_join(br_coord, by = c("geo" = "Abbrev")) |>
      left_join(shared_symptom_dis, by = c("termo" = "topic"))
    
    dis_code_selection = switch(input$topic_cor,
                                "Doenças" = 3,
                                "Sintomas" = c(1,2),
                                "Sintomas específicos" = 1,
                                "Sintomas compartilhados" = 2,
                                "Sintomas de outras doenças" = 0)
    
    if(input$topic_cor == "Todos") {
      cor_tab_filtered <- cor_tab  %>%
        mutate(cor_interval_display = ifelse(is.na(cor_value) | is.nan(cor_value), -99, cor_value),
               max_correlation = ifelse(is.na(max_correlation) | is.nan(max_correlation), -99, max_correlation))
      
    } else {
      cor_tab_filtered <- cor_tab |>
        filter(topic_code %in% dis_code_selection)  %>%
        mutate(cor_interval_display = ifelse(is.na(cor_value) | is.nan(cor_value), -99, cor_value),
               max_correlation = ifelse(is.na(max_correlation) | is.nan(max_correlation), -99, max_correlation))
      
    }
    
    cor_tab_filtered <- cor_tab_filtered  %>%
      mutate(lag = ifelse(is.na(lag), "ausente", paste(lag, "semanas")),
             original_correlation = as.character(original_correlation)) |>
      mutate(cor_interval_display = ifelse(is.na(cor_value) | is.nan(cor_value), -99, cor_value),
             max_correlation = ifelse(is.na(max_correlation) | is.nan(max_correlation), -99, max_correlation)) |>
      complete(termo, geo, fill = list(lag = "ausente", original_correlation = "ausente"))
    
    cor_tab_filtered
    
  })
  
  output$compare_correlations <- renderPlotly({
    
    divergent_palette <- colorRampPalette(c("red", "white", "blue"))
    
    if(input$cor_checkbox) {
      
      plot_ly(
        data = cor_tab(),
        x = ~reorder(termo, topic_order),
        y = ~fct_rev(reorder(FU, br_order)),
        z = ~max_correlation,
        customdata = ~ map2(original_correlation, lag, list),  # Mapeia as colunas 'lag' e 'outpt' em customdata
        type = 'heatmap',
        colors =  c("black", divergent_palette(100)),  # Preto para valores especiais, seguido da escala
        zmin = -1,  # Valor mínimo da escala
        zmax = 1,   # Valor máximo da escala
        colorbar = list(title = "Correlação máxima"),
        showscale = TRUE,
        hovertemplate = paste0(
          "UF: %{y}<br>",
          "Assunto: %{x}<br>",
          "Correlação máxima: %{customdata[0]}<br>",
          "Lag: %{customdata[1]}<extra></extra>"
        )
      ) %>%
        layout(
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = ""),
          plot_bgcolor = "rgba(0, 0, 0, 0)", # Fundo transparente
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        )  %>%
        #   # Atualizar o mapa de cores para o intervalo especial (-99 = preto)
        colorbar(list(
          tickvals = c(-99, -1, 0, 1),  # Inclui o valor especial e os extremos da escala
          ticktext = c("N/A", "-1", "0", "1")
        ))
      
    } else {
      
      plot_ly(
        data = cor_tab(),
        x = ~reorder(termo, topic_order),
        y = ~fct_rev(reorder(FU, br_order)),
        z = ~cor_interval_display,
        customdata = ~ map2(original_correlation, lag, list),  # Mapeia as colunas 'lag' e 'outpt' em customdata
        type = 'heatmap',
        colors = c("black", divergent_palette(100)),  # Preto para valores especiais, seguido da escala
        zmin = -1,  # Valor mínimo da escala
        zmax = 1,   # Valor máximo da escala
        colorbar = list(title = "Correlação máxima"),
        showscale = TRUE,
        hovertemplate = paste0(
          "UF: %{y}<br>",
          "Assunto: %{x}<br>",
          "Correlação máxima: %{customdata[0]}<br>",
          "Lag: %{customdata[1]}<extra></extra>"
        )
      ) %>%
        layout(
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = ""),
          plot_bgcolor = "rgba(0, 0, 0, 0)", # Fundo transparente
          paper_bgcolor = "rgba(0, 0, 0, 0)"
        )  %>%
        #   # Atualizar o mapa de cores para o intervalo especial (-99 = preto)
        colorbar(list(
          tickvals = c(-99, -1, 0, 1),  # Inclui o valor especial e os extremos da escala
          ticktext = c("N/A", "-1", "0", "1")
        ))
    }
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
