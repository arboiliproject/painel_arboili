# ─────────────────────────────────────────────────────────────
# Pré-processamento de dados para o dashboard ArboILI
# Este script carrega e organiza dados de doenças notificadas
# e dados de busca por sintomas no Google Trends, antes da
# inicialização da aplicação Shiny.
# ─────────────────────────────────────────────────────────────

# ─────────────────────────────
# Carregar pacotes necessários
# ─────────────────────────────
require(tidyverse)   # Manipulação e visualização de dados
require(data.table)  # Leitura rápida de grandes volumes de dados
require(plotly)      # Visualizações interativas (utilizado no app)
require(DT)          # Tabelas interativas no Shiny

# ─────────────────────────────
# Carregar dados de notificações de doenças (ArboILI)
# ─────────────────────────────

# Leitura do banco principal de casos agregados por estado/semana
dis_ts <- fread("data/ArboILI_disease_table.csv") |> 
  rename(
    SE_SIN_PRI = ew_symptom_onset,       # Semana epidemiológica de início dos sintomas
    UF = location,                       # Sigla do estado (ou 'BR' para Brasil)
    casos_dengue = dengue_cases,
    casos_chik = chik_cases,
    casos_arbo = arbo_cases,             # Total de arboviroses
    casos_srag = sari_cases,             # Síndrome respiratória aguda grave
    casos_covid = covid_cases,
    casos_influenza = flu_cases,
    casos_outros_srag = other_sari_cases,
    casos_na_srag = na_sari_cases        # Casos sem especificação
  ) |>
  mutate(
    UF = ifelse(UF == "BR", "Brasil", UF)
  )

# ─────────────────────────────
# Criar tabela de correspondência entre sintomas em português e inglês
# ─────────────────────────────

# Lista de sintomas em português (como aparecerão no dashboard)
termos_pt <- c(
  "Artralgia", "Artrite", "Calafrio", "Cefaleia", "Chikungunya", "COVID-19",
  "Dengue", "Diarreia", "Disfunção cognitiva", "Dispneia", "Dor de garganta",
  "Dor no peito", "Dor retro-orbital", "Erupção cutânea", "Fadiga", "Febre",
  "Influenza/gripe", "Mialgia", "Náusea", "Perda ou alteração do paladar ou do olfato",
  "Rigidez muscular", "Tosse", "Vômito"
)

# Dicionário de mapeamento manual: português → termos originais do Google Trends (em inglês)
mapa_termos <- c(
  "Artralgia" = "Arthralgia",
  "Artrite" = "Arthritis",
  "Calafrio" = "Chills",
  "Cefaleia" = "Headache",
  "Chikungunya" = "Chikungunya",
  "COVID-19" = "COVID-19",
  "Dengue" = "Dengue",
  "Diarreia" = "Diarrhea",
  "Disfunção cognitiva" = "Cognitive dysfunction",
  "Dispneia" = "Dyspnea",
  "Dor de garganta" = "Sore throat",
  "Dor no peito" = "Chest pain",
  "Dor retro-orbital" = "Retro-orbital pain",
  "Erupção cutânea" = "Skin rash",
  "Fadiga" = "Fatigue",
  "Febre" = "Fever",
  "Influenza/gripe" = "Influenza",
  "Mialgia" = "Myalgia",
  "Náusea" = "Nausea",
  "Perda ou alteração do paladar ou do olfato" = "Loss of smell or taste",
  "Rigidez muscular" = "Muscle stiffness",
  "Tosse" = "Cough",
  "Vômito" = "Vomiting"
)

# Gerar tabela de correspondência entre termos
df_correspondencia <- data.frame(
  termo = termos_pt,
  topic = unname(mapa_termos[termos_pt])
)

# ─────────────────────────────
# Identificar o diretório mais recente com dados do Google Trends
# ─────────────────────────────

dirs <- dir("data/GoogleTrends", full.names = TRUE, recursive = FALSE)
last_dir <- dirs[str_detect(basename(dirs), "^\\d{4}_\\d{2}_\\d{2}$")] |>
  enframe(name = "date", value = "dirs") |>
  mutate(date = paste0(
    substr(dirs, 19, 22), "-",  # ano
    substr(dirs, 24, 25), "-",  # mês
    substr(dirs, 27, 28)        # dia
  ),
  date = as.Date(date, format = "%Y-%m-%d")) |>
  arrange(desc(date)) |>      # ordena da mais recente para a mais antiga
  slice(1) |>                 # seleciona o diretório mais recente
  select(dirs) |>
  unlist()

# ─────────────────────────────
# Carregar dados do Google Trends (séries temporais por sintoma)
# ─────────────────────────────

gt_ts <- fread(paste0(last_dir, "/GoogleTrends_search.csv")) |>
  rename(geo = location) |> 
  mutate(geo = ifelse(geo == "BR", "Brasil", geo)) |> 
  left_join(df_correspondencia, by = "topic") |> 
  drop_na(termo) |>             
  select(-topic)                

# ─────────────────────────────
# Carregar dados auxiliares de comparação
# ─────────────────────────────

gt_ts_compare <- fread("dados/gt_ts_compare.csv")        # Série temporal agregada para comparação
topic_comparison_tbl <- fread("dados/topic_comparison.csv") # Correlação entre sintomas e notificações

# ─────────────────────────────
# Carregar dados de "related topics" e "related queries"
# ─────────────────────────────

dir_path = "dados/"
related_files <- list.files(path = "data/GoogleTrends/related")
top_topic_files <- grep("top_topic.*.csv", related_files, value = TRUE)
top_query_files <- grep("top_query.*.csv", related_files, value = TRUE)

# Related Topics (tópicos mais associados nas buscas)
gt_results_topic <- data.frame()
for(f in top_topic_files) {
  gt_results_topic <- bind_rows(gt_results_topic, fread(paste0("data/GoogleTrends/related/", f)))
}

# Related Queries (buscas mais frequentes associadas aos termos)
gt_results_query <- data.frame()
for(f in top_query_files) {
  gt_results_query <- bind_rows(gt_results_query, fread(paste0("data/GoogleTrends/related/", f)))
}

# Extrai os anos únicos presentes nos dados de tópicos relacionados do Google Trends (coluna "time")
anos_related <- sort(unique(substr(gt_results_topic$time, 1, 4)))

# Extrai as palavras-chave únicas usadas nos dados de tópicos relacionados
keywords_related <- unique(gt_results_topic$keyword)

# Lê a tabela com o mapeamento de termos de busca em português
tbl_queries <- read_csv("dados/termos_buscas.csv")

# Lê os nomes e siglas das unidades federativas (UFs)
UFs <- read.csv("dados/UFs.csv")

# Lê os resultados de correlação entre buscas e séries temporais de doenças
correlation_results <- fread("dados/correlation_results.csv")

# Lê e prepara os dados de coordenadas geográficas de cada UF
br_coord <- read.csv("data/fed_units_coordinates.csv")

br_coord <- br_coord |> 
  mutate(Region = factor(Region, levels = c("Brasil","Norte","Nordeste","Centro-Oeste","Sudeste","Sul"),
                         ordered = TRUE))  |>
  arrange(Region, desc(Latitude)) |>
  mutate(br_order = 1:n()) |>
  select(FU, Abbrev, br_order)

# Lê a tabela com dados de quais sintomas são compartilhados por cada agravo
shared_symptom <- read.csv("data/shared_symptom.csv")

# Extrai os termos únicos utilizados nas buscas do Google Trends (em português)
termos_busca <- unique(gt_ts$termo)

# Identifica a data máxima disponível nos dados de doenças notificados (SE_SIN_PRI = semana epidemiológica)
max_date <- as.character(max(dis_ts$SE_SIN_PRI))

# Define o intervalo de datas coberto pelo dataset de comparação de buscas (usado para limitar gráficos temporais)
date_range_gt <- as.Date(range(gt_ts_compare$date))

# Lê a lista de termos populares (ex: sintomas), usada para destacar buscas relevantes
termos_populares <- read.csv("dados/termos_populares.csv")

# Filtra apenas os termos que devem ser considerados e que são codificados (têm "/" como separador, ex: "Cough/Febre")
symptom_codes = termos_populares %>%
  filter(!exclude) %>%                    # Remove os termos marcados para exclusão
  mutate(is_code = grepl("/", terms)) %>% # Verifica se o termo é um código composto
  filter(is_code) %>%                     # Mantém apenas os compostos
  select(terms, is_code)                  # Seleciona apenas as colunas necessárias

# Junta os códigos de sintomas ao dataset de tópicos relacionados do GT
gt_s <- gt_results_topic %>% 
  left_join(symptom_codes, by = c("topicId" = "terms")) %>%
  mutate(
    # Marca como código os termos encontrados ou se o título do tópico for literalmente "Symptom"
    is_code = ifelse(is.na(is_code), FALSE, is_code),
    is_code = ifelse(topicTitle == 'Symptom', TRUE, is_code)
  )

# Lê a tabela de valores mensais de epidemias (casos notificados por mês) e converte a coluna `month` para classe Date.
# Também cria uma coluna 'se' que replica a data para facilitar o merge posterior com séries temporais de buscas.
epi_tbl <- fread(file = "dados/epidemic_month.csv")  |>
  mutate(month = as.Date(month),
         se = month)

### UTILITÁRIOS ###

# Vetor nomeado de meses em português com seus respectivos números (usado para formatar datas)
meses <- c(
  "Janeiro" = "01",
  "Fevereiro" = "02",
  "Março" = "03",
  "Abril" = "04",
  "Maio" = "05",
  "Junho" = "06",
  "Julho" = "07",
  "Agosto" = "08",
  "Setembro" = "09",
  "Outubro" = "10",
  "Novembro" = "11",
  "Dezembro" = "12"
)

# Cria uma tabela com os tópicos analisados no estudo
# Essa tabela serve como base de referência para relacionar termos de doenças e sintomas,
# tanto em inglês quanto em português, com possíveis sinônimos ou variações populares de busca.

table_s1 <- data.frame(
  Topic_type = c(  # Classificação do tipo: "Disease" (doença) ou "Symptom" (sintoma)
    "Disease","Disease","Disease","Disease",
    "Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom",
    "Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom","Symptom"
  ),
  Topic = c(  # Nome do tópico principal em inglês (usado para cruzamentos com dados do GT ou Freebase)
    "Chikungunya","COVID-19","Dengue","Influenza/flu",
    "Alteration of smell or taste","Arthralgia","Arthritis","Chest pain","Chills","Cognitive dysfunction",
    "Cough","Diarrhea","Dyspnea","Fatigue","Fever",
    "Headache","Loss of smell","Loss of smell or taste","Loss of taste","Muscle stiffness",
    "Myalgia","Nausea","Retro-orbital pain","Skin rash","Sore throat","Vomiting"
  ),
  Topic_Portuguese = c(  # Tradução dos tópicos principais para o português (para exibição em dashboards ou relatórios)
    "Chikungunya","COVID-19","Dengue","Influenza/gripe",
    "Alteração de olfato ou paladar","Artralgia","Artrite","Dor no peito","Calafrio","Disfunção cognitiva",
    "Tosse","Diarreia","Dispneia","Fadiga","Febre",
    "Cefaleia","Perda de olfato","Perda de olfato ou paladar","Perda do paladar","Rigidez muscular",
    "Mialgia","Náusea","Dor retro-orbital","Erupção cutânea","Dor de garganta","Vômito"
  ),
  Freebase_ID = c(  # Identificador no Freebase (usado pelo Google Trends para diferenciar entidades)
    "/m/01__7l","*","/m/09wsg","/m/0cycc",
    "/m/01d3gn and /m/04czcv_","/m/021hck","/m/0t1t","/m/02np4v","*","*",
    "/m/01b_21","/m/0f3kl","/m/01cdt5","/m/01j6t0","/m/0cjf0",
    "/m/0j5fv","/m/0m7pl","/m/0m7pl and /m/05sfr2","/m/05sfr2","*",
    "/m/013677","/m/0gxb2","*","*","/m/0b76bty","/m/012qjw"
  ),
  Alternative_terms = c(  # Termos alternativos em português usados popularmente nas buscas, que podem ser mapeados para o mesmo tópico
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

