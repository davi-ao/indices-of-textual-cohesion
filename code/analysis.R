# ------------------------------------------------------------------------------
# Analyze indices of textual cohesion of text using semantic networks of cliques
# Analisar índices de coesão textual utilizando redes semânticas de cliques
# 
#
# Authors: Davi Alves Oliveira, Valter de Senna, and Hernane Borges de Barros 
# Pereira
# Autores: Davi Alves Oliveira, Valter de Senna, e Hernane Borges de Barros 
# Pereira 
#
# Last update: September 01, 2023
# Última atualização: 01/09/2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install the necessary packages
# Instalar pacotes necessários
# install.packages('tidyverse')
# install.packages('SnowballC')
# install.packages('wordnet')

# Load required packages
# Carregar pacotes necessários
library(tidyverse)
library(SnowballC)
library(wordnet)

# Set Wordnet English dictionary
# Configurar o dicionário de inglês do Wordnet
setDict('data/wn3.1.dict/dict/')

# Process OANC XML files (selected from https://anc.org/)
# Processar arquivos XML do OANC (selecionados de https://anc.org/)
# ------------------------------------------------------------------------------
# Inform directory with the OANC XML files
# Informar diretório com os arquivos XML do OANC
oanc_dir = 'data/oanc_xml/'

# Read OANC XML files
# Ler arquivos XML do OANC
oanc_data = lapply(paste0(oanc_dir, list.files(oanc_dir)), function(f) {
  read_file(f) %>% 
    # Extract the children of <fs> elements
    # Extrair of filhos dos elementos <fs>
    str_extract_all('(?<=<fs>)(.|[\\n\\r])*?(?=<\\/fs>)') %>%
    unlist() %>%
    # Remove extra white spaces
    # Remover espaços extras
    str_remove_all('[\\n\\r]') %>%
    str_replace_all('\\s{2,}', ' ') %>%
    str_trim() %>%
    # Get values from the "name" and "value" attributes
    # Isolar valores dos atributos "name" e "value"
    str_match_all('name="(\\S+)" value="(\\S+)"') %>%
    # Replace sentence ending characters with the period sing
    # Substituir os caracteres de conclusão de períodos por pontos
    sapply(function(fs) {
      if (any(fs[,3] %>% str_detect('[.:?!…]'))) {
        '.'
      } else {
        paste0(fs[,2], '_', fs[,3], collapse = '|')
      }
    }) %>%
    # Convert the text into a single string
    # Converter o texto em um string único
    paste0(collapse = ' ') %>%
    # Split the text using the period sign as delimiter
    # Separa o texto utilizando o ponto como delimitador
    str_split_1(' . ') %>%
    # Convert the string vector into a tibble column
    # Converter o vetor de strings em uma coluna de tabela
    tibble(
      # Identify genre from file name
      # Identificar gênero textual a partir do nome do arquivo
      genre = f %>% str_match('oanc_xml/([a-z]+)_') %>% .[,2],
      text = f, 
      token = .) %>%
    # Attribute an ID number to each sentence
    # Atribuir um número identificador (ID) para cada sentença
    mutate(sentence_id = row_number()) %>%
    # Split sentences into tokens
    # Separar sentenças em tokens
    separate_rows(token, sep = ' ') %>%
    # Attribute an ID number to each token
    # Atribuir um número de identificação (ID) para cada token
    mutate(token_id = row_number()) %>%
    # Separate token and Part-Of-Speech (POS) tags
    # Separar tokens e etiquetas de categorias gramaticais (POS)
    separate_rows(token, sep = '\\|') %>% 
    separate(token, c('name', 'value'), '_') %>%
    pivot_wider(id_cols = c(genre, text, sentence_id, token_id), 
                names_from = name, 
                values_from = value) %>%
    select(genre, text, sentence_id, base, msd, affix) %>%
    rename(clique_id = sentence_id, lemma = base, pos = msd) %>%
    # Get lemma stems
    # Identificar o radical dos lemas
    mutate(stem = wordStem(lemma)) %>%
    # Keep only content words
    # Manter apenas as palavras lexicais
    filter(
      pos %in% c('CD', 'FW', 'JJ', 'JJR', 'JJS', 'NN', 'NNS', 'NNP', 'NNPS',
                 'RB', 'RBR', 'RBS', 'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ'))
}) %>%
  bind_rows()

# Write OANC processed data to a CSV file
# Salvar dados processados do OANC em um arquivo CSV
# write_csv(oanc_data, 'data/oanc_data.csv')
# ------------------------------------------------------------------------------

# Get synonyms and hypernyms from Wordnet
# Identificar sinônimos e hiperônimos usando o Wordnet
# ------------------------------------------------------------------------------
# Get unique lemmas with POS tags
# Identificar os lemas únicos com suas etiquetas POS
lemmas = oanc_data %>%
  distinct(lemma, pos)

# Create tibble with synonyms and hypernyms
# Criar tabela com sinônimos e hiperônimos
synonyms_hypernyms = mapply(function(lemma, pos) {
  pos_wordnet = NA
  
  # Map POS tags in the corpus with the correspondent POS tags in Wordnet
  # Mapeia as etiquetas POS no corpus com as etiquetas POS no Wordnet
  if(pos %in% c('VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')) {
    pos_wordnet = 'VERB'
  } else if (pos %in% c('NN', 'NNS', 'NNP', 'NNPS')) {
    pos_wordnet = 'NOUN'
  } else if (pos %in% c('ADJ', 'JJ', 'JJR', 'JJS')) {
    pos_wordnet = 'ADJECTIVE'
  } else if (pos %in% c('ADV', 'RB', 'RBR', 'RBS')) {
    pos_wordnet = 'ADVERB'
  }
  
  if (is.na(pos_wordnet)) {
    return(NA)
  } else {
    tibble(lemma = lemma,
           pos = pos,
           # Get synonyms from Wordnet
           # Identificar sinônimos usando o Wordnet
           synonyms = synonyms(lemma, pos_wordnet) %>%
             paste(collapse = '|') %>%
             str_to_lower(),
           # Get hypernyms from Wordnet
           # Identificar hiperônimos usando o Wordnet
           hypernyms = tryCatch({
             filter = getTermFilter('ExactMatchFilter', lemma, T)
             terms = getIndexTerms(pos_wordnet, 1, filter)
             synsets = getSynsets(terms[[1]])
             
             sapply(synsets, function(s) {
               relatedSynsets = getRelatedSynsets(s, '@')
               
               sapply(relatedSynsets, getWord)
             }) %>%
               unlist() %>%
               unique() %>%
               paste0(collapse = '|') %>%
               str_to_lower()
           }, error = function(e) {
             return(NULL)
           }))
  }
}, lemmas$lemma, lemmas$pos, SIMPLIFY = F) %>% 
  unname() %>%
  .[!is.na(.)] %>%
  bind_rows()

# Write lemmas with their synonyms and hypernyms to a CSV file
# Salvar lemas com seus sinônimos e hiperônimos em um arquivo CSV
# write_csv(synonyms_hypernyms, 'data/synonyms_hypernyms.csv')
# ------------------------------------------------------------------------------

# Generate pseudotexts
# Gerar pseudotextos
# ------------------------------------------------------------------------------
# To use the same pseudotexts reported in the study, uncomment and run the code 
# in line 184 instead of running lines 188-215
# Para usar os mesmo pseudotextos reportados no estudo, descomente e rode o 
# código na linha 184 ao invés de rodar as linhas 188-215
# pseudotexts = read_csv('data/oanc_pseudotexts_study.csv')

# Get the number of texts in the corpus
# Identificar o número de textos no corpus
corpus_size = oanc_data$text %>% unique() %>% length()

# Generate pseudotexts
# Gerar pseudotextos
pseudotexts = lapply(1:corpus_size, function(i) {
  oanc_data %>%
    # Attribute an ID number to each clique in the corpus
    # Atribuir um número de identificação (ID) para cada clique no corpus
    unite('text_clique_id', text:clique_id, remove = F) %>%
    filter(text_clique_id %in% (
      # Get a random sample of cliques, one per text
      # Selecionar uma amostra aleatória de cliques, uma por texto
      oanc_data %>%
        group_by(text) %>%
        sample_n(1) %>%
        unite('text_clique_id', text:clique_id) %>%
        .$text_clique_id)) %>%
    group_by(text_clique_id) %>%
    # Attribute an ID number to each pseudotexts and reset clique_id
    # Atribuir um número de identificação (ID) para cada pseudotexto e 
    # reconfigurar a coluna clique_id
    mutate(genre = 'pseudotext',
           text = paste0('pseudotext_', str_pad(i, 2, 'left', '0')),
           clique_id = cur_group_id()) %>%
    ungroup() %>%
    select(-text_clique_id)
}) %>%
  bind_rows()

# Write pseudotexts to a CSV file
# Salvar pseudotextos em um arquivo CSV
# write_csv(pseudotexts, 'data/oanc_pseudotexts.csv')
# ------------------------------------------------------------------------------

# Combine the data
# Combinar os dados
# ------------------------------------------------------------------------------
data = oanc_data %>% 
  bind_rows(pseudotexts) %>%
  left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
  group_by(text, clique_id) %>%
  # Keep only the unique stems in each clique
  # Manter apenas os radicais únicos em cada clique
  distinct(stem, .keep_all = T) %>%
  ungroup() %>%
  # Remove synonym data not considered in the analysis
  # Remover dados, desconsiderados na análise, dos sinônimos
  mutate(synonyms = synonyms %>% str_remove_all('\\(a\\)|\\(p\\)')) %>%
  # Attribute an ID number to each clique in the data
  # Atribuir um número de identificação (ID) para cada clique nos dados
  unite('text_clique_id', text:clique_id, remove = F)

# Write the combined data to a CSV file
# Salvar os dados combinados em um arquivo CSV
# write_csv(data, 'data/data.csv')
#-------------------------------------------------------------------------------

# Calculate the cohesion indices
# Calcular os índices de coesão
# ------------------------------------------------------------------------------
# Global Backward Cohesion
# Coesão Regressiva Global
global_backward_cohesion = lapply(
  # Iterate each clique in the data
  # Iterar cada clique nos dados
  (data %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    # Select the clique rows
    # Selecionar as linhas da clique
    q_i = data %>%
      filter(text_clique_id == t_q_id)
    # Identify the genre
    # Identificar o gênero textual
    genre = q_i$genre %>% unique()
    # Identify the text ID
    # Identificar o ID do texto
    t = q_i$text %>% unique()
    # Identify the clique ID
    # Identificar o ID do clique
    q = q_i$clique_id %>% unique()
    # Get the cliques added before the current clique
    # Selecionar as cliques adicionadas antes da clique atual
    G_i = data %>% filter(text == t & clique_id < q)
    
    # Calculate the indices
    # Calcular os índices
    tibble(
      genre = genre,
      text = t,
      clique_id = q,
      # Calculate the number of repeated vertices
      # Calcular o número de vértices repetidos
      r_i = intersect(q_i %>% .$stem, G_i %>% .$stem) %>% length(),
      # Calculate the number of cohesion edges
      # Calcular o número de arestas coesivas
      m_i = (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma) %in% 
        (c(G_i %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           G_i %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      # Get the number of vertices in the clique
      # Calcular o número de vértices na clique
      n_q_i = q_i %>% nrow(),
      # Get the number of vertices in G_i
      # Calcular o número de vértices em G_i
      n_iprev = G_i %>% nrow(),
      # Calculate vertex index
      # Calcular o índice de vértice
      v = ifelse(
        n_iprev == 0,
        0,
        ifelse(
          n_q_i > 0 && r_i < n_q_i && r_i < n_iprev,
          r_i/n_q_i,
          1
        )),
      # Calculate edge index
      # Calcular o índice de aresta
      e = ifelse(
        n_q_i > 0 && n_iprev > 0 && r_i < n_q_i && r_i < n_iprev, 
        m_i/((n_q_i - r_i)*(n_iprev - r_i)), 
        0)
    )
  }) %>%
  bind_rows()

# Local Backward Cohesion
# Coesão Regressiva Local
local_backward_cohesion = lapply(
  # Iterate each clique in the data
  # Iterar cada clique nos dados
  (data %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    data_q_position = data %>%
      # Ensure correct clique position ID
      # Garantir um número de identificação (ID) correto para a posição das
      # cliques
      group_by(text, clique_id) %>%
      mutate(clique_position = cur_group_id()) %>%
      ungroup()
    
    # Get the clique rows
    # Selecionar as linhas da clique
    q_i = data_q_position %>%
      filter(text_clique_id == t_q_id)
    # Get the genre
    # Identificar o gênero textual
    genre = q_i$genre %>% unique()
    # Get the text ID
    # Identificar o ID do texto
    t = q_i$text %>% unique()
    # Get the clique ID
    # Identificar o ID da clique
    q = q_i$clique_id %>% unique()
    # Get the clique position
    # Identificar a posição da clique
    q_position = q_i$clique_position %>% unique()
    # Get clique q_{i-1}
    # Selecionar a clique q_{i-1}
    q_j = data_q_position %>%
      filter(text == t & clique_position == q_position - 1)
    
    # Calculate the indices
    # Calcular os índices
    tibble(
      genre = genre,
      text = t,
      clique_id = q,
      # Calculate the number of repeated vertices
      # Calcular o número de vértices repetidos
      r_i = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
      # Calculate the number of cohesion edges
      # Calcular o número de aretas coesivas
      m_i = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
           q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>% 
           unique()) %>%
        sum(),
      # Get the number of vertices in the clique
      # Calcular o número de vértices na clique
      n_q_i = q_i %>% nrow(),
      # Get the number of vertices in q_{i-1}
      # Calcular o número de vértices in q_{i-1}
      n_q_i_iprev = q_j %>% nrow(),
      # Calculate vertex index
      # Calcular o índice de vértice
      v = ifelse(
        n_q_i_iprev == 0,
        0,
        ifelse(
          n_q_i > 0 && r_i < n_q_i && r_i < n_q_i_iprev,
          r_i/n_q_i,
          1
        )),
      # Calculate edge index
      # Calcular o índice de aresta
      e = ifelse(
        n_q_i > 0 && n_q_i_iprev > 0 && r_i < n_q_i && r_i < n_q_i_iprev, 
        m_i/((n_q_i - r_i)*(n_q_i_iprev - r_i)), 
        0)
    )
  }) %>%
  bind_rows()

# Mean Pairwise Cohesion
# Coesão Média Pareada
mean_pairwise_cohesion_temp = lapply(
  # Iterate through each clique in the data
  # Iterar cada clique nos dados
  (data %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    data_q_position = data %>%
      # Ensure correct clique position ID
      # Garantir um número de identificação (ID) correto para a posição das
      # cliques
      group_by(text, clique_id) %>%
      mutate(clique_position = cur_group_id()) %>%
      ungroup()
    
    # Get the clique rows
    # Selecionar as linhas da clique
    q_i = data_q_position %>%
      filter(text_clique_id == t_q_id)
    # Get the genre
    # Identificar o gênero textual
    genre = q_i$genre %>% unique()
    # Get the text ID
    # Identificar o ID do texto
    t = q_i$text %>% unique()
    # Get the clique ID
    # Identificar o ID da clique
    q = q_i$clique_id %>% unique()
    # Get the clique position
    # Identificar a posição da clique
    q_position = q_i$clique_position %>% unique()
    # Get cliques q_j with j > i
    # Selecionar as cliques q_j com j > i
    q_js = data_q_position %>%
      filter(text == t & clique_position > q_position)
    # Get the number of vertices in the clique
    # Calcular o número de vértices na clique
    n_q_i = q_i %>% nrow()
    
    # Calculate the pairwise indices
    # Calcular os índices pareados
    lapply(q_js$clique_position %>% unique(), function(j) {
      q_j = q_js %>%
        filter(clique_position == j)
  
      tibble(
        genre = genre,
        text = t,
        clique_id = q,
        clique_j = q_j$clique_id %>% unique(),
        # Calculate the number or repeated vertices
        # Calcular o número de vértices repetidos
        r_i = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
        # Calculate the number of cohesion edges
        # Calcular o número de arestas coesivas
        m_i = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in%
          (c(q_j %>% .$synonyms %>% str_split('\\|') %>% unlist(),
             q_j %>% .$hypernyms %>% str_split('\\|') %>% unlist()) %>%
             unique()) %>%
          sum(),
        n_q_i = n_q_i,
        # Get the number of vertices in q_j
        # Calcular o número de vértices em q_j
        n_q_j = q_j %>% nrow(),
        # Calculate vertex index
        # Calcular o índice de vértice
        v = ifelse(
          n_q_j == 0,
          0,
          ifelse(
            n_q_i > 0 && r_i < n_q_i && r_i < n_q_j,
            r_i/n_q_i,
            1
          )),
        # Calculate edge index
        # Calcular o índice de aresta
        e = ifelse(
          n_q_i > 0 && n_q_j > 0 && r_i < n_q_i && r_i < q_j, 
          m_i/((n_q_i - r_i)*(n_q_j - r_i)), 
          0)
      )
    }) %>%
      bind_rows()
  }) %>%
  bind_rows()

# Calculate the mean pairwise cohesion indices
# Calcular os índices de coesão pareada média
mean_pairwise_cohesion = mean_pairwise_cohesion_temp  %>%
  # Add the necessary repeated pairs
  # Adicionar os pares repetidos necessários
  bind_rows(mean_pairwise_cohesion_temp %>%
              rename(clique_id = clique_j,
                     clique_j = clique_id)) %>%
  group_by(genre, text, clique_id) %>%
  # Calculate the mean for each clique
  # Calcula a média para cada clique
  summarise(v = mean(v),
            e = mean(e))

# Combine the results into a single tibble
# Combinar os resultados em uma única tabela
indices = global_backward_cohesion %>%
  select(genre, text, clique_id, v, e) %>%
  # Identify the index
  # Identificar o índice
  mutate(index = 'global') %>%
  bind_rows(
    local_backward_cohesion %>%
      select(genre, text, clique_id, v, e) %>%
      # Identify the index
      # Identificar o índice
      mutate(index = 'local')
  ) %>%
  bind_rows(
    mean_pairwise_cohesion %>%
      select(genre, text, clique_id, v, e) %>%
      # Identify the index
      # Identificar o índice
      mutate(index = 'pairwise')
  )

# Write the results to a CSV file
# Salva os resultados em um arquivo CSV
# write_csv(indices, 'networks/cohesion_indices.csv')
# ------------------------------------------------------------------------------

# Calculate the mean cohesion indices of texts and pseudotexts
# Calcular os índices de coesão médios dos textos e pseudotextos
# ------------------------------------------------------------------------------
# Calculate the mean cohesion indices
# Calcula os índices médios de coesão
mean_cohesion_indices = indices %>%
  group_by(genre, text, index) %>%
  summarise(v = mean(v),
            e = mean(e)) %>%
  pivot_longer(c(v, e), names_to = 'type', values_to = 'value')

# Calculate the median, mean and standard deviation of the mean cohesion 
# distribution (shown in Table 2 in the article)
# Calcula a mediana, a média e o desvio padrão dos índices médios de coesão 
# (mostrados na Tabela 2 no artigo)
mean_cohesion_summary = mean_cohesion_indices %>%
  group_by(genre, index, type) %>%
  summarise(Median = median(value),
            Mean = mean(value),
            SD = sd(value)) %>%
  arrange(type, index)

# Write the results to a CSV file
# Salva os resultados em um arquivo CSV
# write_csv(mean_cohesion_indices, 'networks/mean_cohesion_indices.csv')
# ------------------------------------------------------------------------------

# Calculate the empirical probabilities
# Calcular as probabilidades empíricas
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# Recalculate the indices with sample of sentences
# Recalcular os ínices com amostras de períodos
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# Plot figures
# Gerar figuras
# ------------------------------------------------------------------------------
