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
# Last update: September 04, 2023
# Última atualização: 04/09/2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install the necessary packages
# Instalar pacotes necessários
# install.packages('tidyverse')
# install.packages('SnowballC')
# install.packages('wordnet')
# install.packages('jtools')
# install.packages('ggpattern')
# install.packages('gridExtra')

# Load required packages
# Carregar pacotes necessários
library(tidyverse)
library(SnowballC)
library(jtools)
library(ggpattern)
library(gridExtra)

# Set APA theme for plotting
# Configurar o tema APA para figuras
theme_set(theme_apa())

# Process OANC XML files (selected from https://anc.org/)
# Processar arquivos XML do OANC (selecionados de https://anc.org/)
# ------------------------------------------------------------------------------
# Inform directory with the OANC XML files
# Informar diretório com os arquivos XML do OANC
oanc_dir = '../data/oanc_xml/'

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
write_csv(oanc_data, '../data/oanc_processed.csv')
# ------------------------------------------------------------------------------

# Get synonyms and hypernyms from Wordnet
# Identificar sinônimos e hiperônimos usando o Wordnet
# ------------------------------------------------------------------------------
# Read synonyms and hypernyms retrieved from Wordnet. 
# The data were retrieved using the code in lines 139-205. The code worked in a 
# Debian 11 machine running R version 4.0.4 and OpenJDK version 11.0.20, and in 
# the Posit Cloud (https://posit.cloud/) with R version 4.3.1, but failed in a 
# Windows 10 machine with R version 4.3.1 and Oracle Java SE 17, and in a Cloud 
# Ocean environment running Ubuntu 18.04, R version 4.2.1 and default-jdk 
# package version 2:1.11-68ubuntu1~18.04.1. Because of these inconsistencies, we
# provide the retrieved data in a CSV file.
# Ler sinônimos e hiperônimos recuperados do Wordnet.
# Os dados foram recuperados usando o código nas linhas 139-205. O código 
# funcionou em uma máquina Debian 11 rodando a versão 4.0.4 do R e a versão 
# 11.0.20 do OpenJDK, e na Posit Cloud (https://posit.cloud/) com a versão 4.3.1
# do R, mas falhou em uma máquina Windows 10 com a versão 4.3.1 do R e o Java SE
# 17 da Oracle, e em um ambiente do Cloud Ocean rodando Ubuntu 18.04, versão 
# 4.2.1 do R e o pacote default-jdk na versão 2:1.11-68ubuntu1~18.04.1. Por 
# conta destas inconsistências, disponibilizamos os dados recuperados em um 
# arquivo CSV.
synonyms_hypernyms = read_csv('../data/wordnet_synonyms_hypernyms.csv')

# # Load required package
# library(wordnet)
# 
# # Set Wordnet English dictionary
# # Configurar o dicionário de inglês do Wordnet
# setDict('../data/wn3.1.dict/dict/')
# 
# # Get unique lemmas with POS tags
# # Identificar os lemas únicos com suas etiquetas POS
# lemmas = oanc_data %>%
#   distinct(lemma, pos)
# 
# # Create tibble with synonyms and hypernyms
# # Criar tabela com sinônimos e hiperônimos
# synonyms_hypernyms = mapply(function(lemma, pos) {
#   pos_wordnet = NA
#   
#   # Map POS tags in the corpus with the correspondent POS tags in Wordnet
#   # Mapeia as etiquetas POS no corpus com as etiquetas POS no Wordnet
#   if(pos %in% c('VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')) {
#     pos_wordnet = 'VERB'
#   } else if (pos %in% c('NN', 'NNS', 'NNP', 'NNPS')) {
#     pos_wordnet = 'NOUN'
#   } else if (pos %in% c('ADJ', 'JJ', 'JJR', 'JJS')) {
#     pos_wordnet = 'ADJECTIVE'
#   } else if (pos %in% c('ADV', 'RB', 'RBR', 'RBS')) {
#     pos_wordnet = 'ADVERB'
#   }
#   
#   if (is.na(pos_wordnet)) {
#     return(NA)
#   } else {
#     tibble(lemma = lemma,
#            pos = pos,
#            # Get synonyms from Wordnet
#            # Identificar sinônimos usando o Wordnet
#            synonyms = synonyms(lemma, pos_wordnet) %>%
#              paste(collapse = '|') %>%
#              str_to_lower(),
#            # Get hypernyms from Wordnet
#            # Identificar hiperônimos usando o Wordnet
#            hypernyms = tryCatch({
#              filter = getTermFilter('ExactMatchFilter', lemma, T)
#              terms = getIndexTerms(pos_wordnet, 1, filter)
#              synsets = getSynsets(terms[[1]])
#              
#              sapply(synsets, function(s) {
#                relatedSynsets = getRelatedSynsets(s, '@')
#                
#                sapply(relatedSynsets, getWord)
#              }) %>%
#                unlist() %>%
#                unique() %>%
#                paste0(collapse = '|') %>%
#                str_to_lower()
#            }, error = function(e) {
#              return(NULL)
#            }))
#   }
# }, lemmas$lemma, lemmas$pos, SIMPLIFY = F) %>% 
#   unname() %>%
#   .[!is.na(.)] %>%
#   bind_rows()
# 
# # Write lemmas with their synonyms and hypernyms to a CSV file
# # Salvar lemas com seus sinônimos e hiperônimos em um arquivo CSV
# write_csv(synonyms_hypernyms, '../data/wordnet_synonyms_hypernyms.csv')
# ------------------------------------------------------------------------------

# Generate pseudotexts
# Gerar pseudotextos
# ------------------------------------------------------------------------------
# To generate new pseudotexts instead of usings the ones reported in the study, 
# comment line 215 and uncomment and run the code in lines 217-250 
# Para gerar novos pseudotextos ao invés de usar os mesmos reportados no estudo,
# comente a linha 215 e descomente e rode o código nas linhas 217-250
pseudotexts = read_csv('../data/oanc_pseudotexts_study.csv')

# # Get the number of texts in the corpus
# # Identificar o número de textos no corpus
# corpus_size = oanc_data$text %>% unique() %>% length()
# 
# # Generate pseudotexts
# # Gerar pseudotextos
# pseudotexts = lapply(1:corpus_size, function(i) {
#   oanc_data %>%
#     # Attribute an ID number to each clique in the corpus
#     # Atribuir um número de identificação (ID) para cada clique no corpus
#     unite('text_clique_id', c(text, clique_id), remove = F) %>%
#     filter(text_clique_id %in% (
#       # Get a random sample of cliques, one per text
#       # Selecionar uma amostra aleatória de cliques, uma por texto
#       oanc_data %>%
#         group_by(text) %>%
#         sample_n(1) %>%
#         unite('text_clique_id', c(text, clique_id)) %>%
#         .$text_clique_id)) %>%
#     group_by(text_clique_id) %>%
#     # Attribute an ID number to each pseudotexts and reset clique_id
#     # Atribuir um número de identificação (ID) para cada pseudotexto e
#     # reconfigurar a coluna clique_id
#     mutate(genre = 'pseudotext',
#            text = paste0('pseudotext_', str_pad(i, 2, 'left', '0')),
#            clique_id = cur_group_id()) %>%
#     ungroup() %>%
#     select(-text_clique_id)
# }) %>%
#   bind_rows()
# 
# # Write pseudotexts to a CSV file
# # Salvar pseudotextos em um arquivo CSV
# write_csv(pseudotexts, '../data/oanc_pseudotexts.csv')
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
  unite('text_clique_id', c(text, clique_id), remove = F)

# Write the combined data to a CSV file
# Salvar os dados combinados em um arquivo CSV
write_csv(data, '../data/data.csv')
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
      m_i = G_i %>%
        filter(!.$stem %in% q_i$stem) %>%
        bind_rows(G_i %>% filter(!.$stem %in% q_i$stem)) %>%
        distinct(stem, .keep_all = T) %>%
        separate_rows(hypernyms, sep = '\\|') %>%
        filter(synonyms %in% 
                 (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma) |
                 hypernyms %in% 
                 (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma)) %>%
        nrow(),
      # Get the number of vertices in the clique
      # Calcular o número de vértices na clique
      n_q_i = q_i %>% nrow(),
      # Get the number of vertices in G_i
      # Calcular o número de vértices em G_i
      n_iprev = G_i %>% .$stem %>% unique() %>% length(),
      # Calculate vertex index
      # Calcular o índice de vértices
      v = ifelse(
        n_iprev == 0,
        0,
        ifelse(
          n_q_i > 0 && r_i < n_q_i && r_i < n_iprev,
          r_i/n_q_i,
          1
        )),
      # Calculate edge index
      # Calcular o índice de arestas
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
      m_i = q_j %>%
        filter(!.$stem %in% q_i$stem) %>%
        bind_rows(q_j %>% filter(!.$stem %in% q_i$stem)) %>%
        distinct(stem, .keep_all = T) %>%
        separate_rows(hypernyms, sep = '\\|') %>%
        filter(synonyms %in% 
                 (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) |
                 hypernyms %in% 
                 (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma)) %>%
        nrow(),
      # Get the number of vertices in the clique
      # Calcular o número de vértices na clique
      n_q_i = q_i %>% nrow(),
      # Get the number of vertices in q_{i-1}
      # Calcular o número de vértices in q_{i-1}
      n_q_iprev = q_j %>% nrow(),
      # Calculate vertex index
      # Calcular o índice de vértices
      v = ifelse(
        n_q_iprev == 0,
        0,
        ifelse(
          n_q_i > 0 && r_i < n_q_i && r_i < n_q_iprev,
          r_i/n_q_i,
          1
        )),
      # Calculate edge index
      # Calcular o índice de arestas
      e = ifelse(
        n_q_i > 0 && n_q_iprev > 0 && r_i < n_q_i && r_i < n_q_iprev, 
        m_i/((n_q_i - r_i)*(n_q_iprev - r_i)), 
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
        m_i = q_j %>%
          filter(!.$stem %in% q_i$stem) %>%
          bind_rows(q_j %>% filter(!.$stem %in% q_i$stem)) %>%
          distinct(stem, .keep_all = T) %>%
          separate_rows(hypernyms, sep = '\\|') %>%
          filter(synonyms %in% 
                   (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) |
                   hypernyms %in% 
                   (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma)) %>%
          nrow(),
        n_q_i = n_q_i,
        # Get the number of vertices in q_j
        # Calcular o número de vértices em q_j
        n_q_j = q_j %>% nrow(),
        # Calculate vertex index
        # Calcular o índice de vértices
        v = ifelse(
          n_q_j == 0,
          0,
          ifelse(
            n_q_i > 0 && r_i < n_q_i && r_i < n_q_j,
            r_i/n_q_i,
            1
          )),
        # Calculate edge index
        # Calcular o índice de arestas
        e = ifelse(
          n_q_i > 0 && n_q_j > 0 && r_i < n_q_i && r_i < n_q_j, 
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
write_csv(indices, '../results/cohesion_indices.csv')
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

# Write the results to CSV files
# Salva os resultados em arquivos CSV
write_csv(mean_cohesion_indices, '../results/mean_cohesion_indices.csv')
write_csv(mean_cohesion_summary, '../results/mean_cohesion_indices_summary.csv')
# ------------------------------------------------------------------------------

# Calculate the empirical probabilities
# Calcular as probabilidades empíricas
# ------------------------------------------------------------------------------
# Empirical probabilities of the vertex cohesion indices
# Probabilidades empíricas dos índices de coesão de vértices
empirical_probabilities = indices %>%
  mutate(corpus = ifelse(genre == 'pseudotext', 'pseudotexts', 'texts')) %>%
  slice(rep(1:n(), each = 9)) %>%
  mutate(cutoff = rep(seq(.1, .9, .1), n()/9)) %>%
  group_by(corpus, index, cutoff) %>%
  # Calculate the empirical probabilities
  # Calcular as probabilidades empíricas
  summarise(p = mean(v >= cutoff)) %>%
  pivot_wider(names_from = corpus, values_from = p) %>%
  # Calculate the differences between the probabilites from texts and pseudotexts
  # Calcular as diferenças entre as probabilidades de textos e pseudotextos
  mutate(difference = texts - pseudotexts)

# Write the results to a CSV file
# Salva os resultados em um arquivo CSV
write_csv(empirical_probabilities, 
          '../results/indices_empirical_probabilities.csv')
# ------------------------------------------------------------------------------

# Recalculate the indices with sample of sentences
# Recalcular os índices com amostras de períodos
# ------------------------------------------------------------------------------
# Get samples of the global and local indices calculated from the first 10 to 60 
# sentences of each text and pseudotext
# Selecionar amostras dos índices globais e locais calculados dos primeiros 10 a
# 60 períodos de cada texto e pseudotexto
indices_samples_temp = lapply(seq(10, 60, 10), function(s) {
  indices %>%
    filter(index %in% c('global', 'local')) %>%
    group_by(text, index) %>%
    slice_head(n = s) %>%
    mutate(n_sentences = s)
}) %>%
  bind_rows()

# Recalculate the mean pairwise indices for the first 10 to 60 sentences of each
# text and pseudotext
# Recalcular os índices de coesão pareada média para os primeiros 10 a 60 períodos
# de cada texto e pseudotexto
data_samples = lapply(seq(10, 60, 10), function(s) {
  data %>%
    filter(clique_id <= s) %>%
    mutate(n_sentences = s)
}) %>%
  bind_rows() %>%
  group_by(n_sentences, text) %>%
  # Identify and remove texts with less sentences than the size of the samples
  # Identificar e remover textos com menos períodos que os tamanhos das amostras
  mutate(n_cliques = clique_id %>% unique() %>% length()) %>%
  filter(n_cliques == n_sentences) %>%
  unite('s_text_clique_id', c(n_sentences, text_clique_id), remove = F)

# Recalculate the mean pairwise cohesion indices
# Recalcular os índices de coesão pareada média
samples_mean_pairwise_cohesion_temp = lapply(
  # Iterate through each clique in the data
  # Iterar cada clique nos dados
  (data_samples %>%
     .$s_text_clique_id %>% 
     unique()),
  function(s_t_q_id) {
    data_q_position = data_samples %>%
      # Ensure correct clique position ID
      # Garantir um número de identificação (ID) correto para a posição das
      # cliques
      group_by(text, clique_id, n_sentences) %>%
      mutate(clique_position = cur_group_id()) %>%
      ungroup()
    
    # Get the clique rows
    # Selecionar as linhas da clique
    q_i = data_q_position %>%
      filter(s_text_clique_id == s_t_q_id)
    # Get the sample size
    # Identificar tamanho da amostra
    s = q_i$n_sentences %>% unique()
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
      filter(text == t & n_sentences == s & clique_position > q_position)
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
        m_i = q_j %>%
          filter(!.$stem %in% q_i$stem) %>%
          bind_rows(q_j %>% filter(!.$stem %in% q_i$stem)) %>%
          distinct(stem, .keep_all = T) %>%
          separate_rows(hypernyms, sep = '\\|') %>%
          filter(synonyms %in% 
                   (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) |
                   hypernyms %in% 
                   (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma)) %>%
          nrow(),
        n_q_i = n_q_i,
        # Get the number of vertices in q_j
        # Calcular o número de vértices em q_j
        n_q_j = q_j %>% nrow(),
        # Calculate vertex index
        # Calcular o índice de vértices
        v = ifelse(
          n_q_j == 0,
          0,
          ifelse(
            n_q_i > 0 && r_i < n_q_i && r_i < n_q_j,
            r_i/n_q_i,
            1
          )),
        # Calculate edge index
        # Calcular o índice de arestas
        e = ifelse(
          n_q_i > 0 && n_q_j > 0 && r_i < n_q_i && r_i < n_q_j, 
          m_i/((n_q_i - r_i)*(n_q_j - r_i)), 
          0),
        n_sentences = s
      )
    }) %>%
      bind_rows()
  }) %>%
  bind_rows()

# Calculate the mean pairwise cohesion indices
# Calcular os índices de coesão pareada média
samples_mean_pairwise_cohesion = samples_mean_pairwise_cohesion_temp  %>%
  # Add the necessary repeated pairs
  # Adicionar os pares repetidos necessários
  bind_rows(samples_mean_pairwise_cohesion_temp %>%
              rename(clique_id = clique_j,
                     clique_j = clique_id)) %>%
  group_by(genre, text, clique_id, n_sentences) %>%
  # Calculate the mean for each clique
  # Calcula a média para cada clique
  summarise(v = mean(v),
            e = mean(e))

# Combine the samples indices
# Combinar os índices das amostras
indices_samples = indices_samples_temp %>%
  bind_rows(samples_mean_pairwise_cohesion %>%
              mutate(index = 'pairwise'))

# Write the results to a CSV file
# Salva os resultados em um arquivo CSV
write_csv(indices_samples, '../results/cohesion_indices_samples.csv')
# ------------------------------------------------------------------------------

# Plot figures
# Gerar figuras
# ------------------------------------------------------------------------------
# Prepare data for plotting
# Preparar dados para geração de gráficos
indices_plot = indices %>%
  pivot_longer(c(v, e), names_to = 'index_type', values_to = 'value') %>%
  mutate(index_type = index_type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         corpus = ifelse(genre == 'pseudotext', 'Pseudotexts', 'Texts') %>%
           factor(levels = c('Texts', 'Pseudotexts')),
         genre = genre %>%
           as_factor() %>%
           recode('berlitz' = 'Travel guides',
                  'biomed' = 'Biomedical research articles',
                  'icic' = 'Letters',
                  'media' = 'Government media',
                  'plos' = 'Scientific and medical articles',
                  'pseudo' = 'Pseudotexts',
                  'slate' = 'Magazine articles'))

indices_samples_plot = indices_samples %>%
  pivot_longer(c(v, e), names_to = 'index_type', values_to = 'value') %>%
  mutate(index_type = index_type %>%
           as_factor() %>%
           recode('e' = 'Edge Cohesion',
                  'v' = 'Vertex Cohesion'),
         corpus = ifelse(genre == 'pseudotext', 'Pseudotexts', 'Texts') %>%
           factor(levels = c('Texts', 'Pseudotexts')),
         genre = genre %>%
           as_factor() %>%
           recode('berlitz' = 'Travel guides',
                  'biomed' = 'Biomedical research articles',
                  'icic' = 'Letters',
                  'media' = 'Government media',
                  'plos' = 'Scientific and medical articles',
                  'pseudo' = 'Pseudotexts',
                  'slate' = 'Magazine articles'))

# Figure 11
# Figura 11
figure11 = indices_plot %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'Text Global Backward Cohesion',
                  'local' = 'Text Local Backward Cohesion',
                  'pairwise' = 'Text Mean Pairwise Cohesion')) %>%
  group_by(corpus, genre, text, index_type, index) %>%
  summarize(`Mean indices` = mean(value)) %>%
  ggplot(aes(genre, `Mean indices`, fill = genre, group = genre)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = 'point',
               shape = 22,
               size = 2,
               color = 'black',
               fill = 'white',
               position = position_dodge(width = 1)) +
  facet_wrap(index_type ~ index, scales = 'free') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('') +
  theme(axis.text.x = element_blank())

ggsave(filename = '../results/Figure11.svg',
       plot = figure11,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Figure 12
# Figura 12
figure12 = read_delim('../data/taaco_results.csv', 
                      delim = ';', 
                      locale = locale(decimal_mark = ',')) %>%
  mutate(corpus = corpus %>%
           as_factor() %>%
           recode('text' = 'Texts',
                  'pseudo' = 'Pseudotexts'),
         genre = genre %>%
           as_factor() %>%
           recode('berlitz' = 'Travel guides',
                  'biomed' = 'Biomedical research articles',
                  'icic' = 'Letters',
                  'media' = 'Government media',
                  'plos' = 'Scientific and medical articles',
                  'pseudo' = 'Pseudotexts',
                  'slate' = 'Magazine articles')) %>%
  pivot_longer(-c(corpus, genre, Filename), names_to = 'index', values_to = 'value') %>%
  mutate(index = index %>%
           recode('adjacent_overlap_cw_sent' = str_wrap('Adjacent sentence overlap content lemmas', 32),
                  'adjacent_overlap_cw_sent_div_seg' = str_wrap('Adjacent sentence overlap content lemmas (sentence normed)', 32),
                  'adjacent_overlap_binary_cw_sent' = str_wrap('Binary adjacent sentence overlap content lemmas', 32),
                  'syn_overlap_sent_noun' = str_wrap('Synonym overlap (sentence, noun)', 32),
                  'syn_overlap_sent_verb' = str_wrap('Synonym overlap (sentence, verb)', 32))) %>%
  ggplot(aes(genre, value, fill = genre)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point',
               shape = 22,
               size = 2,
               color = 'black',
               fill = 'white',
               position = position_dodge(width = 1)) +
  facet_wrap(. ~ index, scales = 'free') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('') +
  theme(axis.text.x = element_blank()) +
  ylab('Index value')

ggsave(filename = '../results/Figure12.svg',
       plot = figure12,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Figure 13
# Figura 13
figure13 = indices_plot %>%
  mutate(index = index %>%
           as_factor() %>%
           recode('global' = 'θ - Global Backward Cohesion',
                  'local' = 'λ - Local Backward Cohesion',
                  'pairwise' = 'ρ - Mean Pairwise Cohesion')) %>%
  ggplot(aes(value, 
             fill = corpus, 
             group = corpus, 
             pattern = corpus)) +
  facet_wrap(index_type ~ index, scales = 'free') +
  geom_density_pattern(alpha = .5,
                       pattern_fill = 'black',
                       pattern_color = 'black') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal') +
  scale_fill_brewer(palette = 'Dark2') +
  xlab('Value') +
  ylab('Density')

ggsave(filename = '../results/Figure13.svg',
       plot = figure13,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Figure 14
# Figura 14
figure14 = grid.arrange(
  indices_samples_plot %>%
    filter(index_type == 'Vertex Cohesion' & index == 'global') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Global Backward Vertex Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density'),
  indices_samples_plot %>%
    filter(index_type == 'Edge Cohesion' & index == 'global') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Global Backward Edge Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density')
)

ggsave(filename = '../results/Figure14.svg',
       plot = figure14,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Figure 15
# Figura 15
figure15 = grid.arrange(
  indices_samples_plot %>%
    filter(index_type == 'Vertex Cohesion' & index == 'local') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Local Backward Vertex Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density'),
  indices_samples_plot %>%
    filter(index_type == 'Edge Cohesion' & index == 'local') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Local Backward Edge Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density')
)

ggsave(filename = '../results/Figure15.svg',
       plot = figure15,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)

# Figure 16
# Figura 16
figure16 = grid.arrange(
  indices_samples_plot %>%
    filter(index_type == 'Vertex Cohesion' & index == 'pairwise') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Mean Pairwise Vertex Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density'),
  indices_samples_plot %>%
    filter(index_type == 'Edge Cohesion' & index == 'pairwise') %>%
    ggplot(aes(value, 
               fill = corpus, 
               group = corpus, 
               pattern = corpus)) +
    facet_wrap(. ~ n_sentences, ncol = 6) +
    geom_density_pattern(alpha = .5,
                         pattern_fill = 'black',
                         pattern_color = 'black') +
    theme(legend.position = 'bottom', 
          legend.direction = 'horizontal',
          legend.key.size = unit(.5, 'cm'),
          plot.title = element_text(size = 12, hjust = .5, face = 'plain'),
          axis.text.x = element_text(size = 7)) +
    scale_fill_brewer(palette = 'Dark2') +
    ggtitle('Mean Pairwise Edge Cohesion\nNumber of sentences') +
    xlab('Value') +
    ylab('Density')
)

ggsave(filename = '../results/Figure16.svg',
       plot = figure16,
       device = 'svg',
       width = 24.7,
       height = 16,
       units = 'cm',
       dpi = 300)
