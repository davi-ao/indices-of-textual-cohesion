# ------------------------------------------------------------------------------
# Test the algorithms to calculate the indices of textual cohesion
# Testa os algoritmos para calcular os índices de coesão textual
# 
#
# Authors: Davi Alves Oliveira, Valter de Senna, and Hernane Borges de Barros 
# Pereira
# Autores: Davi Alves Oliveira, Valter de Senna, e Hernane Borges de Barros 
# Pereira 
#
# Last update: September 02, 2023
# Última atualização: 02/09/2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Install the necessary packages
# Instalar pacotes necessários
# install.packages('tidyverse')

# Load required packages
# Carregar pacotes necessários
library(tidyverse)

# Read test data
# Ler dados de teste
# ------------------------------------------------------------------------------
data_test = read_csv('test/test.csv') %>%
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
#-------------------------------------------------------------------------------

# Calculate the cohesion indices
# Calcular os índices de coesão
# ------------------------------------------------------------------------------
# Global Backward Cohesion
# Coesão Regressiva Global
global_backward_cohesion_test = lapply(
  # Iterate each clique in the data
  # Iterar cada clique nos dados
  (data_test %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    # Select the clique rows
    # Selecionar as linhas da clique
    q_i = data_test %>%
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
    G_i = data_test %>% filter(text == t & clique_id < q)
    
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
local_backward_cohesion_test = lapply(
  # Iterate each clique in the data
  # Iterar cada clique nos dados
  (data_test %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    data_q_position = data_test %>%
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
      # Calcular o índice de vértice
      v = ifelse(
        n_q_iprev == 0,
        0,
        ifelse(
          n_q_i > 0 && r_i < n_q_i && r_i < n_q_iprev,
          r_i/n_q_i,
          1
        )),
      # Calculate edge index
      # Calcular o índice de aresta
      e = ifelse(
        n_q_i > 0 && n_q_iprev > 0 && r_i < n_q_i && r_i < n_q_iprev, 
        m_i/((n_q_i - r_i)*(n_q_iprev - r_i)), 
        0)
    )
  }) %>%
  bind_rows()

# Mean Pairwise Cohesion
# Coesão Média Pareada
mean_pairwise_cohesion_temp_test = lapply(
  # Iterate through each clique in the data
  # Iterar cada clique nos dados
  (data_test %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    data_q_position = data_test %>%
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
mean_pairwise_cohesion_test = mean_pairwise_cohesion_temp_test  %>%
  # Add the necessary repeated pairs
  # Adicionar os pares repetidos necessários
  bind_rows(mean_pairwise_cohesion_temp_test %>%
              rename(clique_id = clique_j,
                     clique_j = clique_id)) %>%
  group_by(genre, text, clique_id) %>%
  # Calculate the mean for each clique
  # Calcula a média para cada clique
  summarise(v = mean(v),
            e = mean(e))

# Combine the results into a single tibble
# Combinar os resultados em uma única tabela
indices_test = global_backward_cohesion_test %>%
  select(genre, text, clique_id, v, e) %>%
  # Identify the index
  # Identificar o índice
  mutate(index = 'global') %>%
  bind_rows(
    local_backward_cohesion_test %>%
      select(genre, text, clique_id, v, e) %>%
      # Identify the index
      # Identificar o índice
      mutate(index = 'local')
  ) %>%
  bind_rows(
    mean_pairwise_cohesion_test %>%
      select(genre, text, clique_id, v, e) %>%
      # Identify the index
      # Identificar o índice
      mutate(index = 'pairwise')
  )

# Write the results to a CSV file
# Salva os resultados em um arquivo CSV
write_csv(indices_test, 'test/cohesion_indices_test.csv')
# ------------------------------------------------------------------------------