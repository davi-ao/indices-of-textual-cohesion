# Load required packages
library(tidyverse)
library(SnowballC)
library(wordnet)

# Set Wordnet English dictionary
setDict('data/wn3.1.dict/dict/')

# PROCESS OANC XML FILES -------------------------------------------------------

# Directory with the OANC XML files
oanc_dir = 'data/oanc_xml/'

# Read OANC XML files
oanc_data = lapply(paste0(oanc_dir, list.files(oanc_dir)), function(f) {
  read_file(f) %>% 
    # Get the children of <fs> elements
    str_extract_all('(?<=<fs>)(.|[\\n\\r])*?(?=<\\/fs>)') %>%
    unlist() %>%
    # Remove extra whitespace
    str_remove_all('[\\n\\r]') %>%
    str_replace_all('\\s{2,}', ' ') %>%
    str_trim() %>%
    # Get values from the "name" and "value" attributes
    str_match_all('name="(\\S+)" value="(\\S+)"') %>%
    # Replace sentence ending characters with a period
    sapply(function(fs) {
      if (any(fs[,3] %>% str_detect('[.:?!…]'))) {
        '.'
      } else {
        paste0(fs[,2], '_', fs[,3], collapse = '|')
      }
    }) %>%
    # Convert into a single string
    paste0(collapse = ' ') %>%
    # Split using a period as delimiter
    str_split_1(' . ') %>%
    # Convert vector into a tibble column
    tibble(
      # Get genre from file name
      genre = f %>% str_match('oanc_xml/([a-z]+)_') %>% .[,2],
      text = f, 
      token = .) %>%
    # Attribute an ID to each sentence
    mutate(sentence_id = row_number()) %>%
    # Split tokens
    separate_rows(token, sep = ' ') %>%
    # Attribute an Id to each token
    mutate(token_id = row_number()) %>%
    # Separate token and POS
    separate_rows(token, sep = '\\|') %>% 
    separate(token, c('name', 'value'), '_') %>%
    pivot_wider(id_cols = c(genre, text, sentence_id, token_id), 
                names_from = name, 
                values_from = value) %>%
    select(genre, text, sentence_id, base, msd, affix) %>%
    rename(clique_id = sentence_id, lemma = base, pos = msd) %>%
    # Set lemma stems
    mutate(stem = wordStem(lemma)) %>%
    # Keep only content words
    filter(
      pos %in% c('CD', 'FW', 'JJ', 'JJR', 'JJS', 'NN', 'NNS', 'NNP', 'NNPS',
                 'RB', 'RBR', 'RBS', 'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ'))
}) %>%
  bind_rows()

# Write OANC procesed data
# write_csv(oanc_data, 'data/oanc_data.csv')

# GET SYNONYMS AND HYPERNYMS FROM WORDNET --------------------------------------

# Get unique lemmas with POS
lemmas = corpus %>%
  distinct(lemma, pos)

synonyms_hypernyms = mapply(function(lemma, pos) {
  pos_wordnet = NA
  
  # Map POS in the corpus with the correspondent POS in Wordnet
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
           synonyms = synonyms(lemma, pos_wordnet) %>%
             paste(collapse = '|') %>%
             str_to_lower(),
           # Get hypernyms from Wordnet
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

# Write lemmas with synonyms and hypernyms
# write_csv(synonyms_hypernyms, 'data/synonyms_hypernyms.csv')

# GENERATE PSEUDOTEXTS ---------------------------------------------------------

# To use the same pseudotexts reported in the study, uncomment the code below
# instead of running lines 131-154
pseudotexts = read_csv('data/oanc_pseudotexts_study.csv')

# Get the number of texts in the corpus
# corpus_size = corpus$text %>% unique() %>% length()

# Generate pseudotexts
# pseudotexts = lapply(1:corpus_size, function(i) {
#   corpus %>%
#     # Attribute an unique id for each clique in the corpus
#     unite('text_clique_id', text:clique_id, remove = F) %>%
#     filter(text_clique_id %in% (
#       # Get a random sample of cliques, one per text
#       corpus %>%
#         group_by(text) %>%
#         sample_n(1) %>%
#         unite('text_clique_id', text:clique_id) %>%
#         .$text_clique_id)) %>%
#     group_by(text_clique_id) %>%
#     # Add unique id to pseudotext and reset clique_id
#     mutate(genre = 'pseudotext',
#            text = paste0('pseudotext_', str_pad(i, 2, 'left', '0')),
#            clique_id = cur_group_id()) %>%
#     ungroup() %>%
#     select(-text_clique_id)
# }) %>%
#   bind_rows()

# Write pseudotexts
# write_csv(pseudotexts, 'data/oanc_pseudotexts.csv')

# JOIN THE DATA ----------------------------------------------------------------

data = corpus %>% 
  bind_rows(pseudotexts) %>%
  left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
  group_by(text, clique_id) %>%
  # Keep only the unique stems in each clique
  distinct(stem, .keep_all = T) %>%
  ungroup() %>%
  # Remove synonym data not considered in the analysis
  mutate(synonyms = synonyms %>% str_remove_all('\\(a\\)|\\(p\\)')) %>%
  # Create a unique id for each clique in the data
  unite('text_clique_id', text:clique_id, remove = F)

# Write the data
# write_csv(data, 'data/data.csv')

# CALCULATE THE COHESION INDICES -----------------------------------------------

# Global Backward Cohesion
global_backward_cohesion = lapply(
  # Iterate through each clique in the data
  (data %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    # Get the clique rows
    q_i = data %>%
      filter(text_clique_id == t_q_id)
    # Get the genre
    genre = q_i$genre %>% unique()
    # Get the text id
    t = q_i$text %>% unique()
    # Get the clique id
    q = q_i$clique_id %>% unique()
    # Get the cliques added before the current clique
    G_i = data %>% filter(text == t & clique_id < q)
    
    # Calculate the indices
    tibble(
      genre = genre,
      text = t,
      clique_id = q,
      # Calculate the number of repeated vertices
      r = intersect(q_i %>% .$stem, G_i %>% .$stem) %>% length(),
      # Calculate the number of cohesion edges
      m_c = (q_i %>% filter(!stem %in% G_i$stem) %>% .$lemma) %in% 
        (c(G_i %>% .$synonyms %>% str_split_1('\\|'),
           G_i %>% .$hypernyms %>% str_split_1('\\|')) %>% 
           unique()) %>%
        sum(),
      # Get the number of vertices in the clique
      q_n = q_i %>% nrow(),
      # Get the number of vertices in G_i
      n_iprev = G_i %>% nrow(),
      # Calculate vertex index
      v = ifelse(
        n_iprev == 0,
        0,
        ifelse(
          q_n > 0 && r < q_n && r < n_iprev,
          r/q_n,
          1
        )),
      # Calculate edge index
      e = ifelse(
        q_n > 0 && n_iprev > 0 && r < q_n && r < n_iprev, 
        m_c/((q_n - r)*(n_iprev - r)), 
        0)
    )
  }) %>%
  bind_rows()

# Local Backward Cohesion
local_backward_cohesion = lapply(
  # Iterate through each clique in the data
  (data %>%
     .$text_clique_id %>% 
     unique()),
  function(t_q_id) {
    data_q_position = data %>%
      # Ensure correct clique position indices
      group_by(text, clique_id) %>%
      mutate(clique_position = cur_group_id()) %>%
      ungroup()
    
    # Get the clique rows
    q_i = data_q_position %>%
      filter(text_clique_id == t_q_id)
    # Get the genre
    genre = q_i$genre %>% unique()
    # Get the text id
    t = q_i$text %>% unique()
    # Get the clique id
    q = q_i$clique_id %>% unique()
    # Get the clique position
    q_position = q_i$clique_position %>% unique()
    # Get clique q_{i-1}
    q_j = data_q_position %>%
      filter(text == t & clique_position == q_position - 1)
    
    # Calculate the indices
    tibble(
      genre = genre,
      text = t,
      clique_id = q,
      # Calculate the number of repeated vertices
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length(),
      # Calculate the number of cohesion edges
      m_c = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split_1('\\|'),
           q_j %>% .$hypernyms %>% str_split_1('\\|')) %>% 
           unique()) %>%
        sum(),
      # Get the number of vertices in the clique
      q_n = q_i %>% nrow(),
      # Get the number of vertices in q_{i-1}
      q_n_iprev = q_j %>% nrow(),
      # Calculate vertex index
      v = ifelse(
        q_n_iprev == 0,
        0,
        ifelse(
          q_n > 0 && r < q_n && r < q_n_iprev,
          r/q_n,
          1
        )),
      # Calculate edge index
      e = ifelse(
        q_n > 0 && q_n_iprev > 0 && r < q_n && r < q_n_iprev, 
        m_c/((q_n - r)*(q_n_iprev - r)), 
        0)
    )
  }) %>%
  bind_rows()

# Mean Pairwise Cohesion

# Get the list of clique ids
clique_ids = data %>%
  .$text_clique_id %>%
  unique()

# Create matricces that will be populated with the pairwise indices values
# Vertex cohesion matrix
pairwise_indices_v = matrix(rep(NA, length(clique_ids)^2), 
                            length(clique_ids), 
                            length(clique_ids))
rownames(pairwise_indices_v) = clique_ids
colnames(pairwise_indices_v) = clique_ids
# Edge cohesion matrix
pairwise_indices_e = matrix(rep(NA, length(clique_ids)^2), 
                            length(clique_ids), 
                            length(clique_ids))
rownames(pairwise_indices_e) = clique_ids
colnames(pairwise_indices_e) = clique_ids

# Calculate pairwise cohesion values and populate the matricces
for(i in 1:length(clique_ids)) {
  # Get the clique rows
  q_i = data %>% filter(text_clique_id == clique_ids[i])
  # Get the number of vertices in the clique
  q_n = q_i %>% nrow()
  
  # Iterate through each cell
  for (j in 1:length(clique_ids)) {
    # The matrix is symmetric
    if (j > i) {
      # Get the clique q_j to compare
      q_j = data %>% filter(text_clique_id == clique_ids[j])
      # Calculate the number or repeated vertices
      r = intersect(q_i %>% .$stem, q_j %>% .$stem) %>% length()
      # Calculate the number of cohesion edges
      m_c = (q_i %>% filter(!stem %in% q_j$stem) %>% .$lemma) %in% 
        (c(q_j %>% .$synonyms %>% str_split_1('\\|'),
           q_j %>% .$hypernyms %>% str_split_1('\\|')) %>% 
           unique()) %>%
        sum()
      # Get the number of vertices in q_j
      n_j = q_j %>% nrow()
      # Store the index values in the correspondent matricces' cells
      pairwise_indices_v[i,j] = r/q_n
      pairwise_indices_v[j,i] = r/q_n
      pairwise_indices_e[i,j] = m_c/((q_n - r)*(n_j - r))
      pairwise_indices_e[j,i] = m_c/((q_n - r)*(n_j - r))
    }
  }
}

# Calculate the mean pairwise cohesion
mean_pairwise_cohesion = lapply(1:length(clique_ids), function(i) {
  # Get the clique rows
  q_i = data %>%
    filter(text_clique_id == clique_ids[i])
  # Get the genre
  genre = q_i$genre %>% unique()
  # Get the text id
  t = q_i$text %>% unique()
  # Get the clique id
  q = q_i$clique_id %>% unique()
  
  tibble(
    genre = genre,
    text = t,
    clique_id = q,
    v = mean(pairwise_indices_v[i,], na.rm = T),
    e = mean(pairwise_indices_e[i,], na.rm = T)
  )
}) %>%
  bind_rows()

# Join the results into a single table
indices = global_backward_cohesion %>%
  select(genre, text, clique_id, v, e) %>%
  mutate(index = 'global') %>%
  bind_rows(
    local_backward_cohesion %>%
      select(genre, text, clique_id, v, e) %>%
      mutate(index = 'local')
  ) %>%
  bind_rows(
    mean_pairwise_cohesion %>%
      select(genre, text, clique_id, v, e) %>%
      mutate(index = 'pairwise')
  )

# Write the results
# write_csv(indices, 'data/cohesion_indices.csv')

# CALCULATE THE COHESION INDICES WITH PORTIONS OF TEXTS AND PSEUDOTEXTS --------
