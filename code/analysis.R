# Load required packages
library(tidyverse)
library(SnowballC)
library(wordnet)

# Process OANC XML files -------------------------------------------------------

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

# Get synonyms and hypernyms from Wordnet --------------------------------------

# Set Wordnet English dictionary
setDict('data/wn3.1.dict/dict/')

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
             paste(collapse = '|'),
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
               paste0(collapse = '|')
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

# Generate pseudotexts ---------------------------------------------------------

# To use the same pseudotexts reported in the study, uncomment the code below
# instead of running lines 131-154
# pseudotexts = read_csv('data/oanc_pseudotexts_study.csv')

# Get the number of texts in the corpus
corpus_size = corpus$text %>% unique() %>% length()

# Generate pseudotexts
pseudotexts = lapply(1:corpus_size, function(i) {
  corpus %>%
    # Attribute an unique id for each clique in the corpus
    unite('text_clique_id', text:clique_id, remove = F) %>%
    filter(text_clique_id %in% (
      # Get a random sample of cliques, one per text
      corpus %>%
        group_by(text) %>%
        sample_n(1) %>%
        unite('text_clique_id', text:clique_id) %>%
        .$text_clique_id)) %>%
    group_by(text_clique_id) %>%
    # Add unique id to pseudotext and reset clique_id
    mutate(genre = 'pseudotext',
           text = paste0('pseudotext_', str_pad(i, 2, 'left', '0')), 
           clique_id = cur_group_id()) %>%
    ungroup() %>%
    select(-text_clique_id)
}) %>%
  View()
bind_rows()

# Write pseudotexts
# write_csv(pseudotexts, 'data/oanc_pseudotexts.csv')

# Join the data ----------------------------------------------------------------

data = corpus %>% 
  bind_rows(pseudotexts) %>%
  left_join(synonyms_hypernyms, by = c('lemma', 'pos')) %>%
  distinct(text, clique_id, lemma, .keep_all = T)

