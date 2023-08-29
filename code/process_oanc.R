# Load required packages
library(tidyverse)
library(SnowballC)

# Directory with the OANC xml files
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
    filter(pos %in% c('CD', 'FW', 'JJ', 'JJR', 'JJS', 'NN', 'NNS', 'NNP', 'NNPS',
                      'RB', 'RBR', 'RBS', 'VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ'))
}) %>%
  bind_rows()

# Write OANC procesed data
write_csv(oanc_data, 'data/oanc_data.csv')
