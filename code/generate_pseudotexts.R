# Load required packages
library(tidyverse)

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
  bind_rows()

# Write pseudotexts
write_csv(pseudotexts, 'data/oanc_pseudotexts.csv')
