# Load required packages
library(tidyverse)
library(wordnet)

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
     pos = pos_wordnet,
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
       return(NA)
     }))
  }
}, lemmas$lemma, lemmas$pos, SIMPLIFY = F) %>%
  bind_rows()

# Write lemmas with synonyms and hypernyms
write_csv(synonyms_hypernyms, 'data/synonyms_hypernyms.csv')
