# Load required packages
library(tidyverse)
library(rJava)
library(wordnet)

# Set Wordnet English dictionary
setDict('data/wn3.1.dict/dict/')

# Get unique lemmas with POS
lemmas = corpus %>%
  distinct(lemma, pos)

synonyms_hypermyns = mapply(function(lemma, pos) {
  print(lemma)
  if(pos %in% c('VB', 'VBD', 'VBG', 'VBN', 'VBP', 'VBZ')) {
    synonyms(lemma, 'VERB') %>%
      paste(collapse = '|')
  } else if (pos %in% c('NN', 'NNS', 'NNP', 'NNPS')) {
    synonyms(lemma, 'NOUN') %>%
      paste(collapse = '|')
  } else if (pos %in% c('ADJ', 'JJ', 'JJR', 'JJS')) {
    synonyms(lemma, 'ADJECTIVE') %>%
      paste(collapse = '|')
  } else if (pos %in% c('ADV', 'RB', 'RBR', 'RBS')) {
    synonyms(lemma, 'ADVERB') %>%
      paste(collapse = '|')
  } else {
    NA
  }
}, lemmas$lemma[1:2], lemmas$pos[1:2])
