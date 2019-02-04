rm(list=ls())
library(tidyverse)
library(xlsx)

table <- read.csv2("/Users/au564346/Desktop/Pos_Emo.csv", header = TRUE, sep='\t')

table['significance'] <- apply(table[2:5], 1, function(x) fisher.test(matrix(x, nr=2))$p.value)

write.table(table, "/Users/au564346/Desktop/PositiveEmotions_EmotionWordsOnly.csv", sep='\t')

table
