# Clean environment; Import libraries
rm(list=ls())
# Set working directory
dir <- ("/Users/au564346/Documents/research/unearned_wealth/david/HTST_Hist/new/plays/tagged/other/")
wd <- setwd(dir)
# Libraries
library(readtext)
library(tidyverse)
library(readxl)
library(ggplot2)

# Function to split list of results into 100, sum raw counts
get_binned_values <- function(raw_values, bins = 100){
  if(!is.numeric(raw_values) | !is.numeric(bins)) stop("Input must be a numeric vector")
  if(length(raw_values)/bins < 2){
    stop("Input vector needs to be twice as long as value number")
  }
  chunks <- split(raw_values, cut(1:length(raw_values),bins))
  means <- sapply(chunks, sum)
  names(means) <- 1:bins
  return(means)
}

# Function to extract desired semantic tags from all plays
extractTag <- function(df, tag){
  for (i in (df.list)) {
    raw <- as.numeric(ifelse(grepl(tag, df$HTST), "1", "0"))
    return(sum(raw))
  }
}

# Function to extract desired semantic tags from all plays
extractTag <- function(df, tag) {
    for (i in tag) {
    raw <- as.numeric(ifelse(grepl(tag, df$HTST), "1", "0"))
    return(sum(raw))
  }
}
# Rescale function stolen from Jockers syuzhet package
rescale_x_2 <- function(v){
  x <- 1:length(v)/length(v)
  y <- v/max(v)
  z <- 2 * (v - min(v))/(max(v) - min(v)) - 1
  return (list(x=x,y=y,z=z))
}

## FULL SET ##
# Get list of files
file.list <- list.files(wd , pattern = "*.xlsx")
filenames <- tools::file_path_sans_ext(file.list)
df.list <- lapply(file.list, read_excel)
tbl <- cbind(filenames)

# Use extractTag function on all plays; list of counts
#listed <- lapply(df.list, "AU.12", FUN=extractTag)
# To DF
#df_for_plot <- do.call(rbind.data.frame, listed) %>%
#                as.tibble %>%
#                  rename_("AU.12" = names(.)[1])
# Add filenames
#tbl <- cbind(tbl, df_for_plot)
#tbl <- tbl %>% as.tibble
#write_csv(tbl, "/Users/au564346/Desktop/table.csv",
#          append = FALSE, col_names= TRUE)
# Set viz dimensions
#op <- par(mar=c(11,4,4,2))
# Barplot across all plays
#barplot(as.vector(tbl$GoodBad), names.arg = tbl$filenames, las=2, col="skyblue", main='Trade and Finance')
##################

# Split positive and negative emotion
cols <- c("filenames", "AU.01.a", "AU.01.b", "AU.02", "AU.04", "AU.05.c", "AU.05.d", "AU.14", "AU.14.a", "AU.14.b", "AU.15", "AU.15.a", "AU.15.a.01", "AU.15.a.02", "AU.15.a.03",
            "AU.16", "AU.17", "AU.18", "AU.18.a", "AU.18.b", "AU.19", "AU.20", "AU.21", "AU.21.a", "AU.21.b", "AU.22", "AU.22.a", "AU.22.b", "AU.23", "AU.24", "AU.25", "AU.25.a",
              "AU.25.b", "AU.25.c", "AU.26", "AU.31", "AU.31.a", "AU.31.a.01", "AU.32", "AU.32.a", "AU.32.b", "AU.34.a", "AU.35", "AU.36.a", "AU.37", "AU.37.a", "AU.37.b", "AU.37.d",
                "AU.37.e", "AU.37.f", "AU.37.g", "AU.37.h", "AU.37.i", "AU.40", "AU.40.a", "AU.40.b", "AU.42", "AU.43", "AU.43.a", "AU.43.a.01", "AU.43.a.02", "AU.44", "AU.45", "AU.46", "AU.46.a")
cols2 <- c("filenames", "AU.01.a", "AU.01.b", "AU.02", "AU.04", "AU.05", "AU.05.a", "AU.05.b", "AU.06", "AU.07", "AU.07.a", "AU.08", "AU.09", "AU.10", "AU.10.a", "AU.11", "AU.11.a", "AU.11.b",
            "AU.11.c", "AU.12", "AU.12.a", "AU.13", "AU.13.a", "AU.27", "AU.27.a", "AU.27.b", "AU.27.c", "AU.27.d", "AU.28", "AU.28.a", "AU.28.b", "AU.29", "AU.29.a", "AU.29.b", "AU.29.c", "AU.29.d",
              "AU.30", "AU.34", "AU.36", "AU.37.c", "AU.47", "AU.47.a", "AU.47.a.01", "AU.47.b", "AU.47.c", "AU.47.d")
neg_tags <- c("AU.01.a", "AU.01.b", "AU.02", "AU.04", "AU.05.c", "AU.05.d", "AU.14", "AU.14.a", "AU.14.b", "AU.15", "AU.15.a", "AU.15.a.01", "AU.15.a.02", "AU.15.a.03", "AU.16", "AU.17", "AU.18",
              "AU.18.a", "AU.18.b", "AU.19", "AU.20", "AU.21", "AU.21.a", "AU.21.b", "AU.22", "AU.22.a", "AU.22.b", "AU.23", "AU.24", "AU.25", "AU.25.a", "AU.25.b", "AU.25.c", "AU.26", "AU.31", "AU.31.a",
                "AU.31.a.01", "AU.32", "AU.32.a", "AU.32.b", "AU.34.a", "AU.35", "AU.36.a", "AU.37", "AU.37.a", "AU.37.b", "AU.37.d", "AU.37.e", "AU.37.f", "AU.37.g", "AU.37.h", "AU.37.i", "AU.40", "AU.40.a",
                  "AU.40.b", "AU.42", "AU.43", "AU.43.a", "AU.43.a.01", "AU.43.a.02", "AU.44", "AU.45", "AU.46", "AU.46.a")
pos_tags <- c("AU.01.a", "AU.01.b", "AU.02", "AU.04", "AU.05", "AU.05.a", "AU.05.b", "AU.06", "AU.07", "AU.07.a", "AU.08", "AU.09", "AU.10", "AU.10.a", "AU.11", "AU.11.a", "AU.11.b", "AU.11.c", "AU.12",
              "AU.12.a", "AU.13", "AU.13.a", "AU.27", "AU.27.a", "AU.27.b", "AU.27.c", "AU.27.d", "AU.28", "AU.28.a", "AU.28.b", "AU.29", "AU.29.a", "AU.29.b", "AU.29.c", "AU.29.d", "AU.30", "AU.34", "AU.36",
                "AU.37.c", "AU.47", "AU.47.a", "AU.47.a.01", "AU.47.b", "AU.47.c", "AU.47.d")

## LOOPED ##
for(tag in neg_tags) {
  listed <- lapply(df.list, tag, FUN=extractTag)
  # To DF
  df_for_plot <- do.call(rbind.data.frame, listed) %>%
                    as.tibble %>%
                      rename_(tag = names(.)[1])
  # Add filenames
  tbl <- cbind(tbl, df_for_plot)
}
names(tbl) <- cols
tbl <- tbl %>% as.tibble
write_csv(tbl, "/Users/au564346/Desktop/Non-Shakespeare_Negative.csv",
          append = FALSE, col_names= TRUE)

## LOOPED ##
for(tag in pos_tags) {
  listed <- lapply(df.list, tag, FUN=extractTag)
  # To DF
  df_for_plot <- do.call(rbind.data.frame, listed) %>%
    as.tibble %>%
    rename_(tag = names(.)[1])
  # Add filenames
  tbl <- cbind(tbl, df_for_plot)
}
names(tbl) <- cols2
tbl <- tbl %>% as.tibble
write_csv(tbl, "/Users/au564346/Desktop/Non-Shakespeare_Positive.csv",
          append = FALSE, col_names= TRUE)

#########################

## SINGLE PLAY ANALYSIS ##
# Import single play
df <- read_excel("/Users/au564346/Documents/research/unearned_wealth/HTST_Hist/HTST_Histories/HTST_Histories/tagged/other/Marlowe.EdwardII.A07018.xlsx") %>% as.tibble

# Find desired semantic tag
df_raw <- as.numeric(ifelse(grepl("AU.15", df$HTST), "1", "0"))
sum(df_raw)

# Find scene divisions
#scenes <- ifelse(grepl("Scene", df$TOKEN), "1", "0")
# Split text into 100 equal size chunks
binned_values <- get_binned_values(df_raw, 100)
# Barplot
barplot(binned_values)

# Scaled values
awdw <- round(length(df_raw)*.05)
rolled <- rescale_x_2(zoo::rollmean(df_raw, k=awdw))

# Scaled plotting
par(mfrow=c(1,1))
plot(rolled, type='l', ylab="Instances per slice", xlab="Percentage of play", col='black',
     main="Sorrow, grief in Richard II")
lines(rolled, type='l', col='red')
legend("topleft", legend=c("TIME", "ACTION", "EMOTION"),
       col=c("black", "red", "blue"), lty=1:2, cex=0.8)
legend("topleft", legend=c("The Demoiselle", "Michaelmas Term"),
       col=c("red", "black"), lty=1:2, cex=0.8)

all_emotions <- as.numeric(ifelse(grepl("AU", df$HTST), "1", "0"))
negative <- as.numeric(ifelse(grepl(paste(neg_tags, collapse="|"), pride$HTST), "1", "0"))
positive <- as.numeric(ifelse(grepl(paste(pos_tags, collapse="|"), df$HTST), "1", "0"))

sum(negative)
sum(positive)

binned_all <- get_binned_values(all_emotions, 10)
binned_neg <- get_binned_values(negative, 10)
binned_pos <- get_binned_values(positive, 10)

# Simple plot
plot(binned_values, type='b', ylab="Instances per slice", xlab="Percentage of play")
lines(binned_values, type='b', col='black')
lines(binned_pos, type='b', col='blue')
legend("topleft", legend=c("Positive", "Negative"),
       col=c("red", "black"), lty=1:2, cex=0.8)
