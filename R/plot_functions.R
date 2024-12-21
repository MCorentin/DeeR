#' Will plot the top n artists (in number of songs in the playlist).
#'
#' @param tracks a dataframe with an "artist" column, can be obtained with [DeeR::request_tracks]
#' @param n the number of artists to plot, default: 10
#' @return A ggplot barplot with the top artists by number of songs
#' @export
plot_top_artists <- function(tracks, n = 10) {
  artists <- data.frame(do.call("rbind", tracks$artist))
  artists$name <- as.character(artists$name)
  
  if(!is.numeric(n)) stop("Argument 'n' is not numeric")
  if(n > length(unique(artists$name))) n <- length(unique(artists$name))
  
  top_artists <- data.frame(table(artists$name)[order(table(artists$name), decreasing = T)[1:n]])
  
  gg_top_artists <- ggplot2::ggplot(data = top_artists, ggplot2::aes_string(x = "Var1", y = "Freq")) +
                      ggplot2::geom_bar(col = "black", fill = "deepskyblue4", alpha = 0.7, stat = "identity") + 
                      ggplot2::theme_bw() + ggplot2::xlab("") + ggplot2::ylab("Number of songs") +
                      ggplot2::ggtitle(paste0("Top artists (", length(unique(artists$name)), " artists)")) +
                      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  return(gg_top_artists)
}


#' Will plot the genres present in the playlist as a treemap.
#'
#' @param albums a data.frame obtained with [DeeR::request_albums]
#' @return A treemap of the genres present in the playlist
#' @export
plot_genres_treemap <- function(albums) {
  treemap_data <- data.frame(table(albums$genre))
  
  t <- treemap::treemap(dtf = treemap_data, 
                        index = c("Var1"),
                        vSize = "Freq", type = "index",
                        fontsize.labels = c(30, 12),
                        fontcolor.labels = c("white", "white"),
                        fontface.labels = c(2, 1), # 2 = bold, 1 = normal
                        bg.labels = c("transparent"),
                        align.labels = list(c("center", "center"),
                                           c("center", "center")),
                        inflate.labels = FALSE,
                        title = "Genres - Treemap")
  return(t)
}


#' Will plot the release dates of the albums present in the playlist.
#'
#' @param albums a data.frame obtained with [DeeR::request_albums]
#' @return A plot of the release dates
#' @export
plot_timeline <- function(albums) {
  release_years <- substr(x = albums$release_date, start = 1, stop = 4)
  timespan <- seq(min(release_years[release_years != "0000"]), max(release_years), by = 1)
  
  time_df <- as.data.frame(table(release_years))
  
  gg_time <- ggplot2::ggplot(data = time_df, ggplot2::aes_string(x = "release_years", y = "Freq")) +
    ggplot2::geom_bar(col = "black", fill = "deepskyblue4", alpha = 0.7, stat = "identity") + 
    ggplot2::theme_bw() + ggplot2::ylab("Number of albums") +
    ggplot2::ggtitle(paste0("Release Year (", nrow(time_df), " incredible albums)")) + 
    ggplot2::scale_x_discrete(name = "Release year", breaks = timespan, drop = FALSE)
  
  return(gg_time)
}

#' Will plot a wordcloud of the words present in the tracks title
#'
#' @param tracks a data.frame obtained with [DeeR::request_tracks]
#' @param n the maximum number of words to plot
#' @return A wordcloud of top n most frequent words
#' @export
plot_titles_wordcloud <- function(tracks, n = 50) {
  # Splitting the titles into words:
  title_words <- gsub(pattern = "'", replacement = " ", x = as.character(tracks$title_short))
  # Removing sentences in parenthesis:
  title_words <- gsub(pattern = " \\(.*\\)", replacement = "", x = title_words)
  # Split by spaces
  title_words <- unlist(strsplit(x = title_words, split = " "))
  # Lowering everything:
  title_words <- tolower(title_words)
  
  # Removing punctuation:
  title_words <- gsub(pattern = "[[:punct:]]", replacement = "", x = title_words)
  # Removing common english words:
  title_words <- gsub(pattern = "^don$|^the$|^of$|^on$|^in$|^it$|^if$|^to$|^so$|^for$|^les$|^des$|^le$|^sur$|^dans$", 
                      replacement = "", x = title_words, ignore.case = TRUE)

  
  wc <- wordcloud::wordcloud(words = title_words, 
                             max.words = n, 
                             min.freq = 1,  
                             rot.per = 0, 
                             colors = RColorBrewer::brewer.pal(8, "Paired"), 
                             random.order = FALSE)
  
  return(wc)
}

# fans

# bpm

# Albums mosaic