#' Plot the top n artists (in number of songs in the playlist).
#'
#' @param tracks A dataframe with an "artist" column, can be obtained with [DeeR::request_tracks]
#' @param n The number of artists to plot, default: 10
#' @return A ggplot barplot with the top artists by number of songs
#' @export
plot_top_artists <- function(tracks, n = 10) {
  artists <- data.frame(do.call("rbind", tracks$artist))
  artists$name <- as.character(artists$name)
  
  if(!is.numeric(n)) stop("Argument 'n' is not numeric")
  if(n > length(unique(artists$name))) n <- length(unique(artists$name))
  
  top_artists <- data.frame(table(artists$name)[order(table(artists$name), decreasing = T)[1:n]])
  
  gg_top_artists <- ggplot2::ggplot(data = top_artists, 
                                    ggplot2::aes_string(x = "Var1", y = "Freq")) +
                    ggplot2::geom_bar(col = "black", 
                                      fill = "deepskyblue4", 
                                      alpha = 0.7,
                                      stat = "identity") + 
                    ggplot2::theme_bw() + ggplot2::xlab("") + 
                    ggplot2::ylab("Number of songs") +
                    ggplot2::ggtitle(paste0("Top artists (", length(unique(artists$name)), " artists)")) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  return(gg_top_artists)
}


#' Plot the genres present in the playlist as a treemap.
#'
#' @param albums A data.frame obtained with [DeeR::request_albums]
#' @return A treemap of the genres present in the playlist
#' @export
plot_genres_treemap <- function(albums) {
  treemap_data <- data.frame(table(unlist(albums$genre)))
  
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


#' Plot the release dates of the albums present in the playlist.
#'
#' @param albums A data.frame obtained with [DeeR::request_albums]
#' @return A plot of the release dates
#' @export
plot_timeline <- function(albums) {
  
  release_years <- as.numeric(substr(x = albums$release_date, start = 1, stop = 4))
  timespan <- seq(min(release_years[release_years != "0000"]), max(release_years), by = 1)
  
  # Keeping only the first "genre" of every album: possible source of bias:
  time_df <- data.frame(year = release_years,
                        genre = unlist(lapply(albums$genre, '[[', 1)))
  
  gg_time <- ggplot2::ggplot(time_df, ggplot2::aes_string(x = "year", fill = "genre")) +
               ggplot2::geom_dotplot(col = "black",
                                     alpha = 0.7,
                                     dotsize = 0.7,
                                     binwidth = 1,
                                     drop = FALSE,
                                     stackgroups = TRUE) +
              ggplot2::theme_bw() + 
              ggplot2::ggtitle(paste0("Release Year (", nrow(time_df), " incredible albums)")) + 
              ggplot2::scale_x_continuous(name = "Release year", breaks = time_df$year) +
              ggplot2::scale_fill_discrete(breaks = names(head(sort(table(time_df$genre), decreasing = T), 5))) +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
  
  return(gg_time)
}

#' Plot a wordcloud of the words present in the tracks title
#'
#' @param tracks A data.frame obtained with [DeeR::request_tracks]
#' @param n The maximum number of words to plot
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

#' #' Will plot the distribution of the BPMs in a playlist as a beeswarm !
#' #'
#' #' @param tracks A data.frame obtained with [DeeR::request_tracks]
#' #' @return a beeswarm of the BPMs
#' #' @export
#' plot_BPMs <- function(tracks) {
#'   beeswarm(x = iris$Sepal.Length,
#'     pch = 16,
#'     col = "blue")
#' }

# fans
# The rank is a global indicator of a song's popularity on DEezer, who goes from 0 to 1M. 

# Albums mosaic ?

#' Plot a bubble chart of the number of fans per album 
#'
#' @param albums A data.frame obtained with [DeeR::request_albums]
#' @return A bubble plot of the albums per genre with size corresponding to the number of fans
#' @export
plot_bubble_fans <- function(albums) {
  
  release_years <- as.numeric(substr(x = albums$release_date, start = 1, stop = 4))
  
  bubble_df <- data.frame(year = release_years,
                          genre = unlist(lapply(albums$genre, '[[', 1)),
                          fans = albums$fans / 1000)
  gg_bubble <- 
  ggplot2::ggplot(bubble_df, aes_string(x = "year", y = "genre", col = "genre", size = "fans")) + 
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("Release Year (", nrow(bubble_df), " incredible albums) - Number in 1000s of fans")) + 
    ggplot2::scale_x_continuous(name = "Release year", breaks = bubble_df$year) +
    ggplot2::scale_color_discrete(breaks = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5)) +
    ggplot2::scale_size(range = c(1, 15), name = "Fans")
  
  return(gg_bubble)
}

# # Duration + number of tracks heatmap
# 
# # Waffle chart of labels ?
# plot_top_labels <- function(albums, n = 8) {
#   
#   top_labels <- as.data.frame(head(sort(x = table(albums$label), decreasing = T), n))
#   
#   waffle::waffle(top_labels)
# }