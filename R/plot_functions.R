#' Will plot the top n artists (in number of songs in the playlist).
#'
#' @param tracks: a dataframe with an "artist" column, can be obtained with [DeeR::request_tracks]
#' @param n: the number of artists to plot, default: 10
#' @return A ggplot barplot with the top artists by number of songs
#' @export
plot_top_artists <- function(tracks, n = 10) {
  artists <- data.frame(do.call("rbind", tracks$artist))
  artists$name <- as.character(artists$name)
  
  if(!is.numeric(n)) stop("Argument 'n' is not numeric")
  if(n > length(unique(artists$name))) n <- length(unique(artists$name))
  
  top_artists <- data.frame(table(artists$name)[order(table(artists$name), decreasing = T)[1:n]])
  
  gg_top_artists <- ggplot2::ggplot(data = top_artists, ggplot2::aes(x = Var1, y = Freq)) +
                      ggplot2::geom_bar(col = "black", fill = "deepskyblue4", alpha = 0.7, stat = "identity") + 
                      ggplot2::theme_bw() + ggplot2::xlab("") + ggplot2::ylab("Number of songs") +
                      ggplot2::ggtitle(paste0(user, "'s ", playlist_info$title, " - Top artists (", length(unique(artists$name)), " artists)")) +
                      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5))
  return(gg_top_artists)
}


#' Will plot the genres present in the playlist as a treemap.
#'
#' @param albums: a data.frame obtained with [DeeR::request_albums]
#' @return A treemap of the genres present in the playlist
#' @export
plot_genres_treemap <- function(albums) {
  treemap_data <- data.frame(table(albums$genre))
  
  t <- treemap::treemap(treemap_data, 
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
