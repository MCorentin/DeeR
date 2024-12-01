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
  
  top_artists <- data.frame(table(artists$name)[order(table(artists$name), decreasing = T)[1:n]])
  
  gg_top_artists <- ggplot(data = top_artists, aes(x = Var1, y = Freq)) +
                      geom_bar(col = "black", fill = "deepskyblue4", alpha = 0.7, stat = "identity") + 
                      theme_bw() +  
                      ggtitle(paste0(user, "'s ", playlist_info$title, " - Top artists (", length(unique(artists$name)), " artists)")) +
                      theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  return(gg_top_artists)
}