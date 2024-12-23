#' Get playlist information:
#' Makes a request to Deezer's API to get the tracks as a parsed R object.
#'
#' @param playlist_id a string representing the playlist ID
#' @return A parsed R object from [httr::content()]
#' @export
#' @examples request_playlist_info("13323984883")
request_playlist_info <- function(playlist_id = "13323984883") {
  # Get some metadata directly from the "playlist" object:
  playlist_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id))
  playlist_info <- httr::content(x = playlist_request, as = "parsed")
  
  # If length 1 it means that playlist_info only contains "$error"
  if(length(playlist_info) == 1) {
    stop(paste0("Error while retrieving the playlist: ", playlist_info$error$code))
  } else {
    return(playlist_info)
  }
}


#' Get playlist tracks:
#' Makes a request to Deezer's API to get the tracks as a parsed R object.
#'
#' @param playlist_id a string representing the playlist ID
#' @return A data.frame with the tracks information
#' @export
#' @examples request_tracks("13323984883")
request_tracks <- function(playlist_id = "13323984883") {
  tracks_request <- httr::GET(paste0("https://api.deezer.com/playlist/", 
                                     playlist_id, "/tracks/"))
  tracks_content <- httr::content(x = tracks_request, as = "parsed")
  
  # If length 1 it means that playlist_info only contains "$error"
  if(length(tracks_content) == 1) {
    stop(paste0("Error while retrieving the tracks: ", tracks_content$error$code))
  } else {
    tracks_list <- tracks_content$data
    
    while(is.null(tracks_content$`next`) == FALSE){
      tracks_request <- httr::GET(tracks_content$`next`)
      tracks_content <- httr::content(x = tracks_request, as = "parsed")
      tracks_list <- append(tracks_list, tracks_content$data)
    }
    # Normalising track lists: "title_version" is absent for some songs...
    for(i in c(1:length(tracks_list))) {
      tracks_list[[i]]$title_version <- NULL
    }
    
    return(data.frame(do.call("rbind", tracks_list)))
  }
}


#' Get the albums information:
#' Makes requests to Deezer's API to get the albums as a data.frame
#' Needed to get the genres, year of release and explicit lyrics information.
#'
#' @param tracks a dataframe with an "artist" column, can be obtained with 
#'               [DeeR::request_tracks]
#' @return A data.frame with the albums information
#' @export
request_albums <- function(tracks) {
  
  album_lists <- list()
  print("Requesting Albums info, this can take a while...")
  
  unique_albums <- unique(tracks[tracks$readable == TRUE, "album"])
  
  # Setting up a progress bar:
  pb <- txtProgressBar(max = length(unique_albums), style = 3)
  
  for(album in unique_albums){
    album_request <- httr::GET(paste0("https://api.deezer.com/album/", album$id))
    album_content <- httr::content(album_request)
    
    # Extract the list of genres from the album:
    genres <- list()
    if(length(album_content$genres$data) > 0){
      for(i in c(1:length(album_content$genres$data))){
        genres <- append(genres, album_content$genres$data[[i]]$name)
      }
    } else { genres <- "no genre" }
    
    if(length(album_content$title) == 0)           album_content$title <- "no title"
    if(length(album_content$release_date) == 0)    album_content$release_date <- "0000-00-00"
    if(length(album_content$explicit_lyrics) == 0) album_content$explicit_lyrics <- "Unknown"
    if(length(album_content$label) == 0)           album_content$label <- "Unknown"
    if(length(album_content$nb_tracks) == 0)       album_content$nb_tracks <- "Unknown"
    if(length(album_content$fans) == 0)            album_content$fans <- "Unknown"
    
    album_df <- data.frame(title        = album_content$title,
                           genre        = I(list(genres)),
                           release_date = album_content$release_date,
                           explicit     = album_content$explicit_lyrics,
                           label        = album_content$label,
                           nb_tracks    = album_content$nb_tracks,
                           duration     = album_content$duration,
                           fans         = album_content$fans)
    
    album_lists <- append(album_lists, list(album_df))
    
    setTxtProgressBar(pb, pb$getVal()+1)
    
    # Test with sleep to respect the API quotas (50 requests / second)
    # Todo: find a better way to implement this:
    Sys.sleep(0.021)
  }
  
  close(pb)
  
  return(data.frame(do.call("rbind", album_lists)))
}
