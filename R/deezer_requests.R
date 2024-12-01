#' Get playlist information:
#' Makes a request to Deezer's API to get the tracks as a parsed R object.
#'
#' @param playlist_id: a string representing the playlist ID
#' @return A parsed R object from [httr::content()]
#' @export
#' @examples get_playlist_info("6346527824")
request_playlist_info <- function(playlist_id = "6346527824") {
  # Get some metadata directly from the "playlist" object:
  playlist_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id))
  playlist_info <- httr::content(x = playlist_request, as = "parsed")
  
  # If length 1 it means that playlist_info only contains "$error"
  if(length(playlist_info) == 1) {
    stop(paste0("Error while retrieving the playlist, error code: ", playlist_info$error$code))
  } else {
    return(playlist_info)
  }
}

#' Get playlist tracks:
#' Makes a request to Deezer's API to get the tracks as a parsed R object.
#'
#' @param playlist_id: a string representing the playlist ID
#' @return A data.frame with the tracks information
#' @export
#' @examples get_tracks_list("6346527824")
request_tracks <- function(playlist_id = "6346527824") {
  tracks_request <- httr::GET(paste0("https://api.deezer.com/playlist/", playlist_id, "/tracks/"))
  tracks_content <- httr::content(x = tracks_request, as = "parsed")
  
  # If length 1 it means that playlist_info only contains "$error"
  if(length(tracks_content) == 1) {
    stop(paste0("Error while retrieving the tracks, error code: ", tracks_content$error$code))
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
