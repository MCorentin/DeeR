# DeeR (Deezer exploreR)

This is the repository for DeeR, a R (and soon python) package to explore a Deezer playlist.

## Table of Contents

 - [DeeR](#deer)
    - [Installation](#installation)
    - [Running DeeR](#running-deer)

## Installation 

DeeR is available on GitHub, at [https://github.com/MCorentin/DeeR](https://github.com/MCorentin/DeeR "DeeR GitHub page").

### Dependencies

DeeR needs the following: 
 - **R** (tested on version v4.3.3)
 - **An internet connection**
 - **The following R libraries:** (in parenthesis are the versions tested during development)
    - treemap   (v2.4-4)
    - ggplot2   (v2_3.5.1)
    - httr      (v1.4.7)
    - wordcloud (v2.6)

### Install DeeR with devtools

The easiest way to get DeeR is to install it directly from R using the package “devtools”:

````R
install.packages("devtools")
library("devtools")
install_github(repo = "MCorentin/DeeR", dependencies = TRUE)
library("DeeR")
````

### Install DeeR from source

Alternatively you can clone the GitHub repository:
````shell
git clone https://github.com/MCorentin/DeeR
````

Then open R and install the package script from source:
````R
library("utils")
install.packages("./DeeR/", repos = NULL, type = "source")
````

## Running DeeR

To use DeeR you only need a playlist ID. This can be found in the playlist URL just after *playlist/*.

eg: "13323984883" for [https://www.deezer.com/fr/playlist/13323984883](https://www.deezer.com/fr/playlist/13323984883 "Brutus playlist <3")

Then you can use DeeR functions. The most important objects you will need are produced with *request_tracks* and *request_albums*.

```R
if(!require("devtools")) install.packages("devtools")
if(!require("DeeR")) devtools::install_github(repo = "MCorentin/DeeR", dependencies = TRUE)

playlist_id <- "13323984883"

playlist_info <- request_playlist_info(playlist_id)

user <- playlist_info$creator$name
title <- playlist_info$title

tracks <- request_tracks(playlist_id)
albums <- request_albums(tracks) # This can take a while!
```

Once you have the tracks and/or albums data.frames, you can use DeeR to generate plots:

The top artists of the playlist:

```R
plot_top_artists(tracks)
```

![Top Artists](./images/top_artists_plot.png|width=10)

The genres per album of the playlist:

```R
plot_genres_treemap(albums)
```

![Genres](./images/genres_treemap.png|width=10)

Plotting a Bubble chart of the most popular albums per year:

```R
plot_bubble_fans(albums)
```

![Bubble](./images/genres_bubble.png|width=10)

Word cloud of the most present words in the playlist titles:

```R
plot_titles_wordcloud(tracks)
```

![Wordcloud](./images/wordcloud.png|width=10)


