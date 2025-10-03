#' This script uses what's left of the spotify API to get artist albums and
#' full tracklists, before then passing these to ReccoBeats to get
#' audio features for each track. 
#' 
#' The first part of this script shows a step by step process for one specific
#' artist (Taylor Swift). Then there are generalised functions that should work
#' for any artist in ReccoBeats' catalogue. You can ignore the first part and 
#' jump straight to generalised functions if you want to use the script, but
#' remember to first set your spotify API client ID and secret
#' 
#' The final result from using get_recco_artist_track_features() is a dataset 
#' containing all of the tracks in an artist's discography (or at least, available
#' on Spotify). At the moment, this is limited to a maximum of 400 tracks,
#' but this can be increased by modifying the request_range object in the
#' batch_requests fuction.
#' 

library(tidyverse)
library(spotifyr)
library(httr)
library(jsonlite)

# Get all of the album names from a particular artist (in this example, Taylor Swift)
# This does require Spotify API authorisation, i.e. get your client ID and secret from
# developer.spotify.com
Sys.setenv(SPOTIFY_CLIENT_ID = "YOURCLIENTID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "YOURCLIENTSECRET")

taylor <- get_artist_albums(id = "06HL4z0CvFAxyc27GXpf02")

# I am removing duplicate/special editions of albums to keep this dataset simple
taylor <- taylor %>%
  filter(
    name %in% c("The Life of a Showgirl",
                "THE TORTURED POETS DEPARTMENT",
                "1989 (Taylor's Version)",
                "Speak Now (Taylor's Version)",
                "Midnights",
                "Red (Taylor's Version)",
                "Fearless (Taylor's Version)",
                "evermore",
                "folklore",
                "Lover",
                "reputation",
                "1989 (Deluxe)")
  ) %>%
  as_tibble()

taylor

# Loop through all albums to get track ids
taylor_albums <- taylor$id

# first album
taylor_tracks <- get_album_tracks(id = taylor$id[1]) %>% 
  mutate(album = taylor$name[1],
         release_date = taylor$release_date[1]
         ) # add name and release date of album to tracks tibble

# Remaining tracks
for (i in 2:length(taylor_albums)) {
  
  add_tracks <- get_album_tracks(id = taylor$id[i]) %>% 
    mutate(album = taylor$name[i],
           release_date = taylor$release_date[i])
  taylor_tracks <- bind_rows(taylor_tracks, add_tracks) # bind new tracks to old
  Sys.sleep(1) # rate limiter

  }

# Check all tracks were added
tibble(taylor_tracks)


# Use ReccoBeats API to fetch audio features (acousticness, loudness, danceability, etc.)

# API accepts comma separated track IDs from Spotify IDs or ReccoBeats IDS
# https://reccobeats.com/docs/documentation/request-and-response 

# Currently, the API can only return up to 40 tracks per request

batch_requests <- function (tracks) {
  # This function batches a dataset into chunks of 40 tracks
  
  request_range <- seq(0, 400, 40)
  request_tracks <- list()
  
  for (i in 1:11) {
  
    # Split request range up into chunks of 40 
    request_tracks[[i]] <- tibble(tracks)[(request_range[i]+1):(request_range[i+1]), ]
    request_tracks[[i]] <- request_tracks[[i]] %>% drop_na(id)
    
    # break the loop when all of the request subsets have been allocated
    if (nrow(request_tracks[[i]])==0) break
  
  }
  
  # Remove the excess empty list item
  request_tracks[[length(request_tracks)]] <- NULL
  
  return(request_tracks)

}

taylor_batches <- batch_requests(taylor_tracks)


# Make requests one batch at a time to reccobeats

recco_requests <- function(batches) {
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_results <- list()
  
  for (i in 1:length(batches)) {
    
    url <- paste0("https://api.reccobeats.com/v1/audio-features?ids=", paste0(batches[[i]]$id, collapse = ","))
  
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_results[[i]] <- as_tibble(fromJSON(content(res, "text"))$content) 
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_results)
  
}

taylor_results <- recco_requests(taylor_batches)

taylor_results

# Fetch popularity data from ReccoBeats

taylor_batches

headers = c(
  'Accept' = 'application/json'
)
url <- paste0("https://api.reccobeats.com/v1/track?ids=d4e23851-a6ee-4bb7-abea-0dc9b36865ba")
res <- VERB("GET", 
            url = url,
            add_headers(headers))

as_tibble(fromJSON(content(res, "text"))$content) %>%
  select(
    id, popularity
  )


recco_popularity <- function(results) {
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_popularity <- list()
  
  for (i in 1:length(results)) {
    
    url <- paste0("https://api.reccobeats.com/v1/track?ids=", paste0(results[[i]]$id, collapse = ","))
    
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_popularity[[i]] <- as_tibble(fromJSON(content(res, "text"))$content) %>%
      select(
        id, popularity
      )
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_popularity)
  
}

taylor_popularity <- recco_popularity(taylor_results)


# Now all lists can be flattened and joined

taylor_info <- bind_rows(taylor_batches) %>% mutate(artist = "Taylor Swift", .before = artists)
taylor_features <- bind_rows(taylor_results) %>% 
  rename(recco_id = id) %>% 
  mutate(
    # Create spotify ID from the url link to the taylor by removing the domain
    spotify_id = str_remove(href, pattern = "https://open.spotify.com/track/")
  ) %>%
  select(-href)
taylor_popularity <- bind_rows(taylor_popularity) %>%
  rename(recco_id = id)

taylor_data <- left_join(taylor_info, 
                         taylor_features, 
                         by = c("id" = "spotify_id")) %>%
               left_join(., taylor_popularity, by = "recco_id")


taylor_data


# Generalised functions ----------------------------------------------------


get_tracklist <- function(artist_id) {
  # Get all of the album names from a particular artist
  artist_albums <- get_artist_albums(id = artist_id)
  
  message("Albums found.")
  
  # Loop through all albums to get track ids
  artist_album_ids <- artist_albums$id
  
  # Get the first album to create the structure of the dataset
  artist_tracks <- spotifyr::get_album_tracks(id = artist_albums$id[1]) %>% 
    mutate(album = artist_albums$name[1],
           release_date = artist_albums$release_date[1]) # add name and release of album to tracks tibble
  
  # Remaining tracks
  for (i in 2:length(artist_album_ids)) {
    
    add_tracks <- get_album_tracks(id = artist_albums$id[i]) %>% 
      mutate(album = artist_albums$name[i],
             release_date = artist_albums$release_date[i])
    artist_tracks <- bind_rows(artist_tracks, add_tracks) # bind new tracks to old
    Sys.sleep(1) # rate limiter
    
  }
  
  message("Tracklist downloaded.")
  return(artist_tracks)
}

batch_requests <- function (tracks) {
  
  # Use ReccoBeats API to fetch audio features (acousticness, loudness, danceability, etc.)
  
  # API accepts comma separated track IDs from Spotify IDs or ReccoBeats IDS
  # https://reccobeats.com/docs/documentation/request-and-response 
  
  # Currently, the API can only return up to 40 tracks per request
  
  # This function batches a dataset into chunks of 40 tracks
  
  request_range <- seq(0, 400, 40)
  request_tracks <- list()
  
  for (i in 1:11) {
    
    # Split request range up into chunks of 40 
    request_tracks[[i]] <- tibble(tracks)[(request_range[i]+1):(request_range[i+1]), ]
    request_tracks[[i]] <- request_tracks[[i]] %>% drop_na(id)
    
    # break the loop when all of the request subsets have been allocated
    if (nrow(request_tracks[[i]])==0) break
    
  }
  
  # Remove the excess empty list item
  request_tracks[[length(request_tracks)]] <- NULL
  
  return(request_tracks)
  
}


recco_requests <- function(batches) {
  
  # Make requests one batch at a time to reccobeats
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_results <- list()
  
  for (i in 1:length(batches)) {
    
    url <- paste0("https://api.reccobeats.com/v1/audio-features?ids=", paste0(batches[[i]]$id, collapse = ","))
    
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_results[[i]] <- as_tibble(fromJSON(content(res, "text"))$content)
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_results)
  
}

recco_popularity <- function(results) {
  
  # Use a results from Recco list to fetch the popularity of tracks 
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_popularity <- list()
  
  for (i in 1:length(results)) {
    
    url <- paste0("https://api.reccobeats.com/v1/track?ids=", paste0(results[[i]]$id, collapse = ","))
    
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_popularity[[i]] <- as_tibble(fromJSON(content(res, "text"))$content) %>%
      select(
        id, popularity
      )
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_popularity)
  
}


# Single function to download features etc. from artist id. 
# Artist name argument is to add the "main artist", rather than 
# having vectors of multiple artists
get_recco_artist_track_features <- function(artist_name, 
                                            artist_id) {

  # Get the complete list of track names from all albums
  # by the artist
  artist_tracks <- get_tracklist(artist_id)
  
  # Group the tracks into sets of 40 for making requests to ReccoBeats
  artist_batches <- batch_requests(artist_tracks)
  
  # Make calls to ReccoBeats API to download track audio features
  artist_results <- recco_requests(artist_batches)
  message("Recco audio features downloaded.")
  
  artist_track_popularity <- recco_popularity(artist_results)
  message("Recco track popularity downloaded.")
  
  # Now all lists can be flattened, tidied, and joined
  track_info <- bind_rows(artist_batches) %>% mutate(artist = artist_name, .before = artists)
  track_features <- bind_rows(artist_results) %>% 
                      rename(recco_id = id) %>% 
                      mutate(
                        # Create spotify ID from the url link to the track
                        spotify_id = str_remove(href, "https://open.spotify.com/track/")
                      ) %>%
                      select(-href)
  track_popularity <- bind_rows(artist_track_popularity) %>%
    rename(recco_id = id)
  
  artist_data <- left_join(track_info, 
                           track_features, 
                           by = c("id" = "spotify_id")) %>%
                 left_join(., track_popularity, by = "recco_id")
  
  message("Complete.")
  return(artist_data)

}

# Test using Fleetwood Mac's Discography

fleetwood_mac <- get_recco_artist_track_features(artist_name = "Fleetwood Mac",
                                artist_id = "08GQAI4eElDnROBrJRGE0X")

# Test using Arctic Monkey's Discography

arctic_monkeys <- get_recco_artist_track_features(artist_name = "Arctic Monkeys",
                                                 artist_id = "7Ln80lUS6He07XvHI8qqHH")


frank_ocean <- get_recco_artist_track_features(artist_name = "Frank Ocean",
                                               artist_id = "2h93pZq0e7k5yf4dywlkpM")


# Data is not as comprehensive as the old Spotify API using spotifyr, but is fine.

# Save datasets

taylor_data <- taylor_data %>%
  select(-artists, -available_markets, -preview_url)

fleetwood_mac <- fleetwood_mac %>%
  select(-artists, -available_markets, -preview_url)

arctic_monkeys <- arctic_monkeys %>%
  select(-artists, -available_markets, -preview_url)


# Data tidying

unique(fleetwood_mac$album)

fleetwood_mac <- fleetwood_mac %>%
  filter(
    !album %in% c("Live From The Record Plant (December 15, 1974)",
                  "Mirage Tour '82 (Live)",
                  "Rumours Live",
                  "The Shape I'm In (Live 1972)",
                  "Fleetwood Mac 1975 to 1987",
                  "Before the Beginning - 1968-1970 Rare Live & Demo Sessions (Remastered)",
                  "Tango In the Night (Deluxe Edition)",
                  "Mirage (Deluxe Edition)",
                  "Live (Deluxe Edition)",
                  "50 Years - Don't Stop")
  )

unique(arctic_monkeys$album)

arctic_monkeys <- arctic_monkeys %>%
  filter(
    album %in% c(
      "The Car",
      "Tranquility Base Hotel & Casino",
      "AM",
      "Suck It and See",
      "Humbug",
      "Favourite Worst Nightmare",
      "Whatever People Say I Am, That's What I'm Not"
    )
  )

taylor_data <- read_csv("artist_data/taylor_swift_2026.csv")

# Update key and mode
taylor_data <- taylor_data %>%
  mutate(
    key = case_when(
      is.na(key) ~ NA_character_,
      key == 0 ~ "C",
      key == 1 ~ "C#",
      key == 2 ~ "D",
      key == 3 ~ "D#",
      key == 4 ~ "E",
      key == 5 ~ "F",
      key == 6 ~ "F#",
      key == 7 ~ "G",
      key == 8 ~ "G#",
      key == 9 ~ "A",
      key == 10 ~ "A#",
      key == 11 ~ "B"
    ),
    mode = case_when(
      is.na(mode) ~ NA_character_,
      mode == 1 ~ "Major",
      mode == 0 ~ "Minor"
    )
  )

fleetwood_mac <- fleetwood_mac %>%
  mutate(
    key = case_when(
      is.na(key) ~ NA_character_,
      key == 0 ~ "C",
      key == 1 ~ "C#",
      key == 2 ~ "D",
      key == 3 ~ "D#",
      key == 4 ~ "E",
      key == 5 ~ "F",
      key == 6 ~ "F#",
      key == 7 ~ "G",
      key == 8 ~ "G#",
      key == 9 ~ "A",
      key == 10 ~ "A#",
      key == 11 ~ "B"
    ),
    mode = case_when(
      is.na(mode) ~ NA_character_,
      mode == 1 ~ "Major",
      mode == 0 ~ "Minor"
    )
  )

arctic_monkeys <- arctic_monkeys %>%
  mutate(
    key = case_when(
      is.na(key) ~ NA_character_,
      key == 0 ~ "C",
      key == 1 ~ "C#",
      key == 2 ~ "D",
      key == 3 ~ "D#",
      key == 4 ~ "E",
      key == 5 ~ "F",
      key == 6 ~ "F#",
      key == 7 ~ "G",
      key == 8 ~ "G#",
      key == 9 ~ "A",
      key == 10 ~ "A#",
      key == 11 ~ "B"
    ),
    mode = case_when(
      is.na(mode) ~ NA_character_,
      mode == 1 ~ "Major",
      mode == 0 ~ "Minor"
    )
  )


if (dir.exists("artist_data") == FALSE) {
  dir.create("artist_data")
  write_csv(taylor_data, "artist_data/taylor_swift_2026.csv")
  write_csv(fleetwood_mac, "artist_data/fleetwood_mac_2026.csv")
  write_csv(arctic_monkeys, "artist_data/arctic_monkeys_2026.csv")
} else {
  write_csv(taylor_data, "artist_data/taylor_swift_2026.csv")
  write_csv(fleetwood_mac, "artist_data/fleetwood_mac_2026.csv")
  write_csv(arctic_monkeys, "artist_data/arctic_monkeys_2026.csv")
} 

