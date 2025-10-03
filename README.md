# spotifyr and ReccoBeats artist track and audio features script

This script uses what's left of the spotify API to get artist albums and full tracklists, before then passing these to ReccoBeats to get audio features for each track.

The first part of this script shows a step by step process for one specific artist (Taylor Swift). Then there are generalised functions that should work for any artist in ReccoBeats' catalogue. You can ignore the first part and jump straight to generalised functions if you want to use the script, but remember to first set your spotify API client ID and secret.

The final result from using get_recco_artist_track_features() is a dataset containing all of the tracks in an artist's discography (or at least, available on Spotify). At the moment, this is limited to a maximum of 400 tracks, but this can be increased by modifying the request_range object in the batch_requests fuction.

## Usage

* Set your spotify client ID and client secret using `Sys.setenv()`
* Navigate to "Generalised Functions" and define all of the functions in your working environment
* Use `get_recco_artist_track_features()` to download the discography, audio features, and popularity of tracks for a given artist, e.g. `get_recco_artist_track_features(artist = "Taylor Swift", artist_id = "06HL4z0CvFAxyc27GXpf02")`

