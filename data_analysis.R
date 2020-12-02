library(tidyverse)

song.data <- read_csv("./datasets/songs_lyrics_short.csv",
                      col_names = c(
                        "album",
                        "chart.date",
                        "love.song",
                        "lyrics",
                        "release.date",
                        "song.title",
                        "artist"
                      ),
                      col_types = cols(
                        album = col_character(),
                        chart.date = col_date("%d/%m/%Y"),
                        love.song = col_double(),
                        lyrics = col_character(),
                        release.date = col_date("%d/%m/%Y"),
                        song.title = col_character(),
                        artist = col_character()
                      ),
                      skip = 1)

song.data
