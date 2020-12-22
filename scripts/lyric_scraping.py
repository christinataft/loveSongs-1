import lyricsgenius
import pandas as pd
import re
import datetime

songData=pd.read_csv('.\\datasets\\songs_labelled.csv')

print(songData)

allSongData=pd.DataFrame()

api = lyricsgenius.Genius(# Insert your genius api key here ,
    sleep_time=0.01, verbose=False)

startTime = datetime.datetime.now()

for i in range(0, len(songData)):

    passed = str(datetime.datetime.now() - startTime)
    print(passed + " has passed.")
    rollingPct = int((i/len(songData))*100)
    print(str(rollingPct) + "% complete.")
    songTitle = songData.iloc[i]['song']
    songTitle = re.sub(" and ", " & ", songTitle)
    artistName = songData.iloc[i]['artist']
    artistName = re.sub(" and | \\+ | Featuring ", " & ", artistName) # To account for the difference between how billboard and spotify does stuff
    label = songData.iloc[i]['loveSong']

    try:
        song = api.search_song(songTitle, artist=artistName)
        print("scraping for " + songTitle)
        songAlbum = song.album
        songLyrics = re.sub("\n", " ", song.lyrics) #Remove newline breaks, we won't need them.
        songYear = song.year

    except AttributeError:  # if song not found
        print(songTitle + " not found!")
        songAlbum = "null"
        songLyrics = "null"
        songYear = "null"

    except TypeError:  # if connection timed out
        print("Connection timed out! Writing scraped songs to csv")
        break

    row = {
        "Chart date": songData.iloc[i]['date'],
        "Song Title": songData.iloc[i]['song'],
        "Artist": songData.iloc[i]['artist'],
        "Album": songAlbum,
        "Lyrics": songLyrics,
        "Release Date": songYear,
        "Love Song": label
    }
    allSongData = allSongData.append(row, ignore_index=True)

allSongData.to_csv(".\\datasets\\songs_lyrics_labelled.csv", index=False)
