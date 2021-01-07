library(tidyverse)
library(lubridate)

## get list of existing race data
filenames <- list.files(path = "data/", pattern="*.csv")

## remove old race data and replace with new race data
file.remove(file.path("data", filenames))
temp <- tempfile(fileext = ".zip")
download.file(url = "http://ergast.com/downloads/f1db_csv.zip", destfile = temp)
unzip(zipfile = temp, exdir = "data")
rm(temp)

filenames_csv <- paste0("data/", filenames)

dat <- filenames_csv %>% 
  lapply(read.csv, header = T)

## name individual data frames
filenames <- gsub(pattern = "\\..*", replacement = "", x = filenames)
names(dat) <- filenames

dat <- lapply(dat, function(x){
  x[] <- lapply(x, function(x) replace(x, x %in% "\\N", NA))
  return(x)
})

## clean workspace
rm(filenames, filenames_csv)

## data cleaning
dat[["circuits"]] <- dat[["circuits"]] %>% 
  select(-alt, -url)
dat[["constructor_results"]] <- dat[["constructor_results"]] %>% 
  select(-status)
dat[["constructors"]] <- dat[["constructors"]] %>% 
  select(-url)
dat[["driver_standings"]] <- dat[["driver_standings"]] %>% 
  select(-positionText)
dat[["drivers"]] <- dat[["drivers"]] %>% 
  select(-code, -number, -url) %>% 
  mutate(dob = dob %>% as.POSIXct(tz = "UTC"))
dat[["lap_times"]] <- dat[["lap_times"]] %>% 
  mutate(time = time %>% ms)
dat[["pit_stops"]] <- dat[["pit_stops"]] %>%
  select(-time) %>% 
  mutate(duration = duration %>% as.character %>% as.numeric)
dat[["qualifying"]] <- dat[["qualifying"]] %>% 
  mutate(q1Ms = q1 %>% ms %>% as.numeric * 1000,
         q2Ms = q2 %>% ms %>% as.numeric * 1000,
         q3Ms = q3 %>% ms %>% as.numeric * 1000) %>% 
  select(-q1, -q2, -q3)
dat[["races"]] <- dat[["races"]] %>% 
  select(-time, -url) %>% 
  mutate(date = date %>% as.POSIXct(tz = "UTC"))
dat[["results"]] <- dat[["results"]] %>% 
  mutate(number = number %>% as.integer,
         position = position %>% as.integer,
         endResult = ifelse(position %in% NA, positionText %>% as.character, "C"),
         milliseconds = milliseconds %>% as.character %>% as.numeric,
         fastestLap = fastestLap %>% as.integer,
         rank = rank %>% as.integer,
         fastestLapTime = fastestLapTime %>% ms,
         fastestLapTimeMs = fastestLapTime %>% ms %>% as.numeric * 1000,
         fastestLapSpeed = fastestLapSpeed %>% as.character %>% as.numeric) %>% 
  select(-positionText, -time)

save(dat, file="data/dat.RData")
