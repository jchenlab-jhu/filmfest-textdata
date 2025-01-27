# Experiment Overview

## Movies Used
Six movies were used in the experiments:
1. Catch Me If You Can (`cmiyc_long`)
2. The Boyfriend (`theboyfriend`)
3. The Shoe (`theshoe`)
4. Keith Reynolds (`keithreynolds`)
5. The Rock (`therock`)
6. Bus Stop (`busstop`)

## Experimental Setup

### Prediction Experiment
- Each participant watched only 1 movie
- Movies were shown in clips
- Between clips, participants were asked to predict the next 30 seconds
- Multiple predictions were allowed at each stop
- Time offsets were counted from the beginning of individual movies

### fMRI (FilmFest) Experiment
- Participants watched all movies in 2 scans
- Movies were concatenated into 2 video files:
  - FilmFest_part1: [`cmiyc_long`, `theboyfriend`, `theshoe`, `keithreynolds`]
  - FilmFest_part2: [`therock`, `busstop`]
- Time offsets were counted from the beginning of each video file

## Data Files

### 1. `FILM_merged_test_full.csv`
- Located in individual film folders
- Contains prediction text data
- One file per folder
- See "Predictions_Summary_Data_Table_README.txt" for detailed column descriptions

### 2. `FILM_merged_consensus_mapped_to_neuro.csv`
Column descriptions:
1. Primary Columns:
   - `Counterbalance`: Assigned experiment version with different stopping points
   - `offset`: Prediction stop time in prediction experiment
   - `video_segment`: Identifier for played video clip
   - `filmfest_offset`: Corresponding time in fMRI experiment

2. Additional Metrics:
   - `n_response`: Number of predictions per stop
   - `confidence`: Average confidence rating per stop
   - `consensus_isc`: Prediction consensus (intersubject correlation)
   - `consensus_paired`: Prediction consensus (pair-wise similarity)
   - `accuracy`: Average prediction accuracy per stop
   - `film_fest_onset`: Time in fMRI video file (10s before prediction or at the movie start)
   - `TR_onset`: Time in TR (10s before prediction or at the movie start)
   - `TR_offset`: Prediction stop time in TR

## Finding Specific Video Clips

### Locating Prediction Videos
1. Identify `counterbalance` and `video_segment`
2. Convert counterbalance number to alphabet
3. Find corresponding video in prediction_videos file

Example:
- For Bus Stop, `counterbalance` 0, `video_segment` 1:
  - Translates to `A_1`
  - Path: "Bus Stop/A/filmfest_clip12_A_1.mp4"

### Locating Prediction Points in Full Movies
1. Find the `filmfest_offset`
2. Convert to minutes and seconds
3. Locate the time in "filmfest_videos/FilmFest_partX.mp4"

Note: Video files will be shared separately.