# 🧹 Prediction Data Cleaning and Filtering Documentation

- **Code Location:** `Code/data_cleaning_prediction.ipynb`
## 1. Overview

**Input Data:**  
- Source: [to be updated]
- Number of raw participants in each moive:

| Movie |2019 Participants |2021 Participants| Total Participants Retained |
|------|---------------------|--------------|----|
| busstop | 210 | 85 | 295|
| cmiyc_long | 260 | 62 | 312 |
| keithreynolds | 198 | 64| 262 |
| theboyfriend | 236 | 35| 271 |
| therock | 204 | 77 | 281 |
| theshoe | 262 | 31 | 293 |

---

**Output Data:**  
- Cleaned files are saved in: `Cleaned Data/Prediction_Cleaned_Data/`
- File naming convention: `[movie]_prediction_cleaned.csv`

---

## 2. Filtering Criteria & Dropped Data

The following filters were applied to remove invalid or pilot participants:

### 2.1 **Completed + Audio Catch Filter**  

Kept only participants who appeared in `question_data` and completed the `Audio_Catch` trial.

### 2.2 **Looped Participants Filter**

A small number of participants experienced their session looping back to the start and repeating the instructions and experiment.  
This issue was detected by counting the number of `"id_submit"` events per participant in the raw `trialdata.csv`.  
Participants with more than **two** `id_submit` events were considered to have experienced looping.  

- **Dropped:** Participants whose loop occurred **after entering the experiment phase**, since their initial exposure to the experiment could bias responses after the session restarted.  
- **Kept:** Participants whose loop occurred during instructions or practice phase (before the experiment began) — these IDs were explicitly exempted from removal.

**List of dropped participants by Movies(looped during experiment phase):**
- **busstop:**
  ```
  'debugsgaz9:debugtsrSz',
  'debugcWTZl:debugetWm9',
  'debugBxi4k:debugiNh31',
  'debugs2Uin:debugUss8N',
  'debug617Hp:debugox7Nx',
  'debugoO7XR:debugdSU0R',
  'debugE6AVE:debugdFR5k'
  ```
- **cmiyc_long:**
  ```
  'debugm3tNI:debug9KzKf',
  'debugNSQx8:debugypdvf'
  ```
- **keithreynolds:**
  ```
  'debugRl5wZ:debugBEuIb',
  'debugTD2AD:debugS2JLT'
  ```
- **theshoe:**
  ```
  'debugokHY2:debug7O47g'
  ```
**Exempt participants (looped during instructions/practice phase and kept):**
- **theboyfriend:**
  ```
  debug8CU6p:debugHfGpS
  ```
- **therock:**
  ```
  debugGELZw:debugBfrPh
  ```

### 2.3 **Comprehension & Audio Accuracy Filter**  
Removed participants who failed comprehension questions or the audio catch trial.

### 2.4 **Pilot Data Filter**  
Removed participants with `Timestamp < 1555729200000` (before official study start date)

A total of **13 participants** were removed from `cmiyc_long`.


## 3. Data Cleaning & Correction

### 3.1 New User ID Assignment
Anonymize and standardize participant identifiers across runs and movies.

**Method**  
New IDs are generated from the original IDs by combining:
- Prefix (`P` for prediction, `D` for description)
- Run year code (`map_year(subfolder)`)
- Movie code (`map_movie(movie)`)
- Unique sequential number within each run/movie

**Example**  
`P2021M12_007` → Prefix `P` for prediction, year `2021` for seond run in 2021, movie `02` for movie cmiyc_long, sequence `007`

**Reproducibility**  
IDs are regenerated on every run. They are consistent within a single execution but may differ if the data order changes across runs, since no persistent ID map is stored.

### 3.2 Feature Extraction & Final Schema

From the `Datastring` column, the following fields were extracted:
- `counterbalance`
- `offset`
- `tag`
- `video_segment`
- `phase_type`
- `vidpath`
- `prediction_number`
- `time`
- `confidence`
- `prediction_content` (with escaped quotes cleaned: `\"` → `\`)

Additionally, a new column `data_set` was added to indicate which dataset/subfolder the row originated from.

**Final columns (order preserved)**  
```
ID, Trial, Timestamp, counterbalance, phase_type, confidence,
time, prediction_number, vidpath, prediction_content,
tag, offset, video_segment, data_set
```

**Output Summary:**
| Movie | Participants Retained | Rows Retained |
|------|---------------------|--------------|
| busstop | 198 | 2432 |
| cmiyc_long | 181 | 2775 |
| keithreynolds | 184 | 1727 |
| theboyfriend | 169 | 1916 |
| therock | 182 | 1599 |
| theshoe | 181 | 1077 |

---

