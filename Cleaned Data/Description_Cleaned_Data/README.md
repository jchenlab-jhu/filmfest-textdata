# 🧹 Description Data Cleaning and Filtering Documentation

- **Code Location:** `Code/data_cleaning_description.ipynb`
## 1. Overview

**Input Data:**  
- Source: **Description Raw Psiturk Data Files** – contains the 2022 pilot run and 2023-2024 full run data.

- For each movie (`busstop`, `cmiyc_long`, `keithreynolds`, `theboyfriend`, `therock`, `theshoe`), data from all available runs were combined before cleaning and filtering.

- Number of raw participants in each moive:

| Movie | 2023-2024 Full Run |
|------|---------------------|
| busstop | 372 |
| cmiyc_long | 402 |
| keithreynolds | 364|
| theboyfriend | 366 |
| therock |409 |
| theshoe | 367 |

---

**Output Data:**  
- Cleaned files are saved in: `Cleaned Data/Description_Cleaned_Data/`
- File naming convention: `[movie]_description_cleaned.csv`

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
  'debugNCIhG:debugXGkMI', 'debugivleh:debugJU9xl', 'debugKGgD6:debug9HTiH',
  'debugUNXD2:debugLq2fA', 'debugR6JWf:debugXyZMA', 'debugM8Krk:debug4HHwJ',
  'debugZ8t1w:debug7DJ8u', 'debugIbu1d:debuggMXW3'
  ```
- **cmiyc_long:**
  ```
  'debugw3Dn6:debugwhDAl', 'debugUPs99:debugZn4ia', 'debugftp27:debugFDL8k',
  'debugWsRzo:debuglJqup', 'debugtPDp4:debugZ6HNl', 'debugzIGMU:debuggRqBn'
  ```
- **keithreynolds:**
  ```
  'debugi6716:debugTXMD8', 'debugvs329:debug2GKzU', 'debugvgAxL:debugRBglI',
  'debugVJcBo:debugNLXS5', 'debug3z4gA:debugqihCD'
  ```
- **theboyfriend:**
  ```
  'debugzVQxD:debugWCOZY', 'debugysYGR:debugzZAsj', 'debugvYaqK:debugeLi8h',
  'debugIKZtO:debugLTqgk', 'debugC6OiV:debugFVNPi', 'debugbKRcn:debugj6Onf',
  'debugqYWsZ:debugMovMp'
  ```
- **therock:**
  ```
  'debugDefZV:debugvAJ5G', 'debugXiOXc:debuggnDvN', 'debugmk0pY:debugZBlwR',
  'debugArDN3:debugPsdC1', 'debug3ZAq4:debugWtBYs', 'debugeNJ2L:debugmY0uI',
  'debugaDyAS:debugrackF', 'debuga9k3G:debugDSmuE', 'debug8LVUl:debugTfWtU',
  'debugju8sj:debugjXOgS'
  ```
- **theshoe:**
  ```
  'debugl8I4G:debugLk1Sw', 'debugh282v:debugLSJas', 'debugaG7tQ:debugfWmQi', 'debugLTjRT:debugUwM6o'
  ```
**Exempt participants (looped during instructions/practice phase and kept):**
- **busstop:**
  ```
  debug85E42:debugHMIlp,
  debug3xsl4:debugSL1Ao,
  debug8tWP5:debugXYr1v
  ```
- **cmiyc_long:**
  ```
  debug5fU9J:debug3WJPv, debugqtfx5:debug3XZfc, debugXRIw3:debugcONSF,
  debugvtFsp:debugU9FW1, debug0oLsp:debugscuZ6, debugmk4Qz:debugbPaAN,
  debug09prG:debug7vNPf, debugeJ3re:debugvdfbP, debugEc6if:debugHdNZR, 
  debugt5oxx:debugO8o8R"
  ```
- **keithreynolds:**
  ```
  debugxLb1V:debug8OsDX,
  debugRpwLc:debugO3jh,
  debug4yg2J:debugHq0s6
  ```
- **theboyfriend:**
  ```
  debug6w2T2:debugDZHNH,
  debugurgNU:debugOxZCP
  ```
- **therock:**
  ```
  debugvAgQy:debugY23jk
  ```
- **theshoe:**
  ```
  debugeIXxN:debugf7YGZ, debugJG6DG:debugEgBS1, debugKeAJA:debugNYy79,
  debugWevWe:debugmQnOa, debuglfcYg:debugLVi8o
  ```

### 2.3 **Repeat Participants in CMIYC_long**

Because `cmiyc_long` was hosted on two different Heroku apps (and therefore two different Prolific experiments), a small number of participants were able to participate in both versions of the experiment.  

To prevent duplicate exposure from biasing their responses, the **second-run IDs** of these repeat participants were dropped from the cleaned dataset.

**Dropped IDs (second-run):**
```
debugwSh8w:debugwpzZp,
debugJzmMB:debugGzFsZ,
debugValB4:debugIp2gE,
debugdmrft:debugrrSdu
```

### 2.4 **Comprehension & Audio Accuracy Filter**  
Removed participants who failed comprehension questions or the audio catch trial.
To align with the folder-2 counterbalance offset (+6), `counterbalance` values {6,7,8} were normalized to {0,1,2} before comparison.

## 3. Data Cleaning & Correction

### 3.0 Data Preparation: Combining Subfolders and Counterbalance Adjustment

For `cmiyc_long`, the raw data were originally split into two subfolders (`1` and `2`).  
Before running the cleaning pipeline, these two subfolders were **merged** into a new combined dataset stored at the same directory level as `1` and `2`.  
This ensures that downstream filtering and feature extraction are applied to the entire dataset at once.

During merging, for `trialdata.csv` in subfolder `2`, the `"counterbalance"` value in the `Datastring` column was **adjusted by +6** to create unique counterbalance values across both runs.

### 3.1 New User ID Assignment
Anonymize and standardize participant identifiers across runs and movies.

**Method**  
New IDs are generated from the original IDs by combining:
- Prefix (`P` for prediction, `D` for description)
- Run year code (`map_year(subfolder)`)
- Movie code (`map_movie(movie)`)
- Unique sequential number within each run/movie

**Example**  
`D2023M02_007` → Prefix `D`, year `2023`, movie `02`, sequence `007`

**Reproducibility**  
IDs are regenerated on every run. They are consistent within a single execution but may differ if the data order changes across runs, since no persistent ID map is stored.

### 3.3 Feature Extraction & Final Schema

From the `Datastring` column, the following fields were extracted:
- `counterbalance`
- `phase_type` 
- `description_stop`
- `paired_vid`
- `onset` — extracted by mapping each **description-phase** row to its corresponding **viewing-phase** row using `vidseg` / `vidpair`.  
  - For each participant, we locate the matching viewing-phase row for the same segment.  
  - We then take the **onset value recorded during the viewing phase** and assign it to the description row.  
- `offset`
- `description_content`
- `importance`
- `time`
- `tag`
- `vidpath`

Additionally, a new column `data_set` was added to indicate which dataset/subfolder the row originated from.


**Final columns (order preserved)**  
```
'ID','Timestamp', 'Trial', 'counterbalance', 'phase_type',
'description_stop', "paired_vid", 'onset','offset','description_content',
'importance', 'time', 'tag', 'vidpath', 'Datastring','data_set'
```

**Output Summary:**
| Movie | Participants Retained | Rows Retained |
|------|---------------------|--------------|
| busstop | 271 | 1264 |
| cmiyc_long | 271 | 1990 |
| keithreynolds | 271 | 1054 |
| theboyfriend | 270 | 1410 |
| therock | 270 | 990 |
| theshoe | 271 | 391 |

---
