##############################################################################
#Project: Prediction_Probe (Online via Psiturk/MTurk; Present movie with probes where participants predict what will happen next)
#Video: The Shoe (Film Fest)
#Purpose: Data preparation, figures and analyses
#Date: July 24, 2023
#Name: Buddhika Bellana, Anna Hu
##############################################################################

#########################
#####load libraries######
#########################
library(plyr)
library(ggplot2)
library(stringi)
library(stringr)
library(lsa)
library(hunspell)
library(gganimate)
library(dplyr)
library(Hmisc)
library(effsize)
library(reshape2)
library(tidyverse)

#########################
#######import data#######
#########################

##clear everything except the glove embedding dictionary
rm(list=setdiff(ls(), c("g6b_300","glove.300")))

#set working directory
setwd("D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/")

#create additional directories
prediction_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/predictions/"
dir.create(prediction_path, showWarnings = FALSE)
figure_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/figures/"
dir.create(figure_path, showWarnings = FALSE)
cosmat_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/figures/cosmat/"
dir.create(cosmat_path, showWarnings = FALSE)
hist_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/figures/hist/"
dir.create(hist_path, showWarnings = FALSE)
clean_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/predictions/clean/"
dir.create(clean_path, showWarnings = FALSE)
raw_path <- "D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/predictions/raw/"
dir.create(raw_path, showWarnings = FALSE)

##load data
#contains trial data & keywords
trial.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/trialdata.csv", header = FALSE, comment.char="#")
colnames(trial.data) <- c("ID","Trial","Timestamp","Datastring")
#contains post-task survey data
question.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/questiondata.csv", header = FALSE, comment.char="#")
colnames(question.data) <- c("ID","Question","Response")
#contains window events (e.g., focus on/off, window resize, etc)
event.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Prediction Consensus Data/Old_2019/theshoe/eventdata.csv", header = FALSE, comment.char="#")
colnames(event.data) <- c("ID","Event","Duration", "Details", "Timestamp")

#########################
###prepare dataframes####
#########################
#count number of participants
length(unique(trial.data$ID))
length(unique(event.data$ID))
length(unique(question.data$ID))

#generate list of IDs that did not reach post-task questions (i.e., incomplete)
drop.list <- as.character(unique(question.data$ID))

#drop incomplete subjects from further analysis
trial.data <- subset(trial.data, (ID %in% drop.list))
event.data <- subset(event.data, (ID %in% drop.list))

#select participants from April 11, 2019 pilot (different version of exp)
drop.pilots <- c("A1ZD8RU6YB0VEU:3NAPMVF0ZXRGGK7UY8NIT3BHOU627M",
                 "A3D70ANCSMGX5F:37TD41K0AILFX0JOJ4AZ07GZNZWCSS",
                 "A320QA9HJFUOZO:3IFS6Q0HJJV6A35OBE6LBAEU04FSI9",
                 "A3LL096CAY5WHB:32SVAV9L3GLNK41B9BD7M4IZVGCA3W",
                 "A4J4GGMKJ68L0:3CP1TO84PUD1TGTBAF8LVJXLXOB257",
                 "A2P065E9CYMYJL:3PB5A5BD0WI6VEF1XN87V75IYDB7GA",
                 "A1DBE2OL84TBKB:3SKEMFQBZ4HFXEG3XJLS7ZT6GEY8K1",
                 "A1KA64FR47O9FS:3ZGVPD4G6UTTKT7D0IZXLAD6Y0WTZX",
                 "A2VFEDAK5C1E1O:3XIQGXAUMDKHH0ARMX8BJT5D136X7R",
                 "A1ZTSCPETU3UJW:3907X2AHF1H5MQBUUUHZTP22OOCP2G",
                 "A48WNR6C4CI3J:3QAPZX2QN5P2Y97AD929EO1UGG102L",
                 "A3Q0XAGQ7TD7MP:3QAPZX2QN5P2Y97AD929EO1UGG102L",
                 "ASP32QB28TEZU:3QAPZX2QN5P2Y97AD929EO1UGG102L",
                 "A2M9QZ6894H8I6:3URFVVM166U8K6M20NP7Q6Z8HE2ZUT",
                 "A3QSJY1FFN0N6V:3L4D84MIL04DF5BEI2FMJXMS48QJHU",
                 "A1OM5NWYYYJKQW:3XIQGXAUMDKHH0ARMX8BJT5D136X7R",
                 "A22QD6M0A2C77C:3MTMREQS4WUKB23ZJVO9R1TWT9AAW6",
                 "A397HP5TSIF2LO:3MTMREQS4WUKB23ZJVO9R1TWT9AAW6",
                 "A36SHWB3EU22YE:3MTMREQS4WUKB23ZJVO9R1TWT9AAW6",
                 "A25KM3PUOUZFN1:3MTMREQS4WUKB23ZJVO9R1TWT9AAW6",
                 "A1JPTS3SMZ257I:3PB5A5BD0WI6VEF1XN2PPBGCANAG79")

#drop pilots
trial.data <- subset(trial.data, !(ID %in% drop.pilots))
question.data <- subset(question.data, !(ID %in% drop.pilots))
event.data <- subset(event.data, !(ID %in% drop.pilots))

#drop incomplete subjects from further analysis
#isolate marker of task completion (Audio Catch Trial)
label.temp2 <- subset(question.data, question.data$Question == "Audio_Catch")
keep.list <- as.character(label.temp2$ID)
trial.data <- subset(trial.data, (ID %in% keep.list))
question.data <- subset(question.data, (ID %in% keep.list))
event.data <- subset(event.data, (ID %in% keep.list))

#loop checking section -> use this to create list to drop for participants that looped
ppllist <- (unique(trial.data$ID))
loop_drop.list <- c()
extra_loop_count <- 0
under_loop_count <- 0
for (val in ppllist) {
  indvdata <- subset(trial.data, trial.data$ID == val)
  instrRepeat <- sum(grepl('INSTRUCTIONS', indvdata$Datastring))
  wildeRepeat <- sum(grepl("wildebeest", indvdata$Datastring))
  cat("ID: ", val, "instruction count: ", instrRepeat,"wildebeest count: ", wildeRepeat, "\n")
  if (instrRepeat > 9) {
    extra_loop_count <- extra_loop_count + 1
    cat("extra loop")
    loop_drop.list = append(loop_drop.list, val)
  } else if (instrRepeat < 9) {
    extra_loop_count <- under_loop_count + 1
    loop_drop.list = append(loop_drop.list, val)
  }
}

#final drop list after manual matching with data files
#keeping participants that only looped instructions and not actual experiment
drop.loops <- c("") #none found in this experiment

trial.data <- subset(trial.data, !(ID %in% drop.loops))
question.data <- subset(question.data, !(ID %in% drop.loops))
event.data <- subset(event.data, !(ID %in% drop.loops))

#recount number of participants
length(unique(trial.data$ID))
length(unique(event.data$ID))
length(unique(question.data$ID))

##generate counterbalance labels
#isolate data of interest
label.temp <- trial.data[grep("prac_segA_pred1", trial.data$Datastring), ]
#create a counterbalance column
label.temp <- within(label.temp, {
  counterbalance = ifelse(grepl(": 0,", label.temp$Datastring), "0", ifelse(grepl(": 1,", label.temp$Datastring), "1", ifelse(grepl(": 2,", label.temp$Datastring), "2", ifelse(grepl(": 3,", label.temp$Datastring), "3", ifelse(grepl(": 4,", label.temp$Datastring), "4", ifelse(grepl(": 5,", label.temp$Datastring), "5", "ERROR"))))))
})

#remove unneccessary columns
label.temp <- subset(label.temp, select = c("ID", "counterbalance"))
#add labels to existing datasets
trial.data$counterbalance <- with(label.temp, counterbalance[match(trial.data$ID, ID)])
question.data$counterbalance <- with(label.temp, counterbalance[match(question.data$ID, ID)])
event.data$counterbalance <- with(label.temp, counterbalance[match(event.data$ID, ID)])

#update ID column to experimental IDs
trial.data$turkID <- trial.data$ID #create new turkID column
trial.data$ID <- 0 #replace old ID column with 0s
#replace turkIDs with experimental IDs
j <- 1000 #IDs will begin at 1000
for(i in unique(trial.data$turkID)){ #for all unique IDs
    trial.data$ID[trial.data$turkID==i] <- j #set ID to j
    j <- j + 1 #increment j
}

#list unique IDs (for double check)
unique(trial.data$ID)

#match IDs across dataframes
#event.data
event.data$turkID <- event.data$ID #create new turkID column
event.data$ID <- 0 #replace old ID column with 0s
event.data$ID <- with(trial.data, ID[match(event.data$turkID, turkID)])
#question.data
question.data$turkID <- question.data$ID #create new turkID column
question.data$ID <- 0 #replace old ID column with 0s
question.data$ID <- with(trial.data, ID[match(question.data$turkID, turkID)])

#delete unnecessary columns
trial.data <- subset(trial.data, select= -c(turkID))
event.data <- subset(event.data, select= -c(turkID))
question.data <- subset(question.data, select= -c(turkID))

#set ID columns as factor
trial.data$ID <- as.factor(trial.data$ID)
event.data$ID <- as.factor(event.data$ID)
question.data$ID <- as.factor(question.data$ID)

#########################
###post-task questions###
#########################
#add phase label
question.data$phase <- ""
question.data <- within(question.data, {
  phase = ifelse(grepl("Comp_", question.data$Question), "comprehension", ifelse(grepl("subj_", question.data$Question), "subjective_experience", ifelse(grepl("demographics", question.data$Question), "demographics", ifelse(grepl("Audio", question.data$Question), "audio_catch","ERROR"))))
})

#replace blanks with NAs
question.data[question.data==""] <- NA

#separate question.data into separate phases
demographics <- subset(question.data, phase == "demographics")
subjective <- subset(question.data, phase == "subjective_experience")
comprehension <- subset(question.data, phase == "comprehension")
audio_catch <- subset(question.data, phase == "audio_catch")

##skim open-ended responses 
#display participants' self report: strategies for predictions
subset(subjective, subjective$Question == "subj_strategy")$Response
#display participants' self report: purpose of the experiment
subset(subjective, subjective$Question == "subj_purpose")$Response
#display participants' self report: overall feedback (note: this was optional)
subset(na.omit(subjective), na.omit(subjective)$Question == "subj_feedback")$Response

##subjective ratings
#prep data
ratings <- subset(subjective, Question %in% c("subj_engage", "subj_interest", "subj_enjoy", "subj_predict"))
ratings$Response <- as.numeric(as.character(ratings$Response))

##comprehension (MC)
comprehension$Response <- as.numeric(as.character(comprehension$Response))
#add correct responses
comprehension <- within(comprehension, {
  correct_response = ifelse(comprehension$Question=="Comp_Q1", 2, ifelse(comprehension$Question== "Comp_Q2",4, ifelse(comprehension$Question== "Comp_Q3", 1, ifelse(comprehension$Question== "Comp_Q4", 3, "ERROR"))))
})
comprehension$correct_response <- as.numeric(comprehension$correct_response)
#add accuracy
comprehension <- within(comprehension, {
  accuracy = ifelse(comprehension$Response == comprehension$correct_response, 1, 0)
})
#add question text for reference 
comprehension <- within(comprehension, {
  question_text = ifelse(comprehension$Question=="Comp_Q1", "What was the title of this clip, presented in a white font on a black background?", ifelse(comprehension$Question== "Comp_Q2","Which of the following best describes what the main characters were doing?", ifelse(comprehension$Question== "Comp_Q3", "What happened to the man once he got to the intersection?", ifelse(comprehension$Question== "Comp_Q4", "How did the video clip end?", "ERROR"))))
})

##audio catch trial
#add accuracy
audio_catch <- within(audio_catch, {
  accuracy = ifelse(audio_catch$Response == audio_catch$counterbalance, 1, 0)
})

#drop participants who did not correctly respond to audio catch trial
catch.drop.list <- unique(subset(audio_catch, accuracy==0)$ID)
comprehension <- subset(comprehension, !ID %in% catch.drop.list)
trial.data <- subset(trial.data, !ID %in% catch.drop.list)
event.data <- subset(event.data, !ID %in% catch.drop.list)
question.data <- subset(question.data, !ID %in% catch.drop.list)

#drop participants who did not correctly respond to ALL comprehension questions
comp.drop.list <- unique(subset(comprehension, accuracy==0)$ID)
comprehension <- subset(comprehension, !ID %in% comp.drop.list)
trial.data <- subset(trial.data, !ID %in% comp.drop.list)
event.data <- subset(event.data, !ID %in% comp.drop.list)
question.data <- subset(question.data, !ID %in% comp.drop.list)

#number of participants per counterbalance
table(subset(comprehension, question_text == "How did the video clip end?")$counterbalance)

################
###event data###
################
#count focus events per subject
focus.temp <- ddply(event.data, ~ID + counterbalance, summarise, on_events = sum(Details=="on", na.rm = TRUE), off_events = sum(Details=="off", na.rm = TRUE))

#################################
###overall participant summary###
#################################

#build a composite summary dataframe
#show all post-task scores per participant
summary.subject <- ddply(comprehension, ~ID + counterbalance, summarise, comprehension = mean(accuracy, na.rm = TRUE)) 
#add audio catch trial accuracy
summary.subject$audio_catch <- with(audio_catch, accuracy[match(summary.subject$ID, ID)])
#add focus off/on events
summary.subject$focus_on <- with(focus.temp, on_events[match(summary.subject$ID, ID)])
summary.subject$focus_off <- with(focus.temp, off_events[match(summary.subject$ID, ID)])

#count total subjects
length(unique(summary.subject$ID))

#calculate subjects needed to have 30 in each counterbalance
30 - table(summary.subject$counterbalance)

#delete temporary dataframes
rm(list=ls(pattern='*temp*'))

#######################
###experimental data###
#######################

#add phase label
trial.data$phase <- ""
trial.data <- within(trial.data, {
  phase = ifelse(grepl("prac_seg", trial.data$Datastring), "practice", ifelse(grepl("test_seg", trial.data$Datastring), "test", ifelse(grepl("instructions/instruct", trial.data$Datastring), "instructions", ifelse(grepl("status", trial.data$Datastring), "posttest","ERROR"))))
})

#add test trial label
trial.data$test_trial <- ""
trial.data <- within(trial.data, {
  test_trial = ifelse(grepl("_pred", trial.data$Datastring), "prediction", ifelse(grepl("_view", trial.data$Datastring), "view", NA))
})

#create new datastring column, with ID and Trial number
#also, use stringi to remove any whitespaces
trial.data$Datastring2 <- ""
trial.data <- within(trial.data, {
  Datastring2 = paste(trial.data$Datastring, ",", trial.data$ID, ",", trial.data$Trial)
})

###prediction data###
#subset practice and test data
prediction.data <- subset(trial.data, test_trial=="prediction")
prediction.data$Datastring2 <- as.character(prediction.data$Datastring2)
temp <- do.call(rbind, strsplit(prediction.data$Datastring2, ", \"|(>\\\".*?\\\".*?\\K(, |$))", perl = TRUE))
temp2 <- do.call(rbind, strsplit(temp[,10], ",|(>\\\".*?\\\".*?\\K(, |$))", perl = TRUE))
temp3 <- cbind(temp, temp2)
temp4 <-as.data.frame(temp3)
#rename columns
colnames(temp4) <- c("confidence", "time", "prediction_number", "vidpath", "prediction_content", "tag", "counterbalance", "offset", "phase", "scratch", "video_segment", "ID", "Trial")
temp <- temp4
#set all matching columns to integers or characters to facilitate matching in merge
temp$Trial <- trimws(as.integer(as.character(temp$Trial)))
temp$ID <- trimws(as.character(temp$ID))
prediction.data$Trial <- trimws(as.integer(as.character(prediction.data$Trial)))
prediction.data$ID <- trimws(as.character(prediction.data$ID))
#merge original with temp file
prediction.data <- merge(prediction.data,temp,by=c("ID","Trial")) 
#delete duplicate columns
prediction.data <- subset(prediction.data, select= -c(scratch))
prediction.data <- subset(prediction.data, select= -c(Datastring2))
prediction.data <- subset(prediction.data, select= -c(counterbalance.y))
prediction.data <- subset(prediction.data, select= -c(phase.y))
#rename columns
colnames(prediction.data)[colnames(prediction.data)=="counterbalance.x"] <- "counterbalance"
colnames(prediction.data)[colnames(prediction.data)=="phase.x"] <- "phase"

#working on cleaning up data from this point forward!
#use stringi to remove whitespaces for each data column
prediction.data$confidence <- stri_replace_all_charclass(prediction.data$confidence, "\\p{WHITE_SPACE}", "")
prediction.data$prediction_number <- stri_replace_all_charclass(prediction.data$prediction_number, "\\p{WHITE_SPACE}", "")
prediction.data$vidpath <- stri_replace_all_charclass(prediction.data$vidpath, "\\p{WHITE_SPACE}", "")
prediction.data$offset <- stri_replace_all_charclass(prediction.data$offset, "\\p{WHITE_SPACE}", "")
prediction.data$video_segment <- stri_replace_all_charclass(prediction.data$video_segment, "\\p{WHITE_SPACE}", "")
prediction.data$tag <- stri_replace_all_charclass(prediction.data$tag, "\\p{WHITE_SPACE}", "")

#remove unneccessary text from each data string
prediction.data$confidence <- gsub('"', '', prediction.data$confidence) #remove quotes
prediction.data$confidence <- stri_sub(prediction.data$confidence,13) #remove 13 characters from beginning
prediction.data$prediction_number <- gsub('"', '', prediction.data$prediction_number) #remove quotes
prediction.data$prediction_number <- stri_sub(prediction.data$prediction_number,12) #remove 12 characters from beginning
prediction.data$vidpath <- gsub('"', '', prediction.data$vidpath) #remove quotes
prediction.data$vidpath <- stri_sub(prediction.data$vidpath,9) #remove 9 characters from beginning
prediction.data$prediction_content <- gsub('"', '', prediction.data$prediction_content) #remove quotes
prediction.data$prediction_content <- stri_sub(prediction.data$prediction_content,10) #remove 10 characters from beginning
prediction.data$offset <- gsub('"', '', prediction.data$offset) #remove quotes
prediction.data$offset <- stri_sub(prediction.data$offset,8) #remove 8 characters from beginning
prediction.data$video_segment <- gsub('"', '', prediction.data$video_segment) #remove quotes
prediction.data$video_segment <- stri_sub(prediction.data$video_segment,9) #remove 9 characters from beginning
prediction.data$video_segment <- stri_sub(prediction.data$video_segment,1,-2) #remove 1 characters from end
prediction.data$tag <- gsub('"', '', prediction.data$tag) #remove quotes
prediction.data$tag <- stri_sub(prediction.data$tag,5) #remove 5 characters from beginning

#delete temporary dataframes
rm(list=ls(pattern='temp'))
#set columns to appropriate types
prediction.data$ID <- as.factor(prediction.data$ID)
prediction.data$counterbalance <- as.factor(prediction.data$counterbalance)
prediction.data$phase <- as.factor(prediction.data$phase)
prediction.data$test_trial <- as.factor(prediction.data$test_trial)
prediction.data$confidence <- as.integer(prediction.data$confidence)
prediction.data$prediction_number <- as.factor(prediction.data$prediction_number)
prediction.data$offset <- as.integer(prediction.data$offset)
prediction.data$video_segment <- as.factor(prediction.data$video_segment)

#subset data by segment/counterbalance
#define function to clean and export prediction data for universal sentence encoder
export_clean_prediction <- function(df,phase,counterbalance,segment){
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\u2019", "'", x)})) #replace unicode for ' with proper punctuation 
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\u201c", "\"", x)})) #replace unicode for (left) " with proper punctuation 
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\u201d", "\"", x)})) #replace unicode for (right) " with proper punctuation 
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\n", " ", x)})) #replace new line \n with space
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\", "\"", x)})) #replace \ with quotation mark -> all other special characters should be removed prior to this point so that the only \ left are the ones left from \" (the " gets removed during cleaning)
  # df <- data.frame(lapply(df, function(x) {
  #   gsub("/", " ", x)})) #replace / with space
  df <- data.frame(lapply(df, function(x) {
    str_squish(x)}))#get rid of spaces on ends & consecutive spaces 
  df <- data.frame(lapply(df, function(x) {
    gsub("/$", ".", x)})) #replace / at end of sentence with .
  df <- lapply(df, function(x) stringi::stri_trans_general(x, "latin-ascii")) #remove all non-ascii characters
  df <- data.frame(matrix(unlist(df), nrow=length(df), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
  df <- as.data.frame(t(df)) #transpose
  write.table(df, file = paste0(clean_path,"theshoe_",phase,"_c",counterbalance,"_seg",segment,".txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save prediction column only, without row or column labels, to csv
}
#define function to save all predictions in raw format
export_raw_prediction <- function(df, phase){
  write.csv(df, file = paste0(raw_path,"theshoe_",phase,"_full.csv")) #save full dataframe to csv
}

#define function to save data frame as R variable to use for merged data
export_data_frame <- function(df) {
  df2 <- df %>%
    add_column(data_set = "1") # 1 = old (2019) data
  df2 <- subset(df2, select = -c(Datastring))
  saveRDS(df2, file = "old_data.Rds")
}

##phase: practice
##predictions: all
#counterbalance: all (because, stops were identical)
practice_cAllsAll <- subset(prediction.data, phase == "practice")
export_raw_prediction(practice_cAllsAll, "practice")
practice_cAllsA <- subset(prediction.data, video_segment == "A", select = c(prediction_content))
export_clean_prediction(practice_cAllsA, "practice", "All", "A")
practice_cAllsB <- subset(prediction.data, video_segment == "B", select = c(prediction_content))
export_clean_prediction(practice_cAllsB, "practice", "All", "B")
##phase: test
##predictions: all
test_cAllsAll <- subset(prediction.data, phase == "test")
export_raw_prediction(test_cAllsAll, "test")
export_data_frame(prediction.data)
#counterbalance: all (for title only)
test_cAllsegTitle <- prediction.data[grep("filmfest_clip5_title_3s.mp4", prediction.data$vidpath), ]
test_cAllsegTitle <- subset(test_cAllsegTitle, select = c(prediction_content))
export_clean_prediction(test_cAllsegTitle, "test", "All", "Title")
#counterbalance: 0
test_c0s1 <- subset(prediction.data, counterbalance == 0 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c0s1, "test", "0", "1")
test_c0s2 <- subset(prediction.data, counterbalance == 0 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c0s2, "test", "0", "2")
test_c0s3 <- subset(prediction.data, counterbalance == 0 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c0s3, "test", "0", "3")
#counterbalance: 1
test_c1s1 <- subset(prediction.data, counterbalance == 1 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c1s1, "test", "1", "1")
test_c1s2 <- subset(prediction.data, counterbalance == 1 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c1s2, "test", "1", "2")
test_c1s3 <- subset(prediction.data, counterbalance == 1 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c1s3, "test", "1", "3")
#counterbalance: 2
test_c2s1 <- subset(prediction.data, counterbalance == 2 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c2s1, "test", "2", "1")
test_c2s2 <- subset(prediction.data, counterbalance == 2 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c2s2, "test", "2", "2")
test_c2s3 <- subset(prediction.data, counterbalance == 2 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c2s3, "test", "2", "3")
#counterbalance: 3
test_c3s1 <- subset(prediction.data, counterbalance == 3 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c3s1, "test", "3", "1")
test_c3s2 <- subset(prediction.data, counterbalance == 3 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c3s2, "test", "3", "2")
test_c3s3 <- subset(prediction.data, counterbalance == 3 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c3s3, "test", "3", "3")
#counterbalance: 4
test_c4s1 <- subset(prediction.data, counterbalance == 4 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c4s1, "test", "4", "1")
test_c4s2 <- subset(prediction.data, counterbalance == 4 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c4s2, "test", "4", "2")
test_c4s3 <- subset(prediction.data, counterbalance == 4 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c4s3, "test", "4", "3")
#counterbalance: 5
test_c5s1 <- subset(prediction.data, counterbalance == 5 & video_segment == "1", select = c(prediction_content))
export_clean_prediction(test_c5s1, "test", "5", "1")
test_c5s2 <- subset(prediction.data, counterbalance == 5 & video_segment == "2", select = c(prediction_content))
export_clean_prediction(test_c5s2, "test", "5", "2")
test_c5s3 <- subset(prediction.data, counterbalance == 5 & video_segment == "3", select = c(prediction_content))
export_clean_prediction(test_c5s3, "test", "5", "3")

##use USE (https://arxiv.org/abs/1803.11175) and tensorflow (https://www.tensorflow.org/tutorials) to get sentence-level embeddings 
#path to python script: /Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments//Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/theshoe/predictions

##generate stop-specific full cosine similarity matrix
#attach relevant libraries
library(RColorBrewer)
library(corrplot)
library(ggrepel)
#initialize empty lists
cosmat.list<-list()
cos.lowtri.list<-list()

##################
##USE timecourse##
##################
#load all USE csv files
setwd(paste0(prediction_path,"use/"))
temp = list.files(pattern="*.csv")
use_list = lapply(temp, function(x) read.delim(x, header = FALSE, sep = ","))
stacked_cosmat <- data.frame() #empty variable for stacked similarity matrices

#reorder similarity matrix
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

#loop through stop points in movie
for(i in 1:length(use_list)){ #for all USE csv (i.e., each stop)
  #generate cosine similarity matrix
  cosmat.list[[i]] <- cosine(t(use_list[[i]]))
  #also turn matrix into stacked df for ggplotting
  if (i>2 & i<length(use_list)){ #if statement to avoid including the practice and title matrices, which are much larger than the rest
    temp_df <- data.frame(reorder_cormat(cosmat.list[[i]])) #create temporary dataframe
    colnames(temp_df) <- paste0("X",1:length(colnames(temp_df))) #label column names as X and an increasing integer
    temp_df$variable1 <- paste0("X",1:length(rownames(temp_df))) #create a row name column with X and an increasing integer
    temp_df$label <- as.character(stri_sub(stri_sub(temp[i],13),1,-5)); #add label to dataframe
    stacked_cosmat <- rbind.fill(stacked_cosmat,temp_df) #stack matrices into a dataframe -- note rbind.fill adds NAs as required to rbind dfs that don't have the same number of columns
  }
  #plot correlation matrices for all free associate word embeddings
  corrplot(reorder_cormat(cosmat.list[[i]]), type = "full", method = "color", col= colorRampPalette(c("dark blue","blue","white", "orange", "red"))(10), tl.pos = "n", tl.col = "black")
  dev.print(pdf, paste0(figure_path,'cosmat/', stri_sub(stri_sub(temp[i],13),1,-5),'_cosmat.pdf'))
  #select lower triangle of cosine similarity matrix
  cos.lowtri.list[[i]] <- cosmat.list[[i]][lower.tri(cosmat.list[[i]], diag = FALSE)]
  #save histogram
  hist(cos.lowtri.list[[i]])
  dev.print(pdf, paste0(figure_path,'hist/', stri_sub(stri_sub(temp[i],13),1,-5),'_hist.pdf'))
}

#take stacked similarity matrix dataframe and melt into long format 
melt_cosmat <- melt(subset(stacked_cosmat,!label %in% c("practice_cAll_segA","practice_cAll_segB","test_cAll_segTitle")),id.vars = c("variable1","label"))
melt_cosmat$variable = factor(melt_cosmat$variable, levels=unique(melt_cosmat$variable[order(as.numeric(stri_sub(stri_sub(melt_cosmat$variable,2),1,-1)))]), ordered=TRUE) #reorder the factor level based on segment and counterbalance
melt_cosmat$variable1 = factor(melt_cosmat$variable1, levels=unique(melt_cosmat$variable1[order(as.numeric(stri_sub(stri_sub(melt_cosmat$variable1,2),1,-1)))]), ordered=TRUE) #reorder the factor level based on segment and counterbalance

#melted dataframe is made, but now, order the stops to align with their occurrence in the movie
#this is important for the multipanel similarity matrix plot
melt_cosmat$temp <- stri_sub(stri_sub(melt_cosmat$label,6),1,-1) #create temp column with counterbalance + segment info
melt_cosmat <- cbind(melt_cosmat,str_split_fixed(melt_cosmat$temp, "_", 2)) #separate counterbalance and segment into two columns
melt_cosmat$temp <- NULL #delete the temp column
names(melt_cosmat)[names(melt_cosmat)=="1"] <- "counterbalance" #rename counterbalance
names(melt_cosmat)[names(melt_cosmat)=="2"] <- "segment" #rename segment
melt_cosmat$segment<- as.numeric(stri_sub(stri_sub(melt_cosmat$segment,4),1,-1)) #clean up segment
melt_cosmat$counterbalance <- as.factor(as.numeric(stri_sub(stri_sub(melt_cosmat$counterbalance,2),1,-1))) #clean up counterbalance
melt_cosmat$counterbalance <- factor(melt_cosmat$counterbalance, levels = c("1","2","3","4","5","0")) #reorder factor levels 
melt_cosmat$label = factor(melt_cosmat$label, levels=unique(melt_cosmat$label[order(melt_cosmat$segment,melt_cosmat$counterbalance)]), ordered=TRUE) #reorder the factor level based on segment and counterbalance

#plot matrices of USE similarity for entire movie
ggplot(data = melt_cosmat, aes(x=variable, y=variable1, fill=value)) + 
  facet_wrap(~ label) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_blank(), axis.line.y = element_blank(), plot.title = element_text(size=18), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) +
  geom_tile() +
  scale_fill_gradientn(colours = c("green", "yellow", "red"),
                       values = scales::rescale(c(0, .45, .5, .55, 1)), na.value = "white", name = "Cosine") +
  labs(title="Cosine Similarity: USE in The Shoe", size = 20)
ggsave(paste0(figure_path,"theshoe_use_cosmat.pdf"), scale = 2)

#generate dataframe of pairwise USE similarity estimates per stop in movie
df <- t(plyr::ldply(cos.lowtri.list, rbind)) #list to dataframe
colnames(df) <- stri_sub(stri_sub(temp,13),1,-5) #rename columns
df <- df[,4:ncol(df)-1] #remove practice and title screen
df <- melt(df) #wide to long, for ggplot
df <- subset(df, select= c(Var2,value))
names(df)[names(df)=="value"] <- "similarity" #rename value
names(df)[names(df)=="Var2"] <- "prediction_stop" #rename variable

#dataframe is made, but now, order the stops to align with their occurrence in the movie
#this is important for the multipanel histogram plot
df$temp <- stri_sub(stri_sub(df$prediction_stop,6),1,-1) #create temp dataframe with counterbalance + segment info
df <- cbind(df,str_split_fixed(df$temp, "_", 2)) #separate counterbalance and segment into two columns
df$temp <- NULL #delete the temp column
names(df)[names(df)=="1"] <- "counterbalance" #rename counterbalance
names(df)[names(df)=="2"] <- "segment" #rename segment
df$segment <- as.numeric(stri_sub(stri_sub(df$segment,4),1,-1)) #clean up segment
df$counterbalance <- as.factor(as.numeric(stri_sub(stri_sub(df$counterbalance,2),1,-1))) #clean up counterbalance
df$counterbalance <- factor(df$counterbalance, levels = c("1","2","3","4","5","0")) #reorder factor levels 
df$label = factor(df$prediction_stop, levels=unique(df$prediction_stop[order(df$segment,df$counterbalance)]), ordered=TRUE) #reorder the factor level based on segment and counterbalance

#plot histograms of USE similarity for entire movie
ggplot(data=df, aes(similarity)) +
  facet_wrap(~ label) + 
  geom_histogram(aes(fill =..count..,y=..count..), 
                 breaks=seq(0, 1, by = .1), 
                 col="red") + 
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_vline(data = ddply(df, "label", summarise, label.mean = mean(similarity,na.rm=TRUE)), aes(xintercept = label.mean),col='black',size=.5,linetype="dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), plot.title = element_text(size=18)) +
  labs(title="Histogram: USE similarity in The Shoe", size = 20) +
  labs(x="Similarity", y="Count")
ggsave(paste0(figure_path,"theshoe_use_histogram.pdf"), scale = 2)

#calculate mean cosine similarity at each stop in movie
use.data <- lapply(cos.lowtri.list, mean) #calculate mean cosine similarity from lower triangle
use.data  <- as.data.frame(plyr::ldply(use.data , rbind)) #list to dataframe
names(use.data) <- c("similarity") #rename column
use.data$label <- NA #create temporary values for column
for (i in 1:length(use_list)){
  use.data$label[i] <- stri_sub(stri_sub(temp[i],13),1,-5) #add label
}
use.data <- within(use.data, {
  counterbalance = ifelse(grepl("c0", use.data$label), "0", ifelse(grepl("c1", use.data$label), "1", ifelse(grepl("c2", use.data$label), "2", ifelse(grepl("c3", use.data$label), "3", ifelse(grepl("c4", use.data$label), "4", ifelse(grepl("c5", use.data$label), "5", ifelse(grepl("cAll", use.data$label), NA, "ERROR"))))))) #add counterbalance
})
use.data <- within(use.data, {
  segment = ifelse(grepl("seg10", use.data$label), "10", ifelse(grepl("seg11", use.data$label), "11", ifelse(grepl("seg12", use.data$label), "12", ifelse(grepl("seg2", use.data$label), "2", ifelse(grepl("seg3", use.data$label), "3", ifelse(grepl("seg4", use.data$label), "4", ifelse(grepl("seg5", use.data$label), "5", ifelse(grepl("seg6", use.data$label), "6", ifelse(grepl("seg7", use.data$label), "7", ifelse(grepl("seg8", use.data$label), "8", ifelse(grepl("seg9", use.data$label), "9", ifelse(grepl("seg1", use.data$label), "1", ifelse(grepl("segA", use.data$label), "A", ifelse(grepl("segB", use.data$label), "B", ifelse(grepl("segTitle", use.data$label), "Title", "ERROR"))))))))))))))) #define segment
})
#create a temporary value where counterbalance and segment are concatenated
prediction.data$temp <- paste0(prediction.data$counterbalance, prediction.data$video_segment)
use.data$temp <- paste0(use.data$counterbalance, use.data$segment)
#match this concatenated value across datasets to get offset values
use.data$offset<- with(prediction.data, offset[match(use.data$temp, temp)])
use.data$offset[use.data$temp == "NATitle"] <- 3 #relabel offset for title screen
use.data <- subset(use.data, !(temp %in% c("01", "21", "31", "41", "51","NAA","NAB"))) #remove unnecessary datapoints
use.data$temp2 <- paste0(use.data$counterbalance, use.data$offset)

#calculate number of predictions per stop
use.data.n <- lapply(cosmat.list, function(x) sqrt(length(x))) #calculate number of predictions per stop
use.data.n  <- as.data.frame(plyr::ldply(use.data.n , rbind)) #list to dataframe
names(use.data.n) <- c("n") #rename column
use.data.n$label <- NA #create temporary values for column
for (i in 1:length(use_list)){
  use.data.n$label[i] <- stri_sub(stri_sub(temp[i],13),1,-5) #add label
}

#calculate SD across predictions per stop
use.data.sd <- lapply(cosmat.list, sd) #calculate number of predictions per stop
use.data.sd <- as.data.frame(plyr::ldply(use.data.sd , rbind)) #list to dataframe
names(use.data.sd) <- c("sd") #rename column
use.data.sd$label <- NA #create temporary values for column
for (i in 1:length(use_list)){
  use.data.sd$label[i] <- stri_sub(stri_sub(temp[i],13),1,-5) #add label
}

#calculate standard error
use.data$sd <- with(use.data.sd, sd[match(use.data$label, label)]) #match sd and add to df
use.data$n <- with(use.data.n, n[match(use.data$label, label)]) #match n and add to df
use.data$se <- use.data$sd/sqrt(use.data$n) #calculate se
rm(use.data.sd, use.data.n) #remove unnecessary dataframes

#plot average cosine similarity between USE embeddings for predictions at each stop
ggplot(data=use.data, aes(x=offset, y=similarity)) +
  geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("cosine similarity") + ylim(-.1, .8)
ggsave(paste0(figure_path,"theshoe_use_timecourse.tiff"), dpi = 300, scale = 1)
ggsave(paste0(figure_path,"theshoe_use_timecourse.png"), dpi = 300, scale = 1)

#plot average cosine similarity between USE embeddings for predictions at each stop
ggplot(data=use.data, aes(x=offset, y=similarity)) +
  geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  geom_text_repel(aes(label = use.data$n, size = NULL)) +
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("cosine similarity") + ylim(-.1, .8)
ggsave(paste0(figure_path,"theshoe_use_timecourse_labelled.png"), dpi = 300, scale = 1)

#plot confidence
#z-score confidence ratings by subject
temp.conf <- ddply(subset(prediction.data,phase=="test"), ~ID, summarise, mean = mean(confidence), sd = sd(confidence), se = sd/sqrt(length(unique(prediction.data$ID))))
temp.conf2 <- ddply(subset(prediction.data,phase=="test"), ~ID + offset, summarise, mean = mean(confidence))
temp.conf2$sub.mean <- with(temp.conf, mean[match(temp.conf2$ID, ID)]) 
temp.conf2$sub.sd <- with(temp.conf, sd[match(temp.conf2$ID, ID)]) 
temp.conf2$z <- (temp.conf2$mean - temp.conf2$sub.mean)/temp.conf2$sub.sd

#average all z-scored confidence ratings per offset
confidence.data <- ddply(temp.conf2, ~offset, summarise, z_mean = mean(z, na.rm=TRUE), z_sd = sd(z, na.rm=TRUE)) #average per stop
confidence.data$counterbalance <- with(prediction.data, counterbalance[match(confidence.data$offset, offset)]) #get matching counterbalance
confidence.data$counterbalance[confidence.data$offset == 3] <- NA
confidence.data$temp2 <- paste0(confidence.data$counterbalance, confidence.data$offset) #create a tag for matching
confidence.data$n <- with(use.data, n[match(confidence.data$temp2, temp2)]) #get number of predictions per stop
confidence.data$z_se <- confidence.data$z_sd/sqrt(confidence.data$n) #calculate se 

ggplot(data=confidence.data, aes(x=offset, y=z_mean)) +
  geom_errorbar(aes(ymin=z_mean-z_se, ymax=z_mean+z_se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("confidence (z)") + ylim(-1.1, 1.5)
ggsave(paste0(figure_path,"theshoe_confidence_timecourse.tiff"), dpi = 300, scale = 1)
ggsave(paste0(figure_path,"theshoe_confidence_timecourse.png"), dpi = 300, scale = 1)

ggplot(data=confidence.data, aes(x=offset, y=z_mean)) +
  geom_errorbar(aes(ymin=z_mean-z_se, ymax=z_mean+z_se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  geom_text_repel(aes(label = use.data$n, size = NULL)) +
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("confidence (z)") + ylim(-1.1, 1.5)
ggsave(paste0(figure_path,"theshoe_confidence_timecourse_labelled.png"), dpi = 300, scale = 1)

#correlate confidence with USE cosine similarity
use.data$offset <- as.numeric(use.data$offset) #change offset to numeric
use.data <- use.data[order(use.data$offset),] #order by offset
confidence.data$offset <- as.numeric(confidence.data$offset) #change offset to numeric
confidence.data <- confidence.data[order(confidence.data$offset),] #order by offset
cor.test(confidence.data$z_mean, use.data$similarity) #run correlation test

##upsample USE and confidence timecourses to fMRI timecourse
#spline for use
use.spline.interp <- spline(use.data$offset, use.data$similarity, method="natural", n=100000, xmin=0, xmax=665)
use.spline.interp <- data.frame(matrix(unlist(use.spline.interp), nrow=length(use.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
use.spline.interp <- data.frame(matrix(unlist(use.spline.interp), nrow=length(use.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #go long from wide
names(use.spline.interp) <- c("offset", "similarity") #rename column

#spline for confidence
conf.spline.interp <- spline(confidence.data$offset, confidence.data$z_mean, method="natural", n=100000, xmin=0, xmax=665)
conf.spline.interp <- data.frame(matrix(unlist(conf.spline.interp), nrow=length(conf.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
conf.spline.interp <- data.frame(matrix(unlist(conf.spline.interp), nrow=length(conf.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(conf.spline.interp) <- c("offset", "confidence") #rename column

#build a vector representing each TR, in seconds, for the theshoe movie
tr <- as.data.frame(seq(from = 0, to = 135, by = 1.5))
names(tr) <- c("s") #rename column

#load MALDIquant for match.closest function
library(MALDIquant)

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(tr$s)){
  tr$position_match[i] <- match.closest(tr$s[i], use.spline.interp$offset, tolerance = Inf, nomatch = 0000) #find matching offsets
  tr$similarity[i] <- use.spline.interp$similarity[tr$position_match[i]] #find corresponding similarity estimates 
  tr$confidence[i] <- conf.spline.interp$confidence[tr$position_match[i]] #find corresponding confidence estimates 
}
#export as csv
write.csv(tr, file = paste0(prediction_path,"theshoe_full_interp_tr_match.csv")) #save full dataframe to csv
df.use <- subset(tr, select = similarity)
df.use <- df.use[16:length(t(df.use))-1,] #remove first 14 trs (to deal with an intial transient in the BOLD data) + AND remove trim the last TR (overlaps with subsequent movie)
write.table(df.use, file = paste0(prediction_path,"theshoe_use_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save prediction column only, without row or column labels, to csv
write.table(df.use, file = paste0(prediction_path,"theshoe_use_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save prediction column only, without row or column labels, to txt
df.conf <- subset(tr, select = confidence)
df.conf <- df.conf[16:length(t(df.conf))-1,] #remove first 14 trs (to deal with an intial transient in the BOLD data) + AND remove trim the last TR (overlaps with subsequent movie)
write.table(df.conf, file = paste0(prediction_path,"theshoe_conf_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save prediction column only, without row or column labels, to csv
write.table(df.conf, file = paste0(prediction_path,"theshoe_conf_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save prediction column only, without row or column labels, to txt

#double check correlation between confidence and use similarity
#so long as everything worked properly:
#should show similar magnitude (r values) to original correlation 
cor.test(tr$confidence, tr$similarity) #run correlation test

#plot TR rate estimates 
#for use cosine similarity
ggplot(data=tr, aes(x=s, y=similarity)) +
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7)
ggsave(paste0(figure_path,"theshoe_use_upsampled_timecourse.tiff"), dpi = 300, scale = 1)


#for confidence
ggplot(data=tr, aes(x=s, y=confidence)) +
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("confidence (z)") + ylim(-1, 1)
ggsave(paste0(figure_path,"theshoe_confidence_upsampled_timecourse.tiff"), dpi = 300, scale = 1)

