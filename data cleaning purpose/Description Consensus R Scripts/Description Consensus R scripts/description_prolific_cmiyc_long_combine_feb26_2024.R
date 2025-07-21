##############################################################################
#Project: Description_Consensus (Online via Psiturk/Prolific; Present movie with probes where participants describe what happens)
#Video: Catch Me if You Can (FilmFest); #1: counterbalance A-F
#Purpose: Data preparation, figures and analyses
#Date: Feb 26, 2024
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
library(Rmisc)

#########################
#######import data#######
#########################

##clear everything except the glove embedding dictionary
rm(list=setdiff(ls(), c("g6b_300","glove.300")))

#set working directory
setwd("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1")

#create additional directories
description_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/descriptions/"
dir.create(description_path, showWarnings = FALSE)
figure_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/figures/"
dir.create(figure_path, showWarnings = FALSE)
cosmat_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/figures/cosmat/"
dir.create(cosmat_path, showWarnings = FALSE)
hist_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/figures/hist/"
dir.create(hist_path, showWarnings = FALSE)
clean_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/descriptions/clean/"
dir.create(clean_path, showWarnings = FALSE)
raw_path <- "D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/descriptions/raw/"
dir.create(raw_path, showWarnings = FALSE)

##load data & merge cmiyc_1 and cmiyc_2 data into one list
#contains trial data & keywords
trial1.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/trialdata.csv", header = FALSE, comment.char="#")
trial2.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/2/trialdata.csv", header = FALSE, comment.char="#")

#replace cmiyc_2 counterbalances 0,1,2 to 6,7,8
trial2.data$V4 <- gsub('"counterbalance": 0', '"counterbalance": 6', trial2.data$V4)
trial2.data$V4 <- gsub('"counterbalance": 1', '"counterbalance": 7', trial2.data$V4)
trial2.data$V4 <- gsub('"counterbalance": 2', '"counterbalance": 8', trial2.data$V4)

trial.data <- rbind(trial1.data, trial2.data)
colnames(trial.data) <- c("ID","Trial","Timestamp","Datastring")

#contains post-task survey data
question1.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/questiondata.csv", header = FALSE, comment.char="#")
question2.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/2/questiondata.csv", header = FALSE, comment.char="#")

question.data <- rbind(question1.data, question2.data)
colnames(question.data) <- c("ID","Question","Response")


#contains window events (e.g., focus on/off, window resize, etc)
event1.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/1/eventdata.csv", header = FALSE, comment.char="#")
event2.data <- read.csv("D:/PNB/Work/College/Research/Chen_Lab/Description Consensus/cmiyc_long data/2/eventdata.csv", header = FALSE, comment.char="#")

event.data <- rbind(event1.data, event2.data)
colnames(event.data) <- c("ID","Event","Duration", "Details", "Timestamp")

#########################
###prepare dataframes####
#########################
#count number of participants
length(unique(trial.data$ID))
length(unique(event.data$ID))
length(unique(question.data$ID))

#drop all entries before pilot data collection started (9/22/2022)
#identify ids that have timestamps after date (in unix time) to keep
label.temp3 <- subset(event.data, event.data$Timestamp > 1663804800000)
keep.list2 <- as.character(label.temp3$ID)
trial.data <- subset(trial.data, (ID %in% keep.list2))
question.data <- subset(question.data, (ID %in% keep.list2))
event.data <- subset(event.data, (ID %in% keep.list2))

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
  if (instrRepeat > 4) {
    extra_loop_count <- extra_loop_count + 1
    cat("extra loop")
    loop_drop.list = append(loop_drop.list, val)
  } else if (instrRepeat < 4) {
    extra_loop_count <- under_loop_count + 1
    loop_drop.list = append(loop_drop.list, val)
  }
}

# no problematic loops found? if wildebeest loops are ok without exp looping
# #final drop list after manual matching with data files
# #keeping participants that only looped instructions and not actual experiment
# drop.loops <- c()
# 
# trial.data <- subset(trial.data, !(ID %in% drop.loops))
# question.data <- subset(question.data, !(ID %in% drop.loops))
# event.data <- subset(event.data, !(ID %in% drop.loops))

#recount number of participants
length(unique(trial.data$ID))
length(unique(event.data$ID))
length(unique(question.data$ID))

##generate counterbalance labels
#isolate data of interest
label.temp <- trial.data[grep("prac_segB_desc", trial.data$Datastring), ]
#create a counterbalance column
label.temp <- within(label.temp, {
  counterbalance = ifelse(grepl("\"counterbalance\": 0,", label.temp$Datastring), "0", ifelse(grepl("\"counterbalance\": 1,", label.temp$Datastring), "1", ifelse(grepl("\"counterbalance\": 2,", label.temp$Datastring), "2", ifelse(grepl("\"counterbalance\": 3,", label.temp$Datastring), "3", ifelse(grepl("\"counterbalance\": 4,", label.temp$Datastring), "4", ifelse(grepl("\"counterbalance\": 5,", label.temp$Datastring), "5", ifelse(grepl("\"counterbalance\": 6,", label.temp$Datastring), "6", ifelse(grepl("\"counterbalance\": 7,", label.temp$Datastring), "7", ifelse(grepl("\"counterbalance\": 8,", label.temp$Datastring), "8", "ERROR")))))))))
})

#remove unneccessary columns
label.temp <- subset(label.temp, select = c("ID", "counterbalance"))
#add labels to existing datasets
trial.data$counterbalance <- with(label.temp, counterbalance[match(trial.data$ID, ID)])
question.data$counterbalance <- with(label.temp, counterbalance[match(question.data$ID, ID)])
event.data$counterbalance <- with(label.temp, counterbalance[match(event.data$ID, ID)])

#update ID column to experimental IDs (to anonymize original ID)
trial.data$hitID <- trial.data$ID #create new hitID column
trial.data$ID <- 0 #replace old ID column with 0s
#replace hitIDs with experimental IDs
j <- 1000 #IDs will begin at 1000
for(i in unique(trial.data$hitID)){ #for all unique IDs
  trial.data$ID[trial.data$hitID==i] <- j #set ID to j
  j <- j + 1 #increment j
}

#list unique IDs (for double check)
unique(trial.data$ID)

#match IDs across dataframes
#event.data
event.data$hitID <- event.data$ID #create new hitID column
event.data$ID <- 0 #replace old ID column with 0s
event.data$ID <- with(trial.data, ID[match(event.data$hitID, hitID)])
#question.data
question.data$hitID <- question.data$ID #create new hitID column
question.data$ID <- 0 #replace old ID column with 0s
question.data$ID <- with(trial.data, ID[match(question.data$hitID, hitID)])

#delete unnecessary columns
trial.data <- subset(trial.data, select= -c(hitID))
event.data <- subset(event.data, select= -c(hitID))
question.data <- subset(question.data, select= -c(hitID))

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
  phase = ifelse(grepl("Comp_", question.data$Question), "comprehension", ifelse(grepl("subj_", question.data$Question), "subjective_experience", ifelse(grepl("demographics", question.data$Question), "demographics", ifelse(grepl("Audio", question.data$Question), "audio_catch", ifelse(grepl("prolific_ID", question.data$Question), "ID", "ERROR")))))
})

#replace blanks with NAs
question.data[question.data==""] <- NA

#separate question.data into separate phases
demographics <- subset(question.data, phase == "demographics")
subjective <- subset(question.data, phase == "subjective_experience")
comprehension <- subset(question.data, phase == "comprehension")
audio_catch <- subset(question.data, phase == "audio_catch")

##skim open-ended responses 
#display participants' self report: strategies for descriptions
subset(subjective, subjective$Question == "subj_strategy")$Response
#display participants' self report: purpose of the experiment
subset(subjective, subjective$Question == "subj_purpose")$Response
#display participants' self report: overall feedback (note: this was optional)
subset(na.omit(subjective), na.omit(subjective)$Question == "subj_feedback")$Response

##subjective ratings
#prep data
ratings <- subset(subjective, Question %in% c("subj_engage", "subj_interest", "subj_enjoy","subj_predict", "subj_desc"))
ratings$Response <- as.numeric(as.character(ratings$Response))

##comprehension (MC)
comprehension$Response <- as.numeric(as.character(comprehension$Response))
#add correct responses
comprehension <- within(comprehension, {
  correct_response = ifelse(comprehension$Question=="Comp_Q1", 1, ifelse(comprehension$Question== "Comp_Q2",3, ifelse(comprehension$Question== "Comp_Q3", 4, ifelse(comprehension$Question== "Comp_Q4", 1, "ERROR"))))
})
comprehension$correct_response <- as.numeric(comprehension$correct_response)
#add accuracy
comprehension <- within(comprehension, {
  accuracy = ifelse(comprehension$Response == comprehension$correct_response, 1, 0)
})
#add question text for reference 
comprehension <- within(comprehension, {
  question_text = ifelse(comprehension$Question=="Comp_Q1", "What was the title of this clip, presented in a white font on a black background?", ifelse(comprehension$Question== "Comp_Q2","What did the woman do to the man's phone?", ifelse(comprehension$Question== "Comp_Q3", "What was the result of the magician's spell?", ifelse(comprehension$Question== "Comp_Q4", "Which character(s) ended up getting shot?", "ERROR"))))
})

##audio catch trial
audio_catch <- within(audio_catch, {
  accuracy = ifelse(audio_catch$Response == audio_catch$counterbalance|(audio_catch$counterbalance == 6 & audio_catch$Response == 0)|(audio_catch$counterbalance == 7 & audio_catch$Response == 1)|(audio_catch$counterbalance == 8 & audio_catch$Response == 2), 1, 0)
})

# #NOTE:commented out for awhile -> why?
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
table(subset(comprehension, question_text == "What was the title of this clip, presented in a white font on a black background?")$counterbalance)

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
table(summary.subject$counterbalance)

#delete temporary dataframes
rm(list=ls(pattern='*temp*'))

######################
###spoiler analysis###
######################

# #TODO: debug
# 
# #subset spoiler data
# spoiler.data <- subset(question.data, Question %in% c("subj_spoiler_use","subj_spoiler_text","subj_trailer_use","subj_trailer_text","subj_spoiler_addtltext"))
# spoiler.data$dataset <- "cmiyc_long_1"
# #drop hitID from question.data
# question.data <- subset(question.data, select= -c(hitID))
# #drop ID from spoiler.data
# spoiler.data <- subset(spoiler.data, select= -c(ID))
# #total number of subjects
# length(unique(spoiler.data$hitID))
# #export spoiler questions as csv
# write.csv(spoiler.data, file = paste0(raw_path,"cmiyc_long_1_spoilerdata.csv")) #save dataframe to csv

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
  test_trial = ifelse(grepl("_desc", trial.data$Datastring), "description", ifelse(grepl("_view", trial.data$Datastring), "view", NA))
})

#NOTE: Commenting out 2/12/2024 to add in reorganization code for random disorderly rows

# #create new datastring column, with ID and Trial number
# #also, use stringi to remove any whitespaces
# trial.data$Datastring2 <- ""
# trial.data <- within(trial.data, {
#   Datastring2 = paste(trial.data$Datastring, ",", trial.data$ID, ",", trial.data$Trial)
# })

###description data###
#subset practice and test data
description.data <- subset(trial.data, test_trial=="description")
description.data$Datastring <- as.character(description.data$Datastring)

#parse into segments
temp <- do.call(rbind, strsplit(description.data$Datastring, ", \"|(>\\\".*?\\\".*?\\K(, |$))", perl = TRUE))
temp <- as.data.frame(temp)
temp$V1 <- as.character(stri_sub(temp$V1,3)) #remove curly brackets from first column

#reorganize rows
#super helpful code from jay.sf: https://stackoverflow.com/questions/60664813/reorder-columns-of-a-dataframe-on-a-row-by-row-basis-in-r/60664843#60664843
#reorders each row based on values before the colon
#this matters because psiturk doesn't necessarily save the data with the columns in the same order across subjects
temp <- as.data.frame(t(apply(as.data.frame(temp), 1, function(x) {
  g1 <- gsub(x, pattern="(\\w+\\:).*", r="\\1")
  g2 <- gsub(x, pattern="\\w+\\:(.*)", r="\\1")
  as.character(as.character(g2[order(g1)]))
})))

#clean up and recombine
temp2 <- as.data.frame(do.call(rbind, strsplit(temp[,8], ",|(>\\\".*?\\\".*?\\K(, |$))", perl = TRUE)))
temp2$V2 <- description.data$ID
temp2$V3 <- description.data$Trial

temp2$V2[grep("tag",temp2$V2)]<-NA
temp2$V3[grep("tag",temp2$V3)]<-NA
temp3 <- cbind(temp, temp2)
temp4 <-as.data.frame(temp3)
#rename columns
colnames(temp4) <- c("description_content", "counterbalance", "description_stop", "importance", "offset", "phase", "tag", "time", "paired_vid", "vidpath", "scratch", "ID", "Trial")
#previous: colnames(temp4) <- c("phase", "counterbalance", "vidpath", "description_stop", "paired_vid", "offset", "description_content", "importance", "time", "scratch", "tag", "ID", "Trial")

temp <- temp4
#set all matching columns to integers or characters to facilitate matching in merge
temp$Trial <- trimws(as.integer(as.character(temp$Trial)))
temp$ID <- trimws(as.character(temp$ID))
description.data$Trial <- trimws(as.integer(as.character(description.data$Trial)))
description.data$ID <- trimws(as.character(description.data$ID))
#merge original with temp file
description.data <- merge(description.data,temp,by=c("ID","Trial")) 
#delete duplicate columns
description.data <- subset(description.data, select= -c(scratch))
description.data <- subset(description.data, select= -c(counterbalance.y))
description.data <- subset(description.data, select= -c(phase.y))
#rename columns
colnames(description.data)[colnames(description.data)=="counterbalance.x"] <- "counterbalance"
colnames(description.data)[colnames(description.data)=="phase.x"] <- "phase"

#working on cleaning up data from this point forward!
#use stringi to remove whitespaces for each data column; takes all whitespace and replace w/ nothing
#do this to every column in that is not supposed to be text
description.data$vidpath <- stri_replace_all_charclass(description.data$vidpath, "\\p{WHITE_SPACE}", "")
description.data$description_stop <- stri_replace_all_charclass(description.data$description_stop, "\\p{WHITE_SPACE}", "")
description.data$paired_vid <- stri_replace_all_charclass(description.data$paired_vid, "\\p{WHITE_SPACE}", "")
description.data$importance <- stri_replace_all_charclass(description.data$importance, "\\p{WHITE_SPACE}", "")
description.data$offset <- stri_replace_all_charclass(description.data$offset, "\\p{WHITE_SPACE}", "")
description.data$time <- stri_replace_all_charclass(description.data$time, "\\p{WHITE_SPACE}", "")
description.data$tag <- stri_replace_all_charclass(description.data$tag, "\\p{WHITE_SPACE}", "")

#edit the endings of mismatched columns to match other rows (in this case, move the } at the end of vidpath to tag to match)
mismatch_rows <- which(endsWith(description.data$vidpath, "}"))
description.data$vidpath[mismatch_rows] <- stri_sub(description.data$vidpath, 1)
description.data$tag[mismatch_rows] <- paste0(description.data$tag, "") #automatically adds a } when pasting, no need to specify?

#remove unneccessary text label from each data string
description.data$vidpath <- gsub('"', '', description.data$vidpath) #remove quotes
description.data$vidpath <- stri_sub(description.data$vidpath,9) #remove 9 characters from beginning
description.data$description_stop <- gsub('"', '', description.data$description_stop) #remove quotes
description.data$description_stop <- stri_sub(description.data$description_stop,10) #remove 10 characters from beginning
description.data$paired_vid <- gsub('"', '', description.data$paired_vid) #remove quotes
description.data$paired_vid <- stri_sub(description.data$paired_vid,9) #remove 9 characters from beginning
description.data$offset <- gsub('"', '', description.data$offset) #remove quotes
description.data$offset <- stri_sub(description.data$offset,8) #remove 8 characters from beginning
description.data$description_content <- gsub('"', '', description.data$description_content) #remove quotes
description.data$description_content <- stri_sub(description.data$description_content,10) #remove 10 characters from beginning
description.data$importance <- gsub('"', '', description.data$importance) #remove quotes
description.data$importance <- stri_sub(description.data$importance,12) #remove 12 characters from beginning
description.data$time <- gsub('"', '', description.data$time) #remove quotes
description.data$time <- stri_sub(description.data$time,6) #remove 6 characters from beginning
description.data$tag <- gsub('"', '', description.data$tag) #remove quotes
description.data$tag <- stri_sub(description.data$tag,5) #remove 5 characters from beginning
description.data$tag <- stri_sub(description.data$tag,1,-2) #remove 1 characters from end

#delete temporary dataframes
rm(list=ls(pattern='temp'))
#set columns to appropriate types
description.data$ID <- as.factor(description.data$ID)
description.data$phase <- as.factor(description.data$phase)
description.data$counterbalance <- as.factor(description.data$counterbalance)
description.data$test_trial <- as.factor(description.data$test_trial)
description.data$description_stop <- as.factor(description.data$description_stop)
description.data$paired_vid <- as.factor(description.data$paired_vid)
description.data$offset <- as.integer(description.data$offset)
description.data$importance <- as.integer(description.data$importance)
description.data$time <- as.integer(description.data$importance)

#add onset time stamps to description.data
#subset viewing data
viewing.data <- subset(trial.data, test_trial=="view")
#parse into segments
view_temp <- do.call(rbind, strsplit(viewing.data$Datastring, ", \"|(>\\\".*?\\\".*?\\K(, |$))", perl = TRUE))
view_temp <- as.data.frame(view_temp)
view_temp$V1 <- as.character(stri_sub(view_temp$V1,3)) #remove curly brackets from first column
#get rid of mismatched rows
view_temp <- as.data.frame(t(apply(as.data.frame(view_temp), 1, function(x) {
  g1 <- gsub(x, pattern="(\\w+\\:).*", r="\\1")
  g2 <- gsub(x, pattern="\\w+\\:(.*)", r="\\1")
  as.character(as.character(g2[order(g1)]))
})))
#add back cb + phase
view_temp$V10 <- viewing.data$counterbalance
view_temp$V11 <- viewing.data$phase
#delete unnecessary columns
view_temp <- subset(view_temp, select= -c(V1, V3, V4, V6, V9))
colnames(view_temp) <- c("offset", "description_target", "vidpath", "vidseg", "counterbalance", "phase")
#replicate description data cleaning
view_temp$vidseg <- stri_replace_all_charclass(view_temp$vidseg, "\\p{WHITE_SPACE}", "")
view_temp$offset <- stri_replace_all_charclass(view_temp$offset, "\\p{WHITE_SPACE}", "")
view_temp$description_target <- stri_replace_all_charclass(view_temp$description_target, "\\p{WHITE_SPACE}", "")
view_temp$vidseg <- gsub('"', '', view_temp$vidseg) #remove quotes
view_temp$vidseg <- stri_sub(view_temp$vidseg,8) #remove 8 characters from beginning
view_temp$offset <- gsub('"', '', view_temp$offset) #remove quotes
view_temp$offset <- stri_sub(view_temp$offset,8) #remove 8 characters from beginning
view_temp$description_target <- gsub('"', '', view_temp$description_target) #remove quotes
view_temp$description_target<- stri_sub(view_temp$description_target,8) #remove 8 characters from beginning
view_temp <- subset(view_temp, description_target=="not_target")
#create onset time reference table + prepare for merge
onset_ref<- view_temp %>% distinct(vidpath, phase, counterbalance, vidseg, offset)
onset_ref$paired_vid <- as.factor(as.numeric(onset_ref$vidseg) + 1)
onset_ref$onset <- onset_ref$offset
onset_ref <- subset(onset_ref, select= -c(vidpath, vidseg, offset))
onset_ref$phase <- as.factor(onset_ref$phase)
onset_ref$counterbalance <- as.factor(onset_ref$counterbalance)
onset_ref$onset <- as.integer(onset_ref$onset)
#merge onset reference table with description data
description_test <- merge(description.data, onset_ref, all.x = TRUE, all.y = FALSE)
#rearrange description.data in more readable order
col_order <- c("ID", "Timestamp", "Trial", "counterbalance", "phase","description_stop", "paired_vid", "onset", "offset", "description_content", "importance", "time", "tag", "vidpath", "Datastring", "test_trial")
description.data <- description_test[, col_order]
description.data <- arrange(description.data, ID, Timestamp)

#subset data by segment/counterbalance
#define function to clean and export description data for universal sentence encoder
export_clean_description <- function(df,phase,counterbalance,segment){
  df <- data.frame(lapply(df, function(x) {
    gsub("/", " ", x)})) #replace / with space
  df <- data.frame(lapply(df, function(x) {
    gsub("\\\\", "", x)})) #replace \ with space
  df <- lapply(df, function(x) stringi::stri_trans_general(x, "latin-ascii")) #remove all non-ascii characters
  df <- data.frame(matrix(unlist(df), nrow=length(df), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
  df <- as.data.frame(t(df)) #transpose
  write.table(df, file = paste0(clean_path,"cmiyc_long_",phase,"_c",counterbalance,"_seg",segment,".txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to csv
}
#define function to save all descriptions in raw format
export_raw_description <- function(df, phase){
  write.csv(df, file = paste0(raw_path,"cmiyc_long_",phase,"_full.csv")) #save full dataframe to csv
}

##phase: practice
##descriptions: all
#counterbalance: all (because, stops were identical)
#NOTE: the s in these label stand for "stop" rather than "segment" (used in prediction) b/c the stop are not 1:1 with video segments here
practice_cAllsAll <- subset(description.data, phase == "practice")
export_raw_description(practice_cAllsAll, "practice")
practice_cAlls1 <- subset(description.data, phase == "practice" & description_stop == "1", select = c(description_content))
export_clean_description(practice_cAlls1, "practice", "All", "1")

##phase: test
##descriptions: all
test_cAllsAll <- subset(description.data, phase == "test")
export_raw_description(test_cAllsAll, "test")

#counterbalance: 0
test_c0s1 <- subset(description.data, phase == "test" & counterbalance == 0 & description_stop == "1", select = c(description_content))
export_clean_description(test_c0s1, "test", "0", "1")
test_c0s2 <- subset(description.data, counterbalance == 0 & description_stop == "2", select = c(description_content))
export_clean_description(test_c0s2, "test", "0", "2")
test_c0s3 <- subset(description.data, counterbalance == 0 & description_stop == "3", select = c(description_content))
export_clean_description(test_c0s3, "test", "0", "3")
test_c0s4 <- subset(description.data, counterbalance == 0 & description_stop == "4", select = c(description_content))
export_clean_description(test_c0s4, "test", "0", "4")
test_c0s5 <- subset(description.data, counterbalance == 0 & description_stop == "5", select = c(description_content))
export_clean_description(test_c0s5, "test", "0", "5")
test_c0s6 <- subset(description.data, counterbalance == 0 & description_stop == "6", select = c(description_content))
export_clean_description(test_c0s6, "test", "0", "6")
test_c0s7 <- subset(description.data, counterbalance == 0 & description_stop == "7", select = c(description_content))
export_clean_description(test_c0s7, "test", "0", "7")
test_c0s8 <- subset(description.data, counterbalance == 0 & description_stop == "8", select = c(description_content))
export_clean_description(test_c0s8, "test", "0", "8")

#counterbalance: 1
test_c1s1 <- subset(description.data, phase == "test" & counterbalance == 1 & description_stop == "1", select = c(description_content))
export_clean_description(test_c1s1, "test", "1", "1")
test_c1s2 <- subset(description.data, counterbalance == 1 & description_stop == "2", select = c(description_content))
export_clean_description(test_c1s2, "test", "1", "2")
test_c1s3 <- subset(description.data, counterbalance == 1 & description_stop == "3", select = c(description_content))
export_clean_description(test_c1s3, "test", "1", "3")
test_c1s4 <- subset(description.data, counterbalance == 1 & description_stop == "4", select = c(description_content))
export_clean_description(test_c1s4, "test", "1", "4")
test_c1s5 <- subset(description.data, counterbalance == 1 & description_stop == "5", select = c(description_content))
export_clean_description(test_c1s5, "test", "1", "5")
test_c1s6 <- subset(description.data, counterbalance == 1 & description_stop == "6", select = c(description_content))
export_clean_description(test_c1s6, "test", "1", "6")
test_c1s7 <- subset(description.data, counterbalance == 1 & description_stop == "7", select = c(description_content))
export_clean_description(test_c1s7, "test", "1", "7")
test_c1s8 <- subset(description.data, counterbalance == 1 & description_stop == "8", select = c(description_content))
export_clean_description(test_c1s8, "test", "1", "8")

#counterbalance: 2
test_c2s1 <- subset(description.data, phase == "test" & counterbalance == 2 & description_stop == "1", select = c(description_content))
export_clean_description(test_c2s1, "test", "2", "1")
test_c2s2 <- subset(description.data, counterbalance == 2 & description_stop == "2", select = c(description_content))
export_clean_description(test_c2s2, "test", "2", "2")
test_c2s3 <- subset(description.data, counterbalance == 2 & description_stop == "3", select = c(description_content))
export_clean_description(test_c2s3, "test", "2", "3")
test_c2s4 <- subset(description.data, counterbalance == 2 & description_stop == "4", select = c(description_content))
export_clean_description(test_c2s4, "test", "2", "4")
test_c2s5 <- subset(description.data, counterbalance == 2 & description_stop == "5", select = c(description_content))
export_clean_description(test_c2s5, "test", "2", "5")
test_c2s6 <- subset(description.data, counterbalance == 2 & description_stop == "6", select = c(description_content))
export_clean_description(test_c2s6, "test", "2", "6")
test_c2s7 <- subset(description.data, counterbalance == 2 & description_stop == "7", select = c(description_content))
export_clean_description(test_c2s7, "test", "2", "7")
test_c2s8 <- subset(description.data, counterbalance == 2 & description_stop == "8", select = c(description_content))
export_clean_description(test_c2s8, "test", "2", "8")

#counterbalance: 3
test_c3s1 <- subset(description.data, phase == "test" & counterbalance == 3 & description_stop == "1", select = c(description_content))
export_clean_description(test_c3s1, "test", "3", "1")
test_c3s2 <- subset(description.data, counterbalance == 3 & description_stop == "2", select = c(description_content))
export_clean_description(test_c3s2, "test", "3", "2")
test_c3s3 <- subset(description.data, counterbalance == 3 & description_stop == "3", select = c(description_content))
export_clean_description(test_c3s3, "test", "3", "3")
test_c3s4 <- subset(description.data, counterbalance == 3 & description_stop == "4", select = c(description_content))
export_clean_description(test_c3s4, "test", "3", "4")
test_c3s5 <- subset(description.data, counterbalance == 3 & description_stop == "5", select = c(description_content))
export_clean_description(test_c3s5, "test", "3", "5")
test_c3s6 <- subset(description.data, counterbalance == 3 & description_stop == "6", select = c(description_content))
export_clean_description(test_c3s6, "test", "3", "6")
test_c3s7 <- subset(description.data, counterbalance == 3 & description_stop == "7", select = c(description_content))
export_clean_description(test_c3s7, "test", "3", "7")

#counterbalance: 4
test_c4s1 <- subset(description.data, phase == "test" & counterbalance == 4 & description_stop == "1", select = c(description_content))
export_clean_description(test_c4s1, "test", "4", "1")
test_c4s2 <- subset(description.data, counterbalance == 4 & description_stop == "2", select = c(description_content))
export_clean_description(test_c4s2, "test", "4", "2")
test_c4s3 <- subset(description.data, counterbalance == 4 & description_stop == "3", select = c(description_content))
export_clean_description(test_c4s3, "test", "4", "3")
test_c4s4 <- subset(description.data, counterbalance == 4 & description_stop == "4", select = c(description_content))
export_clean_description(test_c4s4, "test", "4", "4")
test_c4s5 <- subset(description.data, counterbalance == 4 & description_stop == "5", select = c(description_content))
export_clean_description(test_c4s5, "test", "4", "5")
test_c4s6 <- subset(description.data, counterbalance == 4 & description_stop == "6", select = c(description_content))
export_clean_description(test_c4s6, "test", "4", "6")
test_c4s7 <- subset(description.data, counterbalance == 4 & description_stop == "7", select = c(description_content))
export_clean_description(test_c4s7, "test", "4", "7")

#counterbalance: 5
test_c5s1 <- subset(description.data, phase == "test" & counterbalance == 5 & description_stop == "1", select = c(description_content))
export_clean_description(test_c5s1, "test", "5", "1")
test_c5s2 <- subset(description.data, counterbalance == 5 & description_stop == "2", select = c(description_content))
export_clean_description(test_c5s2, "test", "5", "2")
test_c5s3 <- subset(description.data, counterbalance == 5 & description_stop == "3", select = c(description_content))
export_clean_description(test_c5s3, "test", "5", "3")
test_c5s4 <- subset(description.data, counterbalance == 5 & description_stop == "4", select = c(description_content))
export_clean_description(test_c5s4, "test", "5", "4")
test_c5s5 <- subset(description.data, counterbalance == 5 & description_stop == "5", select = c(description_content))
export_clean_description(test_c5s5, "test", "5", "5")
test_c5s6 <- subset(description.data, counterbalance == 5 & description_stop == "6", select = c(description_content))
export_clean_description(test_c5s6, "test", "5", "6")
test_c5s7 <- subset(description.data, counterbalance == 5 & description_stop == "7", select = c(description_content))
export_clean_description(test_c5s7, "test", "5", "7")

#counterbalance: 6
test_c6s1 <- subset(description.data, phase == "test" & counterbalance == 6 & description_stop == "1", select = c(description_content))
export_clean_description(test_c6s1, "test", "6", "1")
test_c6s2 <- subset(description.data, counterbalance == 6 & description_stop == "2", select = c(description_content))
export_clean_description(test_c6s2, "test", "6", "2")
test_c6s3 <- subset(description.data, counterbalance == 6 & description_stop == "3", select = c(description_content))
export_clean_description(test_c6s3, "test", "6", "3")
test_c6s4 <- subset(description.data, counterbalance == 6 & description_stop == "4", select = c(description_content))
export_clean_description(test_c6s4, "test", "6", "4")
test_c6s5 <- subset(description.data, counterbalance == 6 & description_stop == "5", select = c(description_content))
export_clean_description(test_c6s5, "test", "6", "5")
test_c6s6 <- subset(description.data, counterbalance == 6 & description_stop == "6", select = c(description_content))
export_clean_description(test_c6s6, "test", "6", "6")
test_c6s7 <- subset(description.data, counterbalance == 6 & description_stop == "7", select = c(description_content))
export_clean_description(test_c6s7, "test", "6", "7")

#counterbalance: 7
test_c7s1 <- subset(description.data, phase == "test" & counterbalance == 7 & description_stop == "1", select = c(description_content))
export_clean_description(test_c7s1, "test", "7", "1")
test_c7s2 <- subset(description.data, counterbalance == 7 & description_stop == "2", select = c(description_content))
export_clean_description(test_c7s2, "test", "7", "2")
test_c7s3 <- subset(description.data, counterbalance == 7 & description_stop == "3", select = c(description_content))
export_clean_description(test_c7s3, "test", "7", "3")
test_c7s4 <- subset(description.data, counterbalance == 7 & description_stop == "4", select = c(description_content))
export_clean_description(test_c7s4, "test", "7", "4")
test_c7s5 <- subset(description.data, counterbalance == 7 & description_stop == "5", select = c(description_content))
export_clean_description(test_c7s5, "test", "7", "5")
test_c7s6 <- subset(description.data, counterbalance == 7 & description_stop == "6", select = c(description_content))
export_clean_description(test_c7s6, "test", "7", "6")
test_c7s7 <- subset(description.data, counterbalance == 7 & description_stop == "7", select = c(description_content))
export_clean_description(test_c7s7, "test", "7", "7")

#counterbalance: 8
test_c8s1 <- subset(description.data, phase == "test" & counterbalance == 8 & description_stop == "1", select = c(description_content))
export_clean_description(test_c8s1, "test", "8", "1")
test_c8s2 <- subset(description.data, counterbalance == 8 & description_stop == "2", select = c(description_content))
export_clean_description(test_c8s2, "test", "8", "2")
test_c8s3 <- subset(description.data, counterbalance == 8 & description_stop == "3", select = c(description_content))
export_clean_description(test_c8s3, "test", "8", "3")
test_c8s4 <- subset(description.data, counterbalance == 8 & description_stop == "4", select = c(description_content))
export_clean_description(test_c8s4, "test", "8", "4")
test_c8s5 <- subset(description.data, counterbalance == 8 & description_stop == "5", select = c(description_content))
export_clean_description(test_c8s5, "test", "8", "5")
test_c8s6 <- subset(description.data, counterbalance == 8 & description_stop == "6", select = c(description_content))
export_clean_description(test_c8s6, "test", "8", "6")
test_c8s7 <- subset(description.data, counterbalance == 8 & description_stop == "7", select = c(description_content))
export_clean_description(test_c8s7, "test", "8", "7")

##use USE (https://arxiv.org/abs/1803.11175) and tensorflow (https://www.tensorflow.org/tutorials) to get sentence-level embeddings 
#path to python script: /Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments//Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/cmiyc_long_1/predictions

##generate stop-specific full cosine similarity matrix
#attach relevant libraries
library(RColorBrewer)
library(corrplot)
#initialize empty lists
cosmat.list<-list()
cos.lowtri.list<-list()

##################
##USE timecourse##
##################
#load all USE csv files
setwd(paste0(description_path,"use/"))
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
    #note: matrices are reordered to show clustering
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
  labs(title="Cosine Similarity: USE in Bus Stop", size = 20)
ggsave(paste0(figure_path,"cmiyc_long_1_use_cosmat.pdf"), scale = 2)

#generate dataframe of pairwise USE similarity estimates per stop in movie
df <- t(plyr::ldply(cos.lowtri.list, rbind)) #list to dataframe
colnames(df) <- stri_sub(stri_sub(temp,13),1,-5) #rename columns
df <- df[,4:ncol(df)-1] #remove practice and title screen
df <- melt(df) #wide to long, for ggplot
df <- subset(df, select= c(Var2,value))
names(df)[names(df)=="value"] <- "similarity" #rename value
names(df)[names(df)=="Var2"] <- "description_stop" #rename variable

#dataframe is made, but now, order the stops to align with their occurrence in the movie
#this is important for the multipanel histogram plot
df$temp <- stri_sub(stri_sub(df$description_stop,6),1,-1) #create temp dataframe with counterbalance + segment info
df <- cbind(df,str_split_fixed(df$temp, "_", 2)) #separate counterbalance and segment into two columns
df$temp <- NULL #delete the temp column
names(df)[names(df)=="1"] <- "counterbalance" #rename counterbalance
names(df)[names(df)=="2"] <- "segment" #rename segment
df$segment <- as.numeric(stri_sub(stri_sub(df$segment,4),1,-1)) #clean up segment
df$counterbalance <- as.factor(as.numeric(stri_sub(stri_sub(df$counterbalance,2),1,-1))) #clean up counterbalance
df$counterbalance <- factor(df$counterbalance, levels = c("1","2","3","4","5","0")) #reorder factor levels 
df$label = factor(df$description_stop, levels=unique(df$description_stop[order(df$segment,df$counterbalance)]), ordered=TRUE) #reorder the factor level based on segment and counterbalance

#plot histograms of USE similarity for entire movie
ggplot(data=df, aes(similarity)) +
  facet_wrap(~ label) + 
  geom_histogram(aes(fill =..count..,y=..count..), 
                 breaks=seq(0, 1, by = .1), 
                 col="red") + 
  scale_fill_gradient("Count", low = "green", high = "red") +
  geom_vline(data = ddply(df, "label", summarise, label.mean = mean(similarity,na.rm=TRUE)), aes(xintercept = label.mean),col='black',size=.5,linetype="dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), plot.title = element_text(size=18)) +
  labs(title="Histogram: USE similarity in Bus Stop", size = 20) +
  labs(x="Similarity", y="Count")
ggsave(paste0(figure_path,"cmiyc_long_1_use_histogram.pdf"), scale = 2)

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
#CHECK IF NEXT STATEMENT WORKS
description.data$temp <- paste0(description.data$counterbalance, description.data$description_stop)
#originally: description.data$temp <- paste0(description.data$counterbalance, description.data$video_*segment)
use.data$temp <- paste0(use.data$counterbalance, use.data$segment)
#match this concatenated value across datasets to get offset values
use.data$offset<- with(description.data, offset[match(use.data$temp, temp)])
use.data$offset[use.data$temp == "NATitle"] <- 7 #relabel offset for title screen
use.data <- subset(use.data, !(temp %in% c("01", "11", "21", "31", "41", "51","NAA","NAB"))) #remove unnecessary datapoints
use.data$temp2 <- paste0(use.data$counterbalance, use.data$offset)

#calculate number of predictions per stop
#UPDATE FOR DESCRIPTION
use.data.n <- lapply(cosmat.list, function(x) sqrt(length(x))) #calculate number of predictions per stop
use.data.n  <- as.data.frame(plyr::ldply(use.data.n , rbind)) #list to dataframe
names(use.data.n) <- c("n") #rename column
use.data.n$label <- NA #create temporary values for column
for (i in 1:length(use_list)){
  use.data.n$label[i] <- stri_sub(stri_sub(temp[i],18),1,-5) #add label
}

#calculate SD across predictions per stop
#UPDATE FOR DESCRIPTION
use.data.sd <- lapply(cosmat.list, sd) #calculate number of predictions per stop
use.data.sd <- as.data.frame(plyr::ldply(use.data.sd , rbind)) #list to dataframe
names(use.data.sd) <- c("sd") #rename column
use.data.sd$label <- NA #create temporary values for column
for (i in 1:length(use_list)){
  use.data.sd$label[i] <- stri_sub(stri_sub(temp[i],18),1,-5) #add label
}

#calculate standard error
use.data$sd <- with(use.data.sd, sd[match(use.data$label, label)]) #match sd and add to df
use.data$n <- with(use.data.n, n[match(use.data$label, label)]) #match n and add to df
use.data$se <- use.data$sd/sqrt(use.data$n) #calculate se
rm(use.data.sd, use.data.n) #remove unnecessary dataframes

#plot average cosine similarity between USE embeddings for predictions at each stop
#UPDATE FOR DESCRIPTION
ggplot(data=use.data, aes(x=offset, y=similarity)) +
  geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7)
ggsave(paste0(figure_path,"cmiyc_long_1_use_timecourse.tiff"), dpi = 300, scale = 1)

#plot importance
#z-score importance ratings by subject
temp.conf <- ddply(subset(description.data,phase=="test"), ~ID, summarise, mean = mean(importance), sd = sd(importance), se = sd/sqrt(length(unique(description.data$ID))))
temp.conf2 <- ddply(subset(description.data,phase=="test"), ~ID + offset, summarise, mean = mean(importance))
temp.conf2$sub.mean <- with(temp.conf, mean[match(temp.conf2$ID, ID)]) 
temp.conf2$sub.sd <- with(temp.conf, sd[match(temp.conf2$ID, ID)]) 
temp.conf2$z <- (temp.conf2$mean - temp.conf2$sub.mean)/temp.conf2$sub.sd

#average all z-scored importance ratings per offset
importance.data <- ddply(temp.conf2, ~offset, summarise, z_mean = mean(z, na.rm=TRUE), z_sd = sd(z, na.rm=TRUE)) #average per stop
importance.data$counterbalance <- with(description.data, counterbalance[match(importance.data$offset, offset)]) #get matching counterbalance
importance.data$counterbalance[importance.data$offset == 7] <- NA
importance.data$temp2 <- paste0(importance.data$counterbalance, importance.data$offset) #create a tag for matching
importance.data$n <- with(use.data, n[match(importance.data$temp2, temp2)]) #get number of predictions per stop
importance.data$z_se <- importance.data$z_sd/sqrt(importance.data$n) #calculate se 

ggplot(data=importance.data, aes(x=offset, y=z_mean)) +
  geom_errorbar(aes(ymin=z_mean-z_se, ymax=z_mean+z_se), width=.1, linetype=5, alpha=.4) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("importance (z)") + ylim(-1, 1)
ggsave(paste0(figure_path,"cmiyc_long_1_importance_timecourse.tiff"), dpi = 300, scale = 1)

#correlate importance with USE cosine similarity
use.data$offset <- as.numeric(use.data$offset) #change offset to numeric
use.data <- use.data[order(use.data$offset),] #order by offset
importance.data$offset <- as.numeric(importance.data$offset) #change offset to numeric
importance.data <- importance.data[order(importance.data$offset),] #order by offset
cor.test(importance.data$z_mean, use.data$similarity) #run correlation test


###############
#load accuracy#
###############

#load accuracy data (from another script: generate_stopwise_cormat_Feb042020.r)
accuracy.data <- read.csv(paste0(description_path,"cmiyc_long_1_stopwise_accuracy.csv"), header = TRUE, comment.char="#")
accuracy.data <- subset(accuracy.data,select=-c(X)) #remove unnecessary column
accuracy.data$offset <- as.numeric(accuracy.data$offset) #change offset to numeric
accuracy.data$counterbalance <- as.factor(accuracy.data$counterbalance) #change counterbalance to factor
accuracy.data <- accuracy.data[order(accuracy.data$offset),] #order by offset
colnames(accuracy.data)[colnames(accuracy.data) == 'cosine'] <- 'similarity' #rename column for consistency

#plot average cosine similarity between USE embeddings for predictions and annotations at each stop
ggplot(data=accuracy.data, aes(x=offset, y=similarity)) +
  geom_line(colour="black", linetype="solid", size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("accuracy") + ylim(.2, .7)
ggsave(paste0(figure_path,"cmiyc_long_1_accuracy_timecourse.png"), dpi = 300, scale = 1)

#correlate importance, USE cosine similarity and accuracy
use.data$offset <- as.numeric(use.data$offset) #change offset to numeric
use.data <- use.data[order(use.data$offset),] #order by offset
importance.data$offset <- as.numeric(importance.data$offset) #change offset to numeric
importance.data <- importance.data[order(importance.data$offset),] #order by offset
#run correlation tests
cor.test(importance.data$z_mean, use.data$similarity) 
cor.test(importance.data$z_mean, accuracy.data$similarity)
cor.test(use.data$similarity, accuracy.data$similarity) 

#add experiment label
importance.data$experiment <- "cmiyc_long_1"
use.data$experiment <- "cmiyc_long_1"
accuracy.data$experiment <- "cmiyc_long_1"

#save summary dataframes as csv
write.csv(importance.data, file = paste0(description_path,"_importance_timecourse.csv")) #save full dataframe to csv
write.csv(use.data, file = paste0(description_path,"_consensus_timecourse.csv")) #save full dataframe to csv
write.csv(accuracy.data, file = paste0(description_path,"_accuracy_timecourse.csv")) #save full dataframe to csv

#scatter importance ~ USE similarity
library(ggpubr)

ggplot(importance.data,aes(x=z_mean, y=use.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "importance (z)", y = "consensus") + 
  scale_y_continuous(limits = c(0.2,.7)) +
  scale_x_continuous(limits = c(-.8,.8)) +
  stat_cor(method = "pearson", label.x = (median((-8:8)/10)), label.y = .675) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,"cmiyc_long_1_use_conf_scatter.png"), scale = 1)

ggplot(importance.data,aes(x=z_mean, y=accuracy.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "importance (z)", y = "accuracy") + 
  scale_y_continuous(limits = c(0.2,.7)) +
  scale_x_continuous(limits = c(-.8,.8)) +
  stat_cor(method = "pearson", label.x = (median((-8:8)/10)), label.y = .675) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,"cmiyc_long_1_accuracy_conf_scatter.png"), scale = 1)

ggplot(accuracy.data,aes(x=similarity, y=use.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "accuracy", y = "consensus") + 
  scale_y_continuous(limits = c(0.2,.7)) +
  scale_x_continuous(limits = c(0.2,.7)) +
  stat_cor(method = "pearson", label.x = .6, label.y = .675) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,"cmiyc_long_1_accuracy_use_scatter.png"), scale = 1)



##upsample USE/importance/accuracy timecourses to fMRI timecourse
#spline for use
use.spline.interp <- spline(use.data$offset, use.data$similarity, method="natural", n=100000, xmin=0, xmax=665)
use.spline.interp <- data.frame(matrix(unlist(use.spline.interp), nrow=length(use.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
use.spline.interp <- data.frame(matrix(unlist(use.spline.interp), nrow=length(use.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #go long from wide
names(use.spline.interp) <- c("offset", "similarity") #rename column

#spline for importance
conf.spline.interp <- spline(importance.data$offset, importance.data$z_mean, method="natural", n=100000, xmin=0, xmax=665)
conf.spline.interp <- data.frame(matrix(unlist(conf.spline.interp), nrow=length(conf.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
conf.spline.interp <- data.frame(matrix(unlist(conf.spline.interp), nrow=length(conf.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(conf.spline.interp) <- c("offset", "importance") #rename column

#spline for accuracy
acc.spline.interp <- spline(accuracy.data$offset, accuracy.data$similarity, method="natural", n=100000, xmin=0, xmax=665)
acc.spline.interp <- data.frame(matrix(unlist(acc.spline.interp), nrow=length(acc.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
acc.spline.interp <- data.frame(matrix(unlist(acc.spline.interp), nrow=length(acc.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(acc.spline.interp) <- c("offset", "accuracy") #rename column

#build a vector representing each TR, in seconds, for the cmiyc_long_1 movie
tr <- as.data.frame(seq(from = 0, to = 425, by = 1.5))
names(tr) <- c("s") #rename column

#load MALDIquant for match.closest function
library(MALDIquant)

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(tr$s)){
  tr$position_match[i] <- match.closest(tr$s[i], use.spline.interp$offset, tolerance = Inf, nomatch = 0000) #find matching offsets
  tr$similarity[i] <- use.spline.interp$similarity[tr$position_match[i]] #find corresponding similarity estimates 
  tr$importance[i] <- conf.spline.interp$importance[tr$position_match[i]] #find corresponding importance estimates 
  tr$accuracy[i] <- acc.spline.interp$accuracy[tr$position_match[i]] #find corresponding accuracy estimates 
}

#export as csv
#raw, not-interpolated, full-length
df.orig.use <- subset(use.data, select = similarity)
write.table(df.orig.use, file = paste0(description_path,"cmiyc_long_1_use_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.orig.use, file = paste0(description_path,"cmiyc_long_1_use_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
df.orig.conf <- subset(importance.data, select = z_mean)
write.table(df.orig.conf, file = paste0(description_path,"cmiyc_long_1_conf_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.orig.conf, file = paste0(description_path,"cmiyc_long_1_conf_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
df.orig.acc <- subset(accuracy.data, select = similarity)
write.table(df.orig.acc, file = paste0(description_path,"cmiyc_long_1_acc_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.orig.acc, file = paste0(description_path,"cmiyc_long_1_acc_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv

write.csv(tr, file = paste0(description_path,"cmiyc_long_1_full_interp_tr_match.csv")) #save full dataframe to csv
df.use <- subset(tr, select = similarity)
#df.use <- df.use[15:length(t(df.use)),] #remove first 14 trs (to deal with an intial transient in the BOLD data)
#write.table(df.use, file = paste0(description_path,"cmiyc_long_1_use_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
#write.table(df.use, file = paste0(description_path,"cmiyc_long_1_use_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt
write.table(df.use, file = paste0(description_path,"cmiyc_long_1_use_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.use, file = paste0(description_path,"cmiyc_long_1_use_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt
df.conf <- subset(tr, select = importance)
#df.conf <- df.conf[15:length(t(df.conf)),] #remove first 14 trs (to deal with an intial transient in the BOLD data) 
#write.table(df.conf, file = paste0(description_path,"cmiyc_long_1_conf_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
#write.table(df.conf, file = paste0(description_path,"cmiyc_long_1_conf_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt
write.table(df.conf, file = paste0(description_path,"cmiyc_long_1_conf_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.conf, file = paste0(description_path,"cmiyc_long_1_conf_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt
df.acc <- subset(tr, select = accuracy)
#df.acc <- df.conf[15:length(t(df.acc)),] #remove first 14 trs (to deal with an intial transient in the BOLD data) 
#write.table(df.acc, file = paste0(description_path,"cmiyc_long_1_acc_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
#write.table(df.acc, file = paste0(description_path,"cmiyc_long_1_acc_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt
write.table(df.acc, file = paste0(description_path,"cmiyc_long_1_acc_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save description column only, without row or column labels, to csv
write.table(df.acc, file = paste0(description_path,"cmiyc_long_1_acc_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save description column only, without row or column labels, to txt

#double check correlation between importance and use similarity and accuracy
#so long as everything worked properly:
#should show similar magnitude (r values) to original correlation 
cor.test(tr$importance, tr$similarity) #run correlation test
cor.test(tr$accuracy, tr$similarity) #run correlation test
cor.test(tr$importance, tr$accuracy) #run correlation test

#plot TR rate estimates 
#for use cosine similarity
ggplot(data=tr, aes(x=s, y=similarity)) +
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7)
ggsave(paste0(figure_path,"cmiyc_long_1_use_upsampled_timecourse.png"), dpi = 300, scale = 1)

#for importance
ggplot(data=tr, aes(x=s, y=importance)) +
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("importance (z)") + ylim(-1, 1)
ggsave(paste0(figure_path,"cmiyc_long_1_importance_upsampled_timecourse.png"), dpi = 300, scale = 1)

#for accuracy
ggplot(data=tr, aes(x=s, y=accuracy)) +
  geom_line(size=1) + 
  geom_point(size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("accuracy") + ylim(.2, .7)
ggsave(paste0(figure_path,"cmiyc_long_1_accuracy_upsampled_timecourse.png"), dpi = 300, scale = 1)

###################
##ROI timecourses##
###################
#delete temporary dataframes
rm(list=ls(pattern='*temp*'))

#load group averaged ROI timecourses
setwd("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/cmiyc_long_1/roi_tc/tc")
temp = list.files(pattern="*.txt")
tc_list = lapply(temp, function(x) read.delim(x, header = FALSE, sep = ","))
#load group averaged pISC ROI timecourses
setwd("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/cmiyc_long_1/roi_tc/pisc")
temp2 = list.files(pattern="*.txt")
pisc_list = lapply(temp2, function(x) read.delim(x, header = FALSE, sep = ","))

#add offset column per element in list
#for tc
for (i in 1:length(tc_list)){
  names(tc_list[[i]]) <- c("group mean") #rename column
  tc_list[[i]]$s <- as.numeric(rownames(tc_list[[i]]))*1.5
}

#for pisc
for (i in 1:length(pisc_list)){
  names(pisc_list[[i]]) <- c("group pISC") #rename column
  pisc_list[[i]]$s <- as.numeric(rownames(pisc_list[[i]]))*1.5
}

#prepare dataframes for combined ggplot
#interpolated similarity + importance
tr_long <- tr #copy df
tr_long$consensus <- scale(tr_long$similarity) #zscore similarity (i.e., consensus)
tr_long$importance <- scale(tr_long$importance) #zscore importance
tr_long$accuracy <- as.numeric(scale(tr_long$accuracy)) #zscore accuracy
tr_long <- melt(subset(tr_long, select = c(s,consensus,importance,accuracy)), id.vars=c("s")) #melt wide to long
names(tr_long) <- c("s","type","estimate") #rename columns

#roi loop
for (i in 1:length(tc_list)){
  #mean timecourse
  tc_long<- melt(tc_list[[i]], id.vars=c("s")) #melt wide to long
  names(tc_long) <- c("s","type","estimate") #rename columns
  tc_long$estimate  <- scale(tc_long$estimate) #zscore
  #mean pattern ISC
  pisc_long <- melt(pisc_list[[i]], id.vars=c("s")) #melt wide to long
  names(pisc_long) <- c("s","type","estimate") #rename columns
  pisc_long$estimate  <- scale(pisc_long$estimate) #zscore
  
  #merge into one df
  combined_tc_df <- rbind(tr_long, tc_long, pisc_long )
  
  #prepare separate df for original similarity estimates for overlay
  use_temp <- subset(use.data, select=c(counterbalance,segment,offset,similarity))
  use_temp$z <- scale(use_temp$similarity)
  
  #set path
  setwd("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/cmiyc_long_1/roi_tc/")
  
  #plot combined timecourses for ROI + non-interpolated USE overlay
  ggplot(data=combined_tc_df, aes(x=s, y=estimate)) +
    facet_wrap(~ type, ncol = length(unique(tr_long$estimate))) + 
    geom_line(size=1) + 
    geom_point(data=use_temp,aes(x=offset, y=z,colour=counterbalance),size=2) + 
    geom_line(data=use_temp,aes(x=offset, y=z),size=1,alpha=.3) + 
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("time (s)") + ylab("Z") + ylim(-4, 4)
  ggsave(paste0("cmiyc_long_1_",stri_sub(stri_sub(temp[i],12),1,-16),"_timecourses.png"), scale = 2)
  
}



#####################
##Human annotations##
#####################
#prep human rated clustering data and generate summary statistics
#these data provide a validation for the USE consensus metric (i.e., does average USE cosine similarity relate to human judgements of semantic relatedness)

#define movie of interest
#potential labels: "cmiyc_long", "busstop", "keithreynolds", "theboyfriend", "therock", "theshoe"
movie <- "cmiyc_long_1"
#define rater:
#possible options: "EMH"
annotater <- "EMH"

#load paths
figure_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/figures/")
data_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/")

#load raw human-rater timecourse
input.rater <- read.delim(paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/_human_clustering/PredictionProbe_Clustering_",movie,"_",annotater,".csv"), header = TRUE, sep = ",")

#insane round-about code to go from input data to summarized version
temp.rater <- data.frame() #create empty df
rater <- data.frame() #create empty df
for (i in unique(input.rater$Label)){ #label loop
  for (j in head(unique(c(substring(as.character(input.rater$Clustering),1,1),substring(as.character(input.rater$Clustering),2,2))),-1)[order(as.character(head(unique(c(substring(as.character(input.rater$Clustering),1,1),substring(as.character(input.rater$Clustering),2,2))),-1)))]){ #crazy command that get the total list of unique clusters (while excluding conjunctions) and sorts them alphabetically
    temp.rater <- as.data.frame(i) #load label into df
    names(temp.rater)[names(temp.rater) == 'i'] <- 'Label' #rename column
    temp.rater$temp <- stri_sub(stri_sub(temp.rater$Label,6),1,-1) #create temp column with counterbalance + segment info
    temp.rater <- cbind(temp.rater,str_split_fixed(temp.rater$temp, "_", 2)) #separate counterbalance and segment into two columns
    temp.rater$temp <- NULL #delete the temp column
    names(temp.rater)[names(temp.rater)=="1"] <- "Counterbalance" #rename counterbalance
    names(temp.rater)[names(temp.rater)=="2"] <- "Segment" #rename segment
    temp.rater$Segment<- as.numeric(stri_sub(stri_sub(temp.rater$Segment,4),1,-1)) #clean up segment
    temp.rater$Counterbalance <- as.factor(as.numeric(stri_sub(stri_sub(temp.rater$Counterbalance,2),1,-1))) #clean up counterbalance
    temp.rater$Counterbalance <- factor(temp.rater$Counterbalance, levels = c("0","1","2","3","4","5")) #reorder factor levels 
    temp.rater$Label = factor(temp.rater$Label, levels=unique(temp.rater$Label[order(temp.rater$Segment,temp.rater$Counterbalance)]), ordered=TRUE) #reorder the factor level based on segment and counterbalance
    temp.rater$Cluster <- j #create cluster column
    temp.rater$Count <- length(grep(j, as.character(subset(input.rater, Label==i)$Clustering))) #count occurence of cluster label in all predictions for a given stop
    rater <- rbind(rater,temp.rater) #bind into df
  }
}

#calculate necessary metrics
rater <- subset(rater, Count>0) #remove all rows where certain clusters were not used
rater$PredTotal <- with(ddply(rater, "Label", summarise, PredTotal = sum(Count)), PredTotal[match(rater$Label, Label)]) #get total prediction count per label
rater$Percent <- (rater$Count/rater$PredTotal)*100 #calculate percentage of total predictions per cluster

#generate summary statistics per stop
rater.summary <- ddply(rater, "Label", summarise, percent.mean = mean(Percent,na.rm=TRUE))
rater.summary$percent.max <- ddply(rater, "Label", summarise, percent.max = max(Percent,na.rm=TRUE))$percent.max
rater.summary$count.cluster <- ddply(rater, "Label", summarise, count.cluster = length(Cluster))$count.cluster

#some more data preparation
rater.summary$offset <- with(use.data, offset[match(rater.summary$Label, label)]) #get offset time in seconds
rater.summary$offset[is.na(rater.summary$offset)] <- 4 #set NAs to 4
rater.summary <- ddply(rater.summary, "offset", summarise, percent.mean = mean(percent.mean,na.rm=TRUE), percent.max = mean(percent.max,na.rm=TRUE), count.cluster = mean(count.cluster,na.rm=TRUE)) #summarize all metrics per stop
rater.summary$label <- with(use.data, label[match(rater.summary$offset, offset)]) #get segment label
rater.summary$segment <- with(rater, Segment[match(rater.summary$label, Label)]) #get segment label
rater.summary$counterbalance <- with(rater, Counterbalance[match(rater.summary$label, Label)]) #get counterbalance
rater.summary$counterbalance <- as.factor(rater.summary$counterbalance)

#export file
write.csv(rater.summary, file = paste0(data_path,movie,"_human_rater_summary_",annotater,".csv")) #save full dataframe to csv

#plot average cosine similarity between USE embeddings for predictions at each stop
ggplot(data=rater.summary, aes(x=offset, y=percent.mean)) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("average cluster size (% of total predictions)") + ylim(0, 40)
ggsave(paste0(figure_path,movie,"_human_mean_timecourse_",annotater,".png"), scale = 1)

#plot max cosine similarity between USE embeddings for predictions at each stop
ggplot(data=rater.summary, aes(x=offset, y=percent.max)) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("max cluster size (% of total predictions)") + ylim(0, 100)
ggsave(paste0(figure_path,movie,"_human_max_timecourse_",annotater,".png"), scale = 1)

#plot average number of clusters for predictions at each stop
ggplot(data=rater.summary, aes(x=offset, y=count.cluster)) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("# of human-rated clusters") + ylim(0, 20)
ggsave(paste0(figure_path,movie,"_human_count_timecourse_",annotater,".png"), scale = 1)

#prepare data to run correlations
use.data$offset <- as.numeric(use.data$offset) #change offset to numeric
use.data <- use.data[order(use.data$offset),] #order by offset
importance.data$offset <- as.numeric(importance.data$offset) #change offset to numeric
importance.data <- importance.data[order(importance.data$offset),] #order by offset
rater.summary$offset <- as.numeric(rater.summary$offset) #change offset to numeric
rater.summary <- rater.summary[order(rater.summary$offset),] #order by offset
#rater.summary.emh <- rater.summary  #if you want to correlate EMH and KW ratings, use this to save a temp variable

#run correlation tests
cor.test(importance.data$z_mean, use.data$similarity) #run correlation test
cor.test(rater.summary$percent.mean, use.data$similarity) #run correlation test
cor.test(rater.summary$percent.mean, importance.data$z_mean) #run correlation test
cor.test(rater.summary$percent.mean, accuracy.data$similarity) #run correlation test
cor.test(rater.summary$percent.max, use.data$similarity) #run correlation test
cor.test(rater.summary$percent.max, importance.data$z_mean) #run correlation test
cor.test(rater.summary$percent.max, accuracy.data$similarity) #run correlation test
cor.test(rater.summary$percent.mean, rater.summary$percent.max) #run correlation test
cor.test(rater.summary$count.cluster, use.data$similarity) #run correlation test
cor.test(rater.summary$count.cluster, accuracy.data$similarity) #run correlation test
cor.test(rater.summary$count.cluster, importance.data$z_mean) #run correlation test
cor.test(rater.summary$count.cluster, rater.summary$percent.mean) #run correlation test
cor.test(rater.summary$count.cluster, rater.summary$percent.max) #run correlation test

##plot the relationships between factors
library(ggpubr)

#human rated clusters ~ USE similarity
ggplot(rater.summary,aes(x=count.cluster, y=use.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "# of human-rated clusters", y = "prediction consensus") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,20)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_count_use_scatter_",annotater,".png"), scale = 1)

#human rated clusters ~ accuracy
ggplot(rater.summary,aes(x=count.cluster, y=accuracy.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "# of human-rated clusters", y = "accuracy") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,20)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_count_accuracy_scatter_",annotater,".png"), scale = 1)

#human rated clusters ~ importance
ggplot(rater.summary,aes(x=count.cluster, y=importance.data$z_mean))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "# of human-rated clusters", y = "importance (z)") + 
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(1,20)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_count_importance_scatter_",annotater,".png"), scale = 1)

#human rated percent mean ~ USE similarity
ggplot(rater.summary,aes(x=percent.mean, y=use.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "average % of participants in a cluster", y = "prediction consensus") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,40)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_mean_use_scatter_",annotater,".png"), scale = 1)

#human rated clusters ~ accuracy
ggplot(rater.summary,aes(x=percent.mean, y=accuracy.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "average % of participants in a cluster", y = "accuracy") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,40)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_mean_accuracy_scatter_",annotater,".png"), scale = 1)

#human rated clusters ~ importance
ggplot(rater.summary,aes(x=percent.mean, y=importance.data$z_mean))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "average % of participants in a cluster", y = "importance (z)") + 
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(1,40)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_mean_importance_scatter_",annotater,".png"), scale = 1)

#human rated percent max ~ USE similarity 
ggplot(rater.summary,aes(x=percent.max, y=use.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "% of participants in largest cluster", y = "prediction consensus") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,80)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_max_use_scatter_",annotater,".png"), scale = 1)

#human rated percent max ~ accuracy
ggplot(rater.summary,aes(x=percent.max, y=accuracy.data$similarity))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "% of participants in largest cluster", y = "accuracy") + 
  scale_y_continuous(limits = c(0,.8)) +
  scale_x_continuous(limits = c(1,80)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_max_accuracy_scatter_",annotater,".png"), scale = 1)

#human rated percent max ~ importance
ggplot(rater.summary,aes(x=percent.max, y=importance.data$z_mean))+
  geom_point(aes(colour=counterbalance),size =3) + 
  geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
  scale_fill_grey(name=element_blank()) +  
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
  labs(x = "% of participants in largest cluster", y = "importance (z)") + 
  scale_y_continuous(limits = c(-1,1)) +
  scale_x_continuous(limits = c(1,80)) + 
  stat_cor(method = "pearson", label.x = (median(1:20)), label.y = .75) #from ggpubr -- plots r and p-value
ggsave(paste0(figure_path,movie,"_human_max_importance_scatter_",annotater,".png"), scale = 1)

#Plot # of clusters and USE timecourse on same scale
rater.summary$z.count.cluster <- scale(rater.summary$count.cluster)*-1
rater.summary$z.percent.mean <- scale(rater.summary$percent.mean)
rater.summary$z.percent.max <- scale(rater.summary$percent.max)
use.data$z.similarity <- scale(use.data$similarity)
rater.summary$z.diff <- abs(use.data$z.similarity - rater.summary$z.count.cluster)

#plot average number of clusters for predictions at each stop
ggplot(data=rater.summary, aes(x=offset, y=z.count.cluster)) +
  geom_line(size=1) + 
  geom_point(aes(colour=counterbalance),size=2) + 
  geom_line(data=use.data,aes(x=use.data$offset, y=use.data$z.similarity), size=1, colour="grey", linetype="dashed") + 
  geom_point(data=use.data,aes(x=use.data$offset, y=use.data$z.similarity, colour=as.factor(use.data$counterbalance)),size=2) + 
  theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
  xlab("time (s)") + ylab("z") + ylim(-5,5)
ggsave(paste0(figure_path,movie,"_human_count_use_ztc_",annotater,".png"), scale = 2:1)

#####
###interpolate human-rated timecourses to 0.6666667 Hz (i.e., equivalent to 1.5 TR sampling rate)
#####

#spline for count cluster (i.e., number of distinct semantic clusters, as rated by an RA)
count.cluster.spline.interp <- spline(rater.summary$offset, rater.summary$count.cluster, method="natural", n=100000, xmin=0, xmax=665)
count.cluster.spline.interp <- data.frame(matrix(unlist(count.cluster.spline.interp), nrow=length(count.cluster.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
count.cluster.spline.interp <- data.frame(matrix(unlist(count.cluster.spline.interp), nrow=length(count.cluster.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(count.cluster.spline.interp) <- c("offset", "count.cluster") #rename column

#spline for percent max (i.e., maximum % of participants whose predictions belonged to ONE distinct semantic cluster, as rated by an RA)
percent.max.spline.interp <- spline(rater.summary$offset, rater.summary$percent.max, method="natural", n=100000, xmin=0, xmax=665)
percent.max.spline.interp <- data.frame(matrix(unlist(percent.max.spline.interp), nrow=length(percent.max.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
percent.max.spline.interp <- data.frame(matrix(unlist(percent.max.spline.interp), nrow=length(percent.max.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(percent.max.spline.interp) <- c("offset", "percent.max") #rename column

#spline for percent max (i.e., average % of participants whose predictions belonged any given distinct semantic cluster, as rated by an RA)
percent.mean.spline.interp <- spline(rater.summary$offset, rater.summary$percent.mean, method="natural", n=100000, xmin=0, xmax=665)
percent.mean.spline.interp <- data.frame(matrix(unlist(percent.mean.spline.interp), nrow=length(percent.mean.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
percent.mean.spline.interp <- data.frame(matrix(unlist(percent.mean.spline.interp), nrow=length(percent.mean.spline.interp), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
names(percent.mean.spline.interp) <- c("offset", "percent.mean") #rename column

#load MALDIquant for match.closest function
library(MALDIquant)

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(tr$s)){
  tr$count.cluster[i] <- count.cluster.spline.interp$count.cluster[tr$position_match[i]] #find corresponding count cluster estimates 
  tr$percent.max[i] <- percent.max.spline.interp$percent.max[tr$position_match[i]] #find corresponding percent max estimates 
  tr$percent.mean[i] <- percent.mean.spline.interp$percent.mean[tr$position_match[i]] #find corresponding percent mean estimates 
}


#export as csv
#raw, not-interpolated, full-length
df.orig.count.cluster <- subset(rater.summary, select = count.cluster)
write.table(df.orig.count.cluster, file = paste0(description_path,movie,"_humancountcluster_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.orig.count.cluster, file = paste0(description_path,movie,"_humancountcluster_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
df.orig.percent.max <- subset(rater.summary, select = percent.max)
write.table(df.orig.percent.max, file = paste0(description_path,movie,"_humanpercentmax_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.orig.percent.max, file = paste0(description_path,movie,"_humanpercentmax_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
df.orig.percent.mean <- subset(rater.summary, select = percent.mean)
write.table(df.orig.percent.mean, file = paste0(description_path,movie,"_humanpercentmean_orig.txt"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.orig.percent.mean, file = paste0(description_path,movie,"_humanpercentmean_orig.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv

#interpolated data
write.csv(tr, file = paste0(description_path,movie,"_full_interp_tr_match.csv")) #save full dataframe to csv
df.count.cluster <- subset(tr, select = count.cluster)
#df.count.cluster <- df.count.cluster[15:length(t(df.count.cluster)),] #remove first 14 trs (to deal with an intial transient in the BOLD data)
#write.table(df.count.cluster, file = paste0(description_path,movie,"_countcluster_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
#write.table(df.count.cluster, file = paste0(description_path,movie,"_countcluster_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt
write.table(df.count.cluster, file = paste0(description_path,movie,"_countcluster_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.count.cluster, file = paste0(description_path,movie,"_countcluster_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt

df.percent.max <- subset(tr, select = percent.max)
#df.percent.max <- df.percent.max[15:length(t(df.percent.max)),] #remove first 14 trs (to deal with an intial transient in the BOLD data)
#write.table(df.percent.max, file = paste0(description_path,movie,"_percentmax_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
#write.table(df.percent.max, file = paste0(description_path,movie,"_percentmax_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt
write.table(df.percent.max, file = paste0(description_path,movie,"_percentmax_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.percent.max, file = paste0(description_path,movie,"_percentmax_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt

df.percent.mean <- subset(tr, select = percent.mean)
#df.percent.mean <- df.percent.mean[15:length(t(df.percent.mean)),] #remove first 14 trs (to deal with an intial transient in the BOLD data)
#write.table(df.percent.mean, file = paste0(description_path,movie,"_percentmean_tr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
#write.table(df.percent.mean, file = paste0(description_path,movie,"_percentmean_tr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt
write.table(df.percent.mean, file = paste0(description_path,movie,"_percentmean_filmfesttr_match.csv"), row.names=FALSE, col.names=FALSE, sep=",", quote = FALSE) #save data column only, without row or column labels, to csv
write.table(df.percent.mean, file = paste0(description_path,movie,"_percentmean_filmfesttr_match.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save data column only, without row or column labels, to txt


# #####look at reliability across raters#####
# #note: need to run the above section for each rater, and save the rater.summary.emh variable (EMH's rating) so it is not overwritten. then, you can compare the twoo raters
# #run correlation tests
# cor.test(rater.summary.emh$percent.mean, rater.summary$percent.mean) #run correlation test
# cor.test(rater.summary.emh$percent.max, rater.summary$percent.max) #run correlation test
# cor.test(rater.summary.emh$count.cluster, rater.summary$count.cluster) #run correlation test
# 
# #reliability of percent mean (i.e., average cluster size)
# ggplot(rater.summary,aes(x=percent.mean, y=rater.summary.emh$percent.mean))+
#   geom_point(aes(colour=counterbalance),size =3) + 
#   geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
#   scale_fill_grey(name=element_blank()) +  
#   theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
#   labs(x = "average cluster size (%; KW)", y = "average cluster size (%; EMH)") + 
#   scale_y_continuous(limits = c(0,40)) +
#   scale_x_continuous(limits = c(0,40))
# ggsave(paste0(figure_path,movie,"_human_mean_interrater_reliability_",annotater,".png"), scale = 1)
# 
# #reliability of percent max (i.e., max cluster size)
# ggplot(rater.summary,aes(x=percent.max, y=rater.summary.emh$percent.max))+
#   geom_point(aes(colour=counterbalance),size =3) + 
#   geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
#   scale_fill_grey(name=element_blank()) +  
#   theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
#   labs(x = "max cluster size (%; KW)", y = "max cluster size (%; EMH)") + 
#   scale_y_continuous(limits = c(0,100)) +
#   scale_x_continuous(limits = c(0,100))
# ggsave(paste0(figure_path,movie,"_human_max_interrater_reliability_",annotater,".png"), scale = 1)
# 
# #reliability of count
# ggplot(rater.summary,aes(x=count.cluster, y=rater.summary.emh$count.cluster))+
#   geom_point(aes(colour=counterbalance),size =3) + 
#   geom_smooth(colour="black", method='lm',formula=y~x, fill="lightgrey") + 
#   scale_fill_grey(name=element_blank()) +  
#   theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1), legend.position = "none") + 
#   labs(x = "# of human-rated clusters (KW)", y = "# of human-rated clusters (EMH)") + 
#   scale_y_continuous(limits = c(1,20)) +
#   scale_x_continuous(limits = c(1,20))
# ggsave(paste0(figure_path,movie,"_human_count_interrater_reliability_",annotater,".png"), scale = 1)


########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################      
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################      
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################      
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################      
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################      

################################################
#0. GENERATE MOVIE-SPECIFIC OMNIBUS OUTPUT FILE# 
################################################
#create a csv file with upsampled and raw USE consensus and importance ratings on a 500ms timescale
#then, using this "common time space" append all behavioural ratings/movie annotations/importance ratings/clustering/PMC activity onto the same datafile

#define movie of interest
#potential labels: "cmiyc_long", "cmiyc_long_1", "keithreynolds", "theboyfriend", "therock", "theshoe"
movie <- "cmiyc_long_1"

#load paths
figure_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/figures/")
data_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/")

####################################################################################
#1. Create common time-space of 500 ms chunks and import USE and importance ratings# 
####################################################################################
#build a vector representing every 500ms, in seconds, for the movie
common_time <- as.data.frame(seq(from = 0, to = 425, by = .5))
names(common_time) <- c("s") #rename column

#loop through common_time units to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match[i] <- match.closest(common_time$s[i], use.spline.interp$offset, tolerance = Inf, nomatch = 0000) #find matching offsets
  common_time$similarity[i] <- use.spline.interp$similarity[common_time$position_match[i]] #find corresponding similarity estimates 
  common_time$importance[i] <- conf.spline.interp$importance[common_time$position_match[i]] #find corresponding importance estimates 
  common_time$accuracy[i] <- acc.spline.interp$accuracy[common_time$position_match[i]] #find corresponding accuracy estimates 
}

#delete unnecessary variables
common_time <- subset(common_time, select = -c(position_match))
#create empty variables
common_time$orig_use <- 0
common_time$orig_conf <- 0
common_time$orig_acc <- 0
common_time$position_match2 <- 0

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match2[i] <- if(is.finite(min(use.data$offset[use.data$offset >= common_time$s[i]]))==FALSE){NA}else{min(use.data$offset[use.data$offset >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
  common_time$orig_use[i] <- if(is.finite(use.data[common_time$position_match2[i]==use.data$offset,]$similarity)==FALSE){NA}else{use.data[common_time$position_match2[i]==use.data$offset,]$similarity} #find corresponding similarity estimates 
  common_time$orig_conf[i] <- if(is.finite(importance.data[common_time$position_match2[i]==importance.data$offset,]$z_mean)==FALSE){NA}else{importance.data[common_time$position_match2[i]==importance.data$offset,]$z_mean} #find corresponding importance estimates 
  common_time$orig_acc[i] <- if(is.finite(accuracy.data[common_time$position_match2[i]==accuracy.data$offset,]$similarity)==FALSE){NA}else{accuracy.data[common_time$position_match2[i]==accuracy.data$offset,]$similarity} #find corresponding accuracy estimates 
}

#check warnings. Should equal the number of NA cells
#warnings()
length(warnings())
table(is.na(common_time))

#rename column
colnames(common_time)[which(names(common_time) == "position_match2")] <- "orig_onset"

#################################################################
#2. Load movie annotation data and import into common time-space# 
#################################################################
#load annotations
annot_movie <- read.csv("~/Dropbox/Academic/Projects/JHU/FilmFest/behaviour/FilmFest_Annotation_JL_Labels(1625).csv")
names(annot_movie)

#clean up
annot_movie <- subset(annot_movie, select = -c(X)) #remove unnecessary columns
#annot_movie <- head(annot_movie,tail(which(annot_movie$SEG.1000.boundary.strength!=0),1)) #remove blank rows
colnames(annot_movie)[which(names(annot_movie) == "Start.Time..m.ss.")] <- "SEG.216.Start.Time..m.ss."
colnames(annot_movie)[which(names(annot_movie) == "End.Time..m.ss.")] <- "SEG.216.End.Time..m.ss."

#populate empty columns (see Sathish's answer: https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell)
#SEG.12.title
while(length(ind <- which(annot_movie$SEG.12.title  == "")) > 0){
  annot_movie$SEG.12.title[ind] <- annot_movie$SEG.12.title[ind -1]
}
#SEG.216.number
while(length(ind <- which(is.na(annot_movie$SEG.216.number))) > 0){
  annot_movie$SEG.216.number[ind] <- annot_movie$SEG.216.number[ind -1]
}
#SEG.216.Start.Time..m.ss.
while(length(ind <- which(is.na(annot_movie$SEG.216.Start.Time..m.ss.))) > 0){
  annot_movie$SEG.216.Start.Time..m.ss.[ind] <- annot_movie$SEG.216.Start.Time..m.ss.[ind -1]
}
#SEG.216.End.Time..m.ss.
while(length(ind <- which(is.na(annot_movie$SEG.216.End.Time..m.ss.))) > 0){
  annot_movie$SEG.216.End.Time..m.ss.[ind] <- annot_movie$SEG.216.End.Time..m.ss.[ind -1]
}
#SEG.216.Description
while(length(ind <- which(annot_movie$SEG.216.Description  == "")) > 0){
  annot_movie$SEG.216.Description[ind] <- annot_movie$SEG.216.Description[ind -1]
}

#little function to isolate the decimals only from a number
reverse_truncate <- function(x) { sign(x) * (x - floor(x)) } 
#turn minute.seconds into total seconds
#for SEG.216
annot_movie$SEG.216.Start.S <- (floor(annot_movie$SEG.216.Start.Time..m.ss.)*60)+(reverse_truncate(annot_movie$SEG.216.Start.Time..m.ss.)*100)
annot_movie$SEG.216.End.S <- (floor(annot_movie$SEG.216.End.Time..m.ss.)*60)+(reverse_truncate(annot_movie$SEG.216.End.Time..m.ss.)*100)
#for SEG.1000
annot_movie$SEG.1000.Start.S <- (floor(annot_movie$SEG.1000.Start.Time..m.ss.)*60)+(reverse_truncate(annot_movie$SEG.1000.Start.Time..m.ss.)*100)
annot_movie$SEG.1000.End.S <- (floor(annot_movie$SEG.1000.End.Time..m.ss.)*60)+(reverse_truncate(annot_movie$SEG.1000.End.Time..m.ss.)*100)

#doesn't seem to be a SEG1000 label, so add arbitrary SEG1000 labels
annot_movie$SEG.1000.arbitrary.number <- as.numeric(row.names(annot_movie))

#isolate Bus Stop only and start timings at actual movie onsets
table(annot_movie$SEG.12.title)
annot_movie_clean <- subset(annot_movie, SEG.12.title %in% c("12. Bus Stop"))
#reset times to beginning of CMIYC
movie_start_time <- head(annot_movie_clean$SEG.216.Start.S ,1) #define movie start time
annot_movie_clean$SEG.216.Start.S <- annot_movie_clean$SEG.216.Start.S - (movie_start_time)
annot_movie_clean$SEG.216.End.S <- annot_movie_clean$SEG.216.End.S - (movie_start_time)
annot_movie_clean$SEG.1000.Start.S <- annot_movie_clean$SEG.1000.Start.S - (movie_start_time)
annot_movie_clean$SEG.1000.End.S <- annot_movie_clean$SEG.1000.End.S - (movie_start_time)

#take annotation data and add to common_times
names(annot_movie)
#create empty variables
common_time$SEG216_number <- 0
common_time$SEG1000_arbitrary_number <- 0
common_time$SEG1000_boundarystrength <- 0
common_time$SEG1000_valence <- 0
common_time$SEG1000_arousal <- 0
common_time$SEG1000_indoor_outdoor <- 0
common_time$SEG1000_location <- 0
common_time$SEG1000_onscreen_chars <- 0
common_time$SEG1000_present_chars <- 0
common_time$SEG1000_speaking_chars <- 0
common_time$SEG1000_music <- 0
common_time$SEG1000_onscreen_words <- 0
common_time$SEG216_description <- 0
common_time$SEG1000_description <- 0
common_time$position_match <- 0

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match[i] <- if(is.finite(min(annot_movie_clean$SEG.1000.End.S[annot_movie_clean$SEG.1000.End.S >= common_time$s[i]]))==FALSE){NA}else{min(annot_movie_clean$SEG.1000.End.S[annot_movie_clean$SEG.1000.End.S >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
  common_time$SEG1000_arbitrary_number[i] <- as.numeric(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.1000.arbitrary.number)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.1000.arbitrary.number}) #find corresponding (arbitrary) SEG1000 number 
  common_time$SEG216_number[i] <- as.numeric(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.216.number)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.216.number}) #find corresponding SEG216 number 
  common_time$SEG1000_description[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.1000.Description)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.1000.Description}) #find corresponding SEG1000 description 
  common_time$SEG216_description[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.216.Description)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$SEG.216.Description}) #find corresponding similarity SEG216 description 
  common_time$SEG1000_valence[i] <- as.numeric(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Valence)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Valence}) #find corresponding SEG1000 valence 
  common_time$SEG1000_arousal[i] <- as.numeric(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Arousal)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Arousal}) #find corresponding SEG1000 arousal 
  common_time$SEG1000_indoor_outdoor[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Space.In.Outdoor)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Space.In.Outdoor}) #find corresponding SEG1000 Space.In.Outdoor label
  common_time$SEG1000_location[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Location)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Location}) #find corresponding SEG1000 Location label
  common_time$SEG1000_onscreen_chars[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Onscreen)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Onscreen}) #find corresponding SEG1000 Name...Onscreen label
  common_time$SEG1000_present_chars[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Present)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Present}) #find corresponding SEG1000 Name...Present label
  common_time$SEG1000_speaking_chars[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Speaking)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Name...Speaking}) #find corresponding SEG1000 Name...Speaking label
  common_time$SEG1000_music[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Music.Presence)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Music.Presence}) #find corresponding SEG1000 Music.Presence label
  common_time$SEG1000_onscreen_words[i] <- as.character(if(is.finite(annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Words.on.Screen)==FALSE){NA}else{annot_movie_clean[common_time$position_match[i]==annot_movie_clean$SEG.1000.End.S,]$Words.on.Screen}) #find corresponding SEG1000 Words.on.Screen label
}

#add boundary strength ratings, from beginning of event, to specific boundary points
common_time$SEG1000_boundarystrength<- with(annot_movie_clean, SEG.1000.boundary.strength[match(common_time$s, SEG.1000.Start.S)])
common_time$SEG1000_boundarystrength[is.na(common_time$SEG1000_boundarystrength)] <- 0 #replace NAs with 0

#add segment start and end  markers
#empty variables
common_time$SEG216_start <- 0
common_time$SEG216_end <- 0
common_time$SEG1000_start <- 0
common_time$SEG1000_end <- 0
#populate variables
for (i in 1:length(common_time$s)){
  common_time$SEG216_start[i] <- if(length(common_time$SEG216_number[i-1]>0)){(if(common_time$SEG216_number[i]==common_time$SEG216_number[i-1]){0}else{1})}else{1}#add 1 to rows when a SEG216 event starts and add 0s everywhere else
  common_time$SEG216_end[i] <- if(is.na(common_time$SEG216_number[i+1])==FALSE){(if(common_time$SEG216_number[i]==common_time$SEG216_number[i+1]){0}else{1})}else{1}#add 1 to rows when a SEG216 event starts and add 0s everywhere else
  common_time$SEG1000_start[i] <- if(length(common_time$SEG1000_arbitrary_number[i-1]>0)){(if(common_time$SEG1000_arbitrary_number[i]==common_time$SEG1000_arbitrary_number[i-1]){0}else{1})}else{1}#add 1 to rows when a SEG1000 event starts and add 0s everywhere else
  common_time$SEG1000_end[i] <- if(is.na(common_time$SEG1000_arbitrary_number[i+1])==FALSE){(if(common_time$SEG1000_arbitrary_number[i]==common_time$SEG1000_arbitrary_number[i+1]){0}else{1})}else{1}#add 1 to rows when a SEG1000 event starts and add 0s everywhere else
}

#there will be tons of warnings. but look at the resulting common_time dataframe and it should be fine. 
#warnings()

##############################################################
#3. Load importance ratings and import into common time-space# 
##############################################################
#load annotations
importance_ff <- read.csv("~/Dropbox/Academic/Projects/JHU/FilmFest/behaviour/Hongmi_behav_data/FilmFest_Combined_Importance_Sep262019.csv")
names(importance_ff)

#clean up
importance_target_movie <- tail(importance_ff,36) #manually specify where the CMIYC data live in the dataframe
importance_target_movie <- subset(importance_target_movie, select = -c(End.Time..s.,End.Time..TRs..1.5s.,Start.Time..s.....concatenated,Start.Time..TRs..1.5s....concatenated,Start.Time..s.,Start.Time..TRs..1.5s.,scene.number...concatenated,End.Time..s.....concatenated,End.Time..TRs..1.5s....concatenated,Start.Time..m.ss.)) #remove unnecessary columns
colnames(importance_target_movie)[which(names(importance_target_movie) == "scene.start.time")] <- "SEG.216.Start.Time..m.ss."
colnames(importance_target_movie)[which(names(importance_target_movie) == "scene.end.time")] <- "SEG.216.End.Time..m.ss."
#turn times (e.g., 1:10) to decimals (e.g., 1.10)
importance_target_movie$SEG.216.Start.Time..m.ss. <- as.character(importance_target_movie$SEG.216.Start.Time..m.ss.)
importance_target_movie$SEG.216.Start.Time..m.ss. <- gsub(":", ".", importance_target_movie$SEG.216.Start.Time..m.ss.)
importance_target_movie$SEG.216.Start.Time..m.ss. <- as.numeric(importance_target_movie$SEG.216.Start.Time..m.ss.)
importance_target_movie$SEG.216.End.Time..m.ss. <- as.character(importance_target_movie$SEG.216.End.Time..m.ss.)
importance_target_movie$SEG.216.End.Time..m.ss. <- gsub(":", ".", importance_target_movie$SEG.216.End.Time..m.ss.)
importance_target_movie$SEG.216.End.Time..m.ss. <- as.numeric(importance_target_movie$SEG.216.End.Time..m.ss.)
names(importance_target_movie)

#turn minute.seconds into total seconds
#for SEG.216
importance_target_movie$SEG.216.Start.S <- (floor(importance_target_movie$SEG.216.Start.Time..m.ss.)*60)+(reverse_truncate(importance_target_movie$SEG.216.Start.Time..m.ss.)*100)
importance_target_movie$SEG.216.End.S <- (floor(importance_target_movie$SEG.216.End.Time..m.ss.)*60)+(reverse_truncate(importance_target_movie$SEG.216.End.Time..m.ss.)*100)
#reset times to beginning of target movie
movie_start_time==head(importance_target_movie$SEG.216.Start.S,1) #make sure movie_start_time matches across datasets
importance_target_movie$SEG.216.Start.S <- importance_target_movie$SEG.216.Start.S - (movie_start_time)
importance_target_movie$SEG.216.End.S <- importance_target_movie$SEG.216.End.S - (movie_start_time)

#scale all importance ratings within subject
importance_target_movie$Importance_Rater1_PostView <- scale(importance_target_movie$Importance_Rater1_PostView)
importance_target_movie$Importance_Rater2_PostView <- scale(importance_target_movie$Importance_Rater2_PostView)
importance_target_movie$Importance_Rater3_PostView <- scale(importance_target_movie$Importance_Rater3_PostView)
importance_target_movie$Importance_Rater4_PostView <- scale(importance_target_movie$Importance_Rater4_PostView)
importance_target_movie$Importance_Rater1_Online <- scale(importance_target_movie$Importance_Rater1_Online)
importance_target_movie$Importance_Rater2_Online <- scale(importance_target_movie$Importance_Rater2_Online)
importance_target_movie$Importance_Rater3_Online <- scale(importance_target_movie$Importance_Rater3_Online)
importance_target_movie$Importance_Rater4_Online <- scale(importance_target_movie$Importance_Rater4_Online)

#average importance ratings within condition (postview, online)
importance_target_movie$mean_importance_postview <- rowMeans(subset(importance_target_movie, select = c(Importance_Rater1_PostView,Importance_Rater2_PostView,Importance_Rater3_PostView,Importance_Rater4_PostView)), na.rm = TRUE)
importance_target_movie$mean_importance_online <- rowMeans(subset(importance_target_movie, select = c(Importance_Rater1_Online,Importance_Rater2_Online,Importance_Rater3_Online,Importance_Rater4_Online)), na.rm = TRUE)
#replace nan with na
importance_target_movie$mean_importance_postview[is.nan(importance_target_movie$mean_importance_postview)] <- NA
importance_target_movie$mean_importance_online[is.nan(importance_target_movie$mean_importance_online)] <- NA

#take importance data and add to common_times
#create empty variables
common_time$SEG216_importance_postview <- 0
common_time$SEG216_importance_online <- 0
common_time$position_match <- 0

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match[i] <- if(is.finite(min(importance_target_movie$SEG.216.End.S[importance_target_movie$SEG.216.End.S >= common_time$s[i]]))==FALSE){NA}else{min(importance_target_movie$SEG.216.End.S[importance_target_movie$SEG.216.End.S >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
  common_time$SEG216_importance_postview[i] <- as.numeric(if(is.finite(importance_target_movie[common_time$position_match[i]==importance_target_movie$SEG.216.End.S,]$mean_importance_postview)==FALSE){NA}else{importance_target_movie[common_time$position_match[i]==importance_target_movie$SEG.216.End.S,]$mean_importance_postview}) #find corresponding mean importance rating (postview)
  common_time$SEG216_importance_online[i] <- as.numeric(if(is.finite(importance_target_movie[common_time$position_match[i]==importance_target_movie$SEG.216.End.S,]$mean_importance_online)==FALSE){NA}else{importance_target_movie[common_time$position_match[i]==importance_target_movie$SEG.216.End.S,]$mean_importance_online}) #find corresponding mean importance rating (online)
}

#there will be tons of warnings. but look at the resulting common_time dataframe and it should be fine. 
#warnings()

##############################################################
#4. Load clustering ratings and import into common time-space# 
##############################################################
#load clustering
clustering_ff <- read.csv(paste0("~/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/",movie,"_human_rater_summary_EMH.csv"))
names(clustering_ff)

#take importance data and add to common_times
#create empty variables
common_time$cluster_count <- 0
common_time$cluster_mean <- 0
common_time$cluster_max <- 0
common_time$position_match <- 0

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match[i] <- if(is.finite(min(clustering_ff$offset[clustering_ff$offset >= common_time$s[i]]))==FALSE){NA}else{min(clustering_ff$offset[clustering_ff$offset >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
  common_time$cluster_count[i] <- as.numeric(if(is.finite(clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$count.cluster)==FALSE){NA}else{clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$count.cluster}) #find corresponding number of clusters
  common_time$cluster_mean[i] <- as.numeric(if(is.finite(clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$percent.mean)==FALSE){NA}else{clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$percent.mean}) #find corresponding mean cluster size
  common_time$cluster_max[i] <- as.numeric(if(is.finite(clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$percent.max)==FALSE){NA}else{clustering_ff[common_time$position_match[i]==clustering_ff$offset,]$percent.max}) #find corresponding max cluster size
}

#there will be tons of warnings. but look at the resulting common_time dataframe and it should be fine. 
#warnings()

#delete unnecessary variables
common_time <- subset(common_time, select = -c(position_match))

########################################################
#5. Load PMC activity and import into common time-space# 
########################################################
#delete temporary dataframes
rm(list=ls(pattern='*temp*'))

#load group averaged ROI timecourses
setwd(paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/roi_tc/tc"))
temp = list.files(pattern="*.txt")
tc_list = lapply(temp, function(x) read.delim(x, header = FALSE, sep = ","))
#load group averaged pISC ROI timecourses
setwd(paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/roi_tc/pisc"))
temp2 = list.files(pattern="*.txt")
pisc_list = lapply(temp2, function(x) read.delim(x, header = FALSE, sep = ","))

#add offset column per element in list
#for tc
for (i in 1:length(tc_list)){
  names(tc_list[[i]]) <- c("group mean") #rename column
  tc_list[[i]]$s <- as.numeric(rownames(tc_list[[i]]))*1.5
}

#for pisc
for (i in 1:length(pisc_list)){
  names(pisc_list[[i]]) <- c("group pISC") #rename column
  pisc_list[[i]]$s <- as.numeric(rownames(pisc_list[[i]]))*1.5
}

#load PMC data
mean_pmc_ff <- as.data.frame(tc_list[which(stri_sub(stri_sub(temp,12),1,-16)=="YEO400_bilateral_PMC")]) #looks for specified ROI label in tc_list
pisc_pmc_ff <- as.data.frame(pisc_list[which(stri_sub(stri_sub(temp,12),1,-16)=="YEO400_bilateral_PMC")]) #looks for specified ROI label in pisc_list
#merge into one file
pmc_ff <- merge(mean_pmc_ff, pisc_pmc_ff, by = 's')
#delete previous files
rm(mean_pmc_ff,pisc_pmc_ff)

#take PMC data and add to common_times
#create empty variables
common_time$pmc_mean <- 0
common_time$pmc_pisc <- 0
common_time$position_match <- 0

#loop through TRs to find corresponding offsets in the interpolated timecourse
for (i in 1:length(common_time$s)){
  common_time$position_match[i] <- if(is.finite(min(pmc_ff$s[pmc_ff$s >= common_time$s[i]]))==FALSE){NA}else{min(pmc_ff$s[pmc_ff$s >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
  common_time$pmc_mean[i] <- as.numeric(if(is.finite(pmc_ff[common_time$position_match[i]==pmc_ff$s,]$group.mean)==FALSE){NA}else{pmc_ff[common_time$position_match[i]==pmc_ff$s,]$group.mean}) #find corresponding number of clusters
  common_time$pmc_pisc[i] <- as.numeric(if(is.finite(pmc_ff[common_time$position_match[i]==pmc_ff$s,]$group.pISC)==FALSE){NA}else{pmc_ff[common_time$position_match[i]==pmc_ff$s,]$group.pISC}) #find corresponding mean cluster size
}

#there will be tons of warnings. but look at the resulting common_time dataframe and it should be fine. 
#warnings()

#delete unnecessary variables
common_time <- subset(common_time, select = -c(position_match))

####################################
#6. Load consensus event boundaries# 
####################################
#these data were summarized across 4 independent raters, by Yiyuan Zhang
#therefore, consensus boundary strenght is a ratio of 4
#if there was 4/4 consensus, boundary strength is 1
#if there is 2/4, boundary strength is .5, ...etc

#load boundaries
boundaries_ff <- read.csv(paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/FilmFest/behaviour/72_segment_event_boundaries/72_seg_data.v.2_50%_consensus.csv"))
names(boundaries_ff)

#clean up
unique(boundaries_ff$SEG.12.title)
boundaries_target <- subset(boundaries_ff, SEG.12.title=="12. Bus Stop") #specify where the data live in the dataframe
#reset times to when the target movie began
boundaries_target$True.Start.Time.S <- boundaries_target$Start.Time.S. - (movie_start_time+1493) #had to add 1493s to start time here, because Yiyuan's file considers the FilmFest parts 1 and 2 as appended
boundaries_target$True.End.Time.S <- boundaries_target$End.Time.S. - (movie_start_time+1493) #had to add 1493s to start time here, because Yiyuan's file considers the FilmFest parts 1 and 2 as appended

#add boundary strength ratings, from beginning of event, to specific boundary points
common_time$SEG216_consensus_boundarystrength<- with(boundaries_target, Level[match(common_time$s, True.Start.Time.S)])
common_time$SEG216_consensus_boundarystrength[is.na(common_time$SEG216_consensus_boundarystrength)] <- 0 #replace NAs with 0

#correct for slight differences in reported timing (for some reason, Yiyuan's boundaries don't match the SEG216 onset/offsets...why? don't know)
boundary.pos <- as.data.frame(which(common_time$SEG216_consensus_boundarystrength>0,arr.ind = T))
  colnames(boundary.pos) <- "pos"
common.pos <- as.data.frame(which(common_time$SEG216_end!=0,arr.ind = T))
  colnames(common.pos) <- "pos"
  
  library(fuzzyjoin) #library that has a fuzzy match function (difference_join)
  diff.pos <- difference_join(common.pos,boundary.pos,max_dist=6) #save output of fuzzy match function
  length(diff.pos$pos.y)==length(unique(diff.pos$pos.y)) #this should be true, if not, there are multiple joins for a given boundary
  diff.pos
  diff.pos  <- diff.pos[-4,] #manually find the duplicate and remove
  
  #loop through relevant values to update positions
  for (i in seq(1,length(diff.pos$pos.x))){
    if((diff.pos$pos.x[i]!=diff.pos$pos.y[i])){ #only run  if x and y don't already match
      common_time$SEG216_consensus_boundarystrength[diff.pos$pos.x[i]] <- common_time$SEG216_consensus_boundarystrength[diff.pos$pos.y[i]]
      common_time$SEG216_consensus_boundarystrength[diff.pos$pos.y[i]] <- 0
    }
  }

  #update boundary position
  boundary.pos <- as.data.frame(which(common_time$SEG216_consensus_boundarystrength>0,arr.ind = T))
    colnames(boundary.pos) <- "pos"
    boundary.pos==diff.pos$pos.x #should all be true!
    
#print output csv
write.table(common_time, file = paste0(description_path,movie,"_full_500ms_match.tsv"), row.names=FALSE, col.names=TRUE, sep="\t", quote = FALSE) #save prediction column only, without row or column labels, to csv

#export annotations in USE-compatible format
  #create additional directories
  annotation_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/predictions/annotations/")
  dir.create(annotation_path, showWarnings = FALSE)
  annotation_clean_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/predictions/annotations/clean/")
  dir.create(annotation_clean_path, showWarnings = FALSE)
  annotation_use_path <- paste0("/Users/bbellan1/Dropbox/Academic/Projects/JHU/online_experiments/prediction_probe/analysis_sandbox/",movie,"/predictions/annotations/use/")
  dir.create(annotation_use_path, showWarnings = FALSE)
  #define function to clean and export annotation data for universal sentence encoder
  export_clean_annotations <- function(df,annot_type){
    df <- data.frame(lapply(df, function(x) {
      gsub("/", " ", x)})) #replace / with space
    df <- data.frame(lapply(df, function(x) {
      gsub("\\\\", "", x)})) #replace \ with space
    df <- lapply(df, function(x) stringi::stri_trans_general(x, "latin-ascii")) #remove all non-ascii characters
    df <- data.frame(matrix(unlist(df), nrow=length(df), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
    #df <- as.data.frame(t(df)) #transpose
    write.table(df, file = paste0(annotation_clean_path,movie,"_",annot_type,"_annotations.txt"), row.names=FALSE, col.names=FALSE, sep=" ", quote = FALSE) #save prediction column only, without row or column labels, to csv
  }
  #export SEG216
  export_clean_annotations(unique(common_time$SEG216_description)[!is.na(unique(common_time$SEG216_description))],"SEG216")
  #export SEG1000
  export_clean_annotations(unique(common_time$SEG1000_description)[!is.na(unique(common_time$SEG1000_description))],"SEG1000")
  
  #################################
  #7. Event boundary analysis (V1)# 
  #################################
  #if boundary strength > boundary_thresh, select # of values, equal to window_size, before and after from DV timecourse of interest
  #essentially, this code allows you to look at whether any DV timecourse is affected by boundary position
  
  #define variables
  window_size <- 30 #window of datapoints either before or after boundary that we'll target
  boundary_thresh <- 0 #minimum strength of boundary required to be included in analysis
  dependent_var <- which(names(common_time)=="similarity") #change string to DV of interest. for list of DVs, try: names(common_time)
  #make empty dataframes
  i=window_size+1 #define i based on window size
  full.pre <- as.data.frame(t(common_time[,dependent_var][seq(if((i-window_size)<0){1}else{i-window_size},i-1,1)]*0))
  full.post <- as.data.frame(t(common_time[,dependent_var][seq(i+1,if((i+window_size)>length(common_time$SEG216_consensus_boundarystrength)){common_time$SEG216_consensus_boundarystrength}else{i+window_size},1)]*0))
  
  #loop through dataframe looking for boundaries
  for (i in boundary.pos$pos){
    temp.pre <- as.data.frame(t(common_time[,dependent_var][seq(if((i-window_size)<0){1}else{i-window_size},if(i-1>1){i-1}else{1},1)])) #create temp df with all dependent_var values from window_size units of time before
    temp.post <- as.data.frame(t(common_time[,dependent_var][seq(if(i+1>length(common_time$SEG216_consensus_boundarystrength)){i}else{i+1},if((i+window_size)>length(common_time$SEG216_consensus_boundarystrength)){common_time$SEG216_consensus_boundarystrength}else{i+window_size},1)])) #create temp df with all dependent_var values from window_size units of time after
    full.pre <- rbind.fill(full.pre,temp.pre)
    full.post <- rbind.fill(full.post,temp.post)
  }
  
  #remove first row
  full.pre <- tail(full.pre,nrow(full.pre)-1)
  full.post <- tail(full.post,nrow(full.post)-1)
  
  #sanity check: number of rows in full.pre and full.post should equal number of boundaries
  length(na.omit(common_time$SEG216_consensus_boundarystrength[common_time$SEG216_consensus_boundarystrength>boundary_thresh]))==nrow(full.pre)
  length(na.omit(common_time$SEG216_consensus_boundarystrength[common_time$SEG216_consensus_boundarystrength>boundary_thresh]))==nrow(full.post)
  
  #calculate mean USE similarity for pre and  post-boundary
  mean.pre <- as.data.frame(cbind(apply(full.pre,1, mean, na.rm = TRUE),apply(full.pre,1, sd, na.rm = TRUE)))
  colnames(mean.pre) <- c("mean","sd")
  mean.pre$cond <- "pre-boundary"
  mean.pre$index <- seq(1,length(mean.pre$cond),1)
  mean.post <- as.data.frame(cbind(apply(full.post,1, mean, na.rm = TRUE),apply(full.post,1, sd, na.rm = TRUE)))
  colnames(mean.post) <- c("mean","sd")
  mean.post$cond <- "post-boundary"
  mean.post$index <- seq(1,length(mean.post$cond),1)
  
  #test hypothesis: pre-boundary > post-boundary prediction consensus
  table(mean.pre$mean>mean.post$mean)
  t.test(mean.pre$mean,mean.post$mean, paired=TRUE, alternative = "two.sided")
  
  #calculate means and prepare df for plotting
  mean.pre.long <- melt(mean.pre, id.vars = c("index","cond"))
  mean.post.long <- melt(mean.post, id.vars = c("index","cond"))
  boundary.df <- rbind(mean.pre.long,mean.post.long)
  
  #plot prediction consensus for each boundary
  ggplot(subset(boundary.df, variable=="mean"),aes(x=index, y=value))+
    geom_line(aes(group=index), size=1) +
    geom_point(aes(colour=cond),size=3) + 
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("boundary index") + ylab("cosine similarity") + ylim(.2, .7)
  #xlab("boundary index") + ylab("cluster count") + ylim(0, 20)
  #xlab("boundary index") + ylab("PMC pISC") + ylim(0,.5)
  #xlab("boundary index") + ylab("PMC mean") + ylim(-1,1)
  
  
  #################################
  #8. Event boundary analysis (V2)# 
  #################################
  #Use SEG216 and SEG1000 as event boundaries
  
  #define variables
  window_size <- 10 #window of datapoints either before or after boundary that we'll target
  boundary_thresh <- 0 #minimum strength of boundary required to be included in analysis
  dependent_var <- which(names(common_time)=="similarity") #change string to DV of interest. for list of DVs, try: names(common_time)
  #make empty dataframes
  i=window_size+1 #define i based on window size
  full.pre <- as.data.frame(t(common_time[,dependent_var][seq(if((i-window_size)<0){1}else{i-window_size},i-1,1)]*0))
  full.post <- as.data.frame(t(common_time[,dependent_var][seq(i+1,if((i+window_size)>length(common_time$SEG216_consensus_boundarystrength)){common_time$SEG216_consensus_boundarystrength}else{i+window_size},1)]*0))
  
  #loop through dataframe looking for boundaries
  for (i in which(common_time$SEG1000_end>0)){ #change SEG1000 to SEG216 if you want to see that instead
    temp.pre <- as.data.frame(t(common_time[,dependent_var][seq(if((i-window_size)<0){1}else{i-window_size},if(i-1>1){i-1}else{1},1)])) #create temp df with all dependent_var values from window_size units of time before
    temp.post <- as.data.frame(t(common_time[,dependent_var][seq(if(i+1>length(common_time$SEG216_consensus_boundarystrength)){i}else{i+1},if((i+window_size)>length(common_time$SEG216_consensus_boundarystrength)){length(common_time$SEG216_consensus_boundarystrength)}else{i+window_size},1)])) #create temp df with all dependent_var values from window_size units of time after
    full.pre <- rbind.fill(full.pre,temp.pre)
    full.post <- rbind.fill(full.post,temp.post)
  }
  
  #remove first row
  full.pre <- tail(full.pre,nrow(full.pre)-1)
  full.post <- tail(full.post,nrow(full.post)-1)
  
  #sanity check: number of rows in full.pre and full.post should equal number of boundaries
  length(na.omit(unique(common_time$SEG1000_arbitrary_number)))==nrow(full.pre)
  length(na.omit(unique(common_time$SEG1000_arbitrary_number)))==nrow(full.post)
  
  #calculate mean USE similarity for pre and  post-boundary
  mean.pre <- as.data.frame(cbind(apply(full.pre,1, mean, na.rm = TRUE),apply(full.pre,1, sd, na.rm = TRUE)))
  colnames(mean.pre) <- c("mean","sd")
  mean.pre$cond <- "pre-boundary"
  mean.pre$index <- seq(1,length(mean.pre$cond),1)
  mean.post <- as.data.frame(cbind(apply(full.post,1, mean, na.rm = TRUE),apply(full.post,1, sd, na.rm = TRUE)))
  colnames(mean.post) <- c("mean","sd")
  mean.post$cond <- "post-boundary"
  mean.post$index <- seq(1,length(mean.post$cond),1)
  
  #test hypothesis: pre-boundary > post-boundary prediction consensus
  table(mean.pre$mean>mean.post$mean)
  t.test(mean.pre$mean,mean.post$mean, paired=TRUE, alternative = "two.sided")
  
  #calculate means and prepare df for plotting
  mean.pre.long <- melt(mean.pre, id.vars = c("index","cond"))
  mean.post.long <- melt(mean.post, id.vars = c("index","cond"))
  boundary.df <- rbind(mean.pre.long,mean.post.long)
  
  #plot prediction consensus for each boundary
  ggplot(subset(boundary.df, variable=="mean"),aes(x=index, y=value))+
    geom_line(aes(group=index), size=1) +
    geom_point(aes(colour=cond),size=3) + 
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("boundary index") + ylab("cosine similarity") + ylim(.2, .7)
  
  ####################################################################################
  #plot timecourses of USE similarity WITH boundaries and boundary strength overlayed#
  ####################################################################################
  #select boundary threshold (like boundary_thresh) but for these figures
  fig_boundary_thresh <- 0
  
  #plot USE timecourse with upsampling
  ggplot(data=tr, aes(x=s, y=similarity)) +
    geom_vline(data=common_time, xintercept=common_time$s[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh], linetype="solid", alpha=(common_time$SEG216_consensus_boundarystrength[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh])) + 
    geom_line(size=1) + 
    geom_point(size=2) + 
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7)
  ggsave(paste0(figure_path,movie,"_ff","_use_upsampled_timecourse+boundaries.tiff"), dpi = 300, scale = 2:1)
  
  #plot USE without upsampling, but instead, dragged (in reverse) to the same temporal resolution
  ggplot(data=common_time, aes(x=s, y=orig_use)) +
    geom_vline(xintercept=common_time$s[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh], linetype="solid", alpha=(common_time$SEG216_consensus_boundarystrength[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh])) + 
    geom_errorbar(data=use.data,aes(x=offset, y=similarity, ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
    geom_line(size=1) + 
    geom_point(data=use.data, aes(x=offset, y=similarity, colour=counterbalance),size=2) +
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7)
  ggsave(paste0(figure_path,movie,"_ff","_use_drag_timecourse+boundaries.tiff"), dpi = 300, scale = 2:1)
  
  #without upsampling, but at a lower temporal resolution
  ggplot(data=use.data, aes(x=offset, y=similarity)) +
    geom_vline(data=common_time, xintercept=common_time$s[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh], linetype="solid", alpha=(common_time$SEG216_consensus_boundarystrength[common_time$SEG216_consensus_boundarystrength>fig_boundary_thresh])) + 
    geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
    geom_line(size=1) + 
    geom_point(aes(colour=counterbalance),size=2) +
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7) 
  ggsave(paste0(figure_path,movie,"_ff","_use_orig_timecourse+boundaries.tiff"), dpi = 300, scale = 2:1)
  
  #without upsampling, but at a lower temporal resolution
  ggplot(data=use.data, aes(x=offset, y=similarity)) +
    geom_vline(xintercept=common_time$s[common_time$SEG216_end>fig_boundary_thresh], linetype="solid") + 
    geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
    geom_line(size=1) + 
    geom_point(aes(colour=counterbalance),size=2) +
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7) 
  ggsave(paste0(figure_path,movie,"_ff","_use_orig_timecourse+SEG216boundaries.tiff"), dpi = 300, scale = 2:1)
  
  #separated per counterbalance (i.e., group of subjects)
  ggplot(data=use.data, aes(x=offset, y=similarity)) +
    facet_wrap(~ counterbalance) + 
    geom_errorbar(aes(ymin=similarity-se, ymax=similarity+se), width=.1, linetype=5, alpha=.4) +
    geom_line(size=1) + 
    geom_point(aes(colour=counterbalance),size=2) +
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    geom_vline(xintercept=common_time$s[common_time$SEG216_end>fig_boundary_thresh], linetype="solid") + 
    xlab("time (s)") + ylab("cosine similarity") + ylim(.2, .7) 
  
  ###################################################################
  #Calculate similarity at the event-level + add recall probability #
  ###################################################################
  
  ##looking at the counterbalance plot above, I thought it might be useful to scale each group of participants by their group mean
  #z-score consensus within counterbalance
  use.data <- use.data %>% filter(counterbalance %in% unique(counterbalance)) %>% group_by(counterbalance) %>%
    mutate(z=scale(similarity)) 
  
  #same as above, but for SEG216 (and using z-scored consensus per counterbalance)
  ggplot(data=use.data, aes(x=offset, y=z)) +
    geom_line(size=1) + 
    geom_point(aes(colour=counterbalance),size=2) +
    theme(text=element_text(size=18),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 1), axis.line.y = element_line(colour="black", size = 1)) +
    geom_vline(xintercept=common_time$s[common_time$SEG216_end>fig_boundary_thresh], linetype="solid") + 
    xlab("time (s)") + ylab("cosine similarity (z)")
  ggsave(paste0(figure_path,movie,"_ff","_zuse_orig_timecourse+SEG216boundaries.tiff"), dpi = 300, scale = 2:1)
  
  #calculate a pos/neutral/neg version of the z-scored consensus (maybe the measure is noisy, but we can discriminate between predictable/neutral/unpredictable scenes)
  use.data$z_discrete <- NA
  use.data$z_discrete [use.data$z<quantile(use.data$z, na.rm=TRUE)[2]] <- -1
  use.data$z_discrete [use.data$z>quantile(use.data$z, na.rm=TRUE)[4]] <- 1
  use.data$z_discrete [use.data$z<quantile(use.data$z, na.rm=TRUE)[4] & use.data$z>quantile(use.data$z, na.rm=TRUE)[2]] <- 0
  
  #upsample z_use
  #spline for use
  use.spline.interp.z <- spline(use.data$offset, use.data$z, method="natural", n=100000, xmin=0, xmax=425)
  use.spline.interp.z <- data.frame(matrix(unlist(use.spline.interp.z), nrow=length(use.spline.interp.z), byrow=TRUE),stringsAsFactors=FALSE) #turn list to data.frame
  use.spline.interp.z <- data.frame(matrix(unlist(use.spline.interp.z), nrow=length(use.spline.interp.z), byrow=TRUE),stringsAsFactors=FALSE) #go long from wide
  names(use.spline.interp.z) <- c("offset", "z_up") #rename column
  
  #(for upsampled z_use) loop through TRs to find corresponding offsets in the interpolated timecourse
  for (i in 1:length(common_time$s)){
    common_time$position_match[i] <- if(is.finite(min(use.spline.interp.z$offset[use.spline.interp.z$offset >= common_time$s[i]]))==FALSE){NA}else{min(use.spline.interp.z$offset[use.spline.interp.z$offset >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
    common_time$z_up[i] <- as.numeric(if(is.finite(use.spline.interp.z[common_time$position_match[i]==use.spline.interp.z$offset,]$z_up)==FALSE){NA}else{use.spline.interp.z[common_time$position_match[i]==use.spline.interp.z$offset,]$z_up}) #find corresponding z
  }
  
  #(for z_use and z_discrete) loop through TRs to find corresponding offsets in the interpolated timecourse
  for (i in 1:length(common_time$s)){
    common_time$position_match[i] <- if(is.finite(min(use.data$offset[use.data$offset >= common_time$s[i]]))==FALSE){NA}else{min(use.data$offset[use.data$offset >= common_time$s[i]])} #find closest offset in use.data that is greater than or equal to common_time$s[i]
    common_time$z_use[i] <- as.numeric(if(is.finite(use.data[common_time$position_match[i]==use.data$offset,]$z)==FALSE){NA}else{use.data[common_time$position_match[i]==use.data$offset,]$z}) #find corresponding z
    common_time$z_discrete[i] <- as.numeric(if(is.finite(use.data[common_time$position_match[i]==use.data$offset,]$z_discrete)==FALSE){NA}else{use.data[common_time$position_match[i]==use.data$offset,]$z_discrete}) #find corresponding z_discrete
  }
  
  #average by SEG216-scene label
  common_event <- ddply(common_time, ~SEG216_number, summarise, consensus_up = mean(similarity, na.rm = TRUE), importance_up = mean(importance, na.rm = TRUE), accuracy_up = mean(accuracy, na.rm = TRUE), zconsensus_up = mean(z_up, na.rm = TRUE), consensus_orig = mean(orig_use, na.rm = TRUE), importance_orig = mean(orig_conf, na.rm = TRUE), accuracy_orig = mean(orig_acc, na.rm = TRUE), zconsensus_orig = mean(z_use, na.rm = TRUE))
  
  #calculate a pos/neutral/neg version of the z-scored consensus (maybe the measure is noisy, but we can discriminate between predictable/neutral/unpredictable scenes)
  common_event$z_discrete <- NA
  common_event$z_discrete [common_event$zconsensus_orig<quantile(common_event$zconsensus_orig, na.rm=TRUE)[2]] <- -1
  common_event$z_discrete [common_event$zconsensus_orig>quantile(common_event$zconsensus_orig, na.rm=TRUE)[4]] <- 1
  common_event$z_discrete [common_event$zconsensus_orig<quantile(common_event$zconsensus_orig, na.rm=TRUE)[4] & common_event$zconsensus_orig>quantile(common_event$zconsensus_orig, na.rm=TRUE)[2]] <- 0
  
  #load recall data
  ff_recall <- read.csv("~/Dropbox/Academic/Projects/JHU/FilmFest/behaviour/Hongmi_behav_data/Filmfest_recall_probability.csv", header = TRUE, comment.char="#")
  
  #select target movie data only
  ff_recall <- subset(ff_recall, movie_label=="12. Bus Stop")
  
  #add recall probability to common_event
  common_event$ff_recall_probability <- NA
  common_event$ff_recall_probability <- with(ff_recall, recall_probability[match(common_event$SEG216_number, SEG216_scene_v2)])
  
  #add importance to common_event
  common_event$ff_importance_postview <- NA
  common_event$ff_importance_online <- NA
  common_event$ff_importance_postview <- with(importance_target_movie, mean_importance_postview[match(common_event$SEG216_number, scene.number)])
  common_event$ff_importance_online <- with(importance_target_movie, mean_importance_online[match(common_event$SEG216_number, scene.number)])
  common_event$ff_importance_combined <- rowMeans(common_event[,c('ff_importance_postview', 'ff_importance_online')])
  
  #add event-level recall to common_time data
  common_time$SEG216_ff_recall_probability <- with(common_event, ff_recall_probability[match(common_time$SEG216_number, SEG216_number)])
  
  #delete unnecessary variables
  common_time <- subset(common_time, select = -c(position_match))
  
  #save to file
  write.csv(common_event, file = paste0(description_path,movie,"_ff_event_match.csv"), row.names=FALSE) #save dataframe to csv
  write.table(common_time, file = paste0(description_path,movie,"_full_500ms_match.tsv"), row.names=FALSE, col.names=TRUE, sep="\t", quote = FALSE) #save prediction column only, without row or column labels, to csv
  
  #quick correlation
  cor.test(common_event$consensus_orig, common_event$ff_recall_probability)  
  plot(common_event$consensus_orig,common_event$ff_recall_probability)
  
  