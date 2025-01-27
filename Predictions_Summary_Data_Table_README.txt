This file was written to describe what the variables in each column of the "[movie_name]_test_full.csv" files (found in ../Predictions/[movie_name]/raw/) are.

ID: subject ID assigned to each participant within each experiment

Trial: starts at 0 when web experiment begins, including the consent page, and increases as experiment proceeds (similar to time)
	Used to tell what data/trials came after one another

Timestamp: exact date + time that the event happened, represented in Unix timestamp

Counterbalance: the version of the experiment each participant was assigned
	Each version/counterbalance stops the movie at 	different time points for predictions.

Phase: either "practice" phase or "test" phase
	Practice = instruction phase using short example video (only occurs at the beginning)
	Test = actual experiment using movie clips

Test_trial: indicates whether the event recorded was a "viewing" one (where the participant simply watches the movie clip) or a "prediction" one (where the participant predicts the movie clip after watching it)
	All the ones listed in this data file will be "prediction"

Confidence: rating of how confident the participant was in the prediction they made

Prediction_content: the actual text transcript of the participant's input when asked to predict the next 30s of the movie clip

Offset: the time, in seconds (s), from the beginning of the movie, at which the movie clip ended and the participant was asked to predict what happens next

Prediction_number: indicates the nth prediction that a participant makes for a particular stop (participants are allowed to enter multiple predictions at one stop)
	i.e. when participant writes their 1st prediction at a particular stop, prediction_number = 1
	When they make another prediction for the same stop, prediction_number = 2
	This number resets to 1 for every stop (so the 1st prediction of the next stop is prediction_number = 1 not prediction_number = 3)

Video_segment: indicates which video within the file folder, in which all the movie clips are stored (vidpath), is played before the participant was asked to predict

Time: how long it took for people to write their descriptions, in milliseconds (ms)


Vidpath: file location of the movie clip being played

Tag: short notation of the details of each description
	1st part = phase
	2nd part = video segment
	3rd part = always "pred", simply meaning "prediction"
	i.e. test_seg2_pred = this prediction was given during the test (actual experiment) phase and the participant described video segment/movie clip 2







