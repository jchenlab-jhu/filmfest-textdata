This file was written to describe what the variables in each column of the "[movie_name]_test_full.csv" files (found in ../Descriptions/[movie_name]/raw) are.

ID: subject ID assigned to each participant within each experiment

Trial: starts at 0 when web experiment begins, including the consent page, and increases as experiment proceeds (similar to time)
	Used to tell what data/trials came after one another

Timestamp: exact date + time that the event happened, represented in Unix timestamp

Datastring: can be ignored (not its own variable), conglomerate string of all the columns that is fed into the data analysis file, which then parses it into the columns shown

Counterbalance: the version of the experiment each participant was assigned
	Each version/counterbalance stops the movie at 	different time points for descriptions.

Phase: either "practice" phase or "test" phase
	Practice = instruction phase using short example video (only occurs at the beginning)
	Test = actual experiment using movie clips

Test_trial: indicates whether the event recorded was a "viewing" one (where the participant simply watches the movie clip) or a "description" one (where the participant describes the movie clip after watching it)
	All the ones listed in this data file will be "description"

Vidpath: file location of the movie clip being played

Description stop: the number of time the participant has been stopped to describe the movie
	i.e. when the participant is stopped for the 1st time to describe the movie, that is description stop 1
	When the participant is stopped for the 5th time to describe the movie, that is description stop 5

Paired_vid: within the file folder where all the movie clips are being stored (vidpath), the movie clip that was played before the participant was asked to describe

Offset: the time, in seconds (s), from the beginning of the movie, at which the movie clip ended and the participant was asked to describe

Description content: the actual text transcript of the participant's input when asked to describe the last 30s of the movie clip

Importance: rating of how important the participant thought the events in their description were to the entire movie

Time: how long it took for people to write their descriptions, in milliseconds (ms)

Tag: short notation of the details of each description
	1st part = phase
	2nd part = video segment
	3rd part = always "desc", simply meaning "description"
	i.e. test_seg2_desc = this description was given during the test (actual experiment) phase and the participant described video segment/movie clip 2







