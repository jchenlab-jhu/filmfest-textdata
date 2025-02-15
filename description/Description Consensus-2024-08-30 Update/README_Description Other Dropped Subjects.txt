Description Consensus Other Dropped Participants

This README serves to document participants that were dropped from the Description Consensus for unusual errors in their experiment and data entry.

Below is a full list of the IDs involved and were dropped as a result (already excluded IDs are not listed). This list does not include participants who were dropped based off of comprehension questions, audio catch data validation, and looping issues (please see Description Exp Repeat IDs README for the last category).

- "debugLqFhi:debugGvqsr" (CMIYC)
- "debug0B9m8:debug3FUu6" (The Boyfriend)
- "debugqafnv:debugdwkh7" (Keith Reynolds)
- "debugwSh8w:debugwpzZp" (CMIYC)
- "debugJzmMB:debugGzFsZ" (CMIYC)
- "debugValB4:debugIp2gE" (CMIYC)
- "debugdmrft:debugrrSdu" (CMIYC)


Further details about each error can be seen below.


***********************************************************************************************************
Additional context on the format of the experiment

During the experiment phase, participants are shown video clips 0 ~ n of the select movie. Typically, clip 0 is played for participants just to watch. Clip 1 then plays and the experiment prompts participants to describe what happened in clip 1 afterwards (the first description stop). The clip after this description stop (clip 2), once again, is only for viewing purposes. Thus, description stops should only happen on odd number clips (clip 1, 3, 5, etc).

All participants recorded on this list thus far, have experienced some type of abnormality related to the clips which are being described during description stops

***********************************************************************************************************
Wrong description stops due to missing clip 1

For participant ID 1191 (sessionID "debugLqFhi:debugGvqsr"), the CMIYC experiment skipped over clip 1, thus prompting the participant to describe clip 2 as the first description stop. As a result, instead of clips 1,3,5,etc (the intended description stops), the participant ended up describing clips 2,4,6,etc. Since this participant described the wrong timeframes in this counterbalance, they are to be dropped from the dataset.

The entire dataset was checked for similar occurrences. and none were found.


Affected participant details:
	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debugLqFhi:debugGvqsr"
	- movie: CMIYC
	- trialID (ID in data table): 1191
	- counterbalance: 1 

***********************************************************************************************************
Wrong description stops due to clip 0 repeating twice

For participants in this section, the experiment played clip 0 twice, thus prompting participants to describe clip 0 as the first description stop. As a result, instead of clips 1,3,5,etc (the intended description stops), the participant ended up describing clips 0,2,4,etc. Since participants described the wrong timeframes in this counterbalance, they are to be dropped from the dataset.

The entire dataset was checked for similar occurrences, and the following participants were identified and dropped:

	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debug0B9m8:debug3FUu6"
	- movie: The Boyfriend
	- trialID (ID in data table): 1088
	- counterbalance: 4

	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debugqafnv:debugdwkh7"
	- movie: Keith Reynolds
	- trialID (ID in data table): 1247
	- counterbalance: 5

	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debuga9k3G:debugDSmuE"
	- movie: The Rock
	- counterbalance: 8
	- was already dropped from dataset due to looping issue, thus not listed in the first 	section



***********************************************************************************************************
Repeat participants in CMIYC_long

CMIYC_long was hosted on two different Heroku apps and thus two different Prolific experiments due to space limitations. Because of this, a few participants were able to participate in the two different versions of the CMIYC_long experiment. The IDs generated from the second experiment of these participants are being dropped as these participants have already seen the exact movie segments of that experiment from there first run.

	- "debugwSh8w:debugwpzZp"
	- "debugJzmMB:debugGzFsZ"
	- "debugValB4:debugIp2gE"
	- "debugdmrft:debugrrSdu"


***********************************************************************************************************
Wrong description stops due to continuously repeating clips

For participants in this section, the experiment skipped clip 0 and played each clip twice. The participants ended up describing clips 2,3,4,etc instead of 1,3,5,etc (the intended description stops). Since participants described the wrong timeframes in this counterbalance, they are to be dropped from the dataset.

The entire dataset was checked for similar occurrences, and the following participants were identified and found to have already been dropped for other reasons (comprehension questions and audio catch validation) and thus not listed in the first section:

	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debugIJ6ok:debugZI6kw"
	- movie: Bus Stop
	- counterbalance: 4

	- sessionID (ID in raw "trialdata.csv" raw psiturk file): "debugFVMVB:debugXdtFZ"
	- movie: The Shoe
	- counterbalance: 3