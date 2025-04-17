% July 17th, 2023
% Yoonjung Lee
% previously, the retrospective event boundaries were identified "within"
% each movie.
% now, we're going to identify peak boundaries by thresholdidng the
% agreement data collapsed across all of the movies

% notes: two versions (smoothed and then z-scored before data concatenation vs. only smoothed and
% then concatenated)

% July 28th, 2023
% we decided not to z-score the agreement data
% we'll just concatenate the agreement data across movies

% YJLee 20231025
% data check for manuscript

% YJLee 20231201
% data check for manuscript & PBS talk
% excluding title onsets

% we'll stick with including title onsets data, but later we'll drop it. 

%% set-up

clear all; close all; clc;

wd = '/Volumes/yjlee_ssd/projects/cortical_alignment/data_filmfest';
%options = 'data_excluding_onsets_titles'; % two sub-folders: 1_'data_including_onsets_titles' or 2_'data_excluding_onsets_titles' % edited by YJ, 20221024
options = 'data_including_onsets_titles'; % YJLee, 20230725

% dir_bound = fullfile(wd, 'retrospective_boundaries', options);
dir_bound = fullfile(wd, 'retrospective_boundaries');

% --- movies
movie_list = {'1_catch_me_if_you_can', '2_the_record', '3_the_boyfriend', '4_the_shoe', ...
    '5_keith_reynolds', '6_the_rock', '7_the_prisoner', '8_the_black_hole', ...
    '9_post_it_love', '10_bus_stop'};

titles = {'Catch Me If You Can', 'The Record', 'The Boyfriend', 'The Shoe', ...
    'Kieth Reynolds', 'The Rock', 'The Prisoner', 'The Black Hole', ...
    'Post It Love', 'Bus Stop'};

% duration in sec; including title scene
dur_sec = [350 138 470 135 359 329 266 147 165 425]; % YJLee 20231025; based on the Filmfest movie event annotation file (for example, "Filfest_Annotation_JL_spellchecked.xlsx")
% below, duration in the 1/10th sec; 100ms
dur_100ms = dur_sec*10;
off_100ms = cumsum(dur_100ms); % a total of 27840 timepoints; 10 movies; in the 1/10th sec resolution
onsets_100ms = off_100ms+1;
ons_100ms = [1 onsets_100ms(1:end-1)];
% below, movie_100ms; individual movie file onset & offset; 10-by-2
movie_100ms(:,1) = ons_100ms';
movie_100ms(:,2) = off_100ms';

% below; timestamps after considering lobby 1 & lobby 2; within each run
% individual movie file onsets in "sec.";
monsets = [41 391 529 999 1134 41 370 636 783 948]; % movies 1-5 from run #1, movies 6-10 from run #2; YJLee 20231025; also, based on the annotation file
% endtimes_tr = [995 915]; % end time of each movie-viewing run (in TR)
% endtimes_sec = [1492 1372]; % end time of each movie-viewing run (in sec)

TR = 1.5; % 1 TR = 1.5 sec
numRun = 2;
% movie1tr = 993; % movie-viewing run1: total 993 TRs (freesurfer pipeline data)
% movie2tr = 914; % movie-viewing run2: total 914 TRs (freesurfer pipeline data)

% parameters for peak boundary detection
w = 10; % e.g., if w=5, +- 500 ms (+- 5 1/10th-sec time points; +- 0.5 sec time points) on either side of the timestamp
% sm_ = 20; % 2 seconds; a variable that holds the value 20; for data smoothing; fitting Gaussian curve
perc = 65;
% ptag = sprintf('perc%02d', perc);

% let's first check the previously calculated smoothed agreement timecourse data for
% each movie; the agreement data in the 1/10th sec resolution
% how many timepoints in the timecourse data? might need to crop several
% timepoints to match the number of timepoints from the original filmfest
% simuli used for fMRI data collection

clear ebounds
load(fullfile(dir_bound, sprintf('filmfest_retrospective_boundaries_peaks_w%d_%s', w, options)), 'ebounds');
% load(fullfile(dir_bound, sprintf('filmfest_retrospective_boundaries_peaks_w%d_%s_20231023', w, options)), 'ebounds'); % YJLee 20231025

agreeData = {};
for xmovie=1:length(movie_list)
    clear mtag xtimecourse
    mtag = sprintf('movie%02d', xmovie);
    xtimecourse = ebounds.(mtag).dsmoothed;
    fprintf('*** the number of timepoints in %s is %d.\n', titles{xmovie}, length(xtimecourse));
    if length(xtimecourse) > dur_100ms(xmovie)
        fprintf('... cropping %d timepoints at the end of the data... now %d timepoints\n', length(xtimecourse) - dur_100ms(xmovie), dur_100ms(xmovie));
        agreeData{xmovie} = xtimecourse(1:dur_100ms(xmovie));
    elseif length(xtimecourse) == dur_100ms(xmovie)
        agreeData{xmovie} = xtimecourse;
    elseif length(xtimecourse) < dur_100ms(xmovie)
        fprintf('... padding %d timepoints at the end of the data... now %d timespoints\n', dur_100ms(xmovie) - length(xtimecourse), dur_100ms(xmovie));
        agreeData{xmovie} = [xtimecourse zeros(1, (dur_100ms(xmovie) - length(xtimecourse)))];
    end
end

% 40 timepoints were cropped at the end of Catch Me If You Can
% no cropping/padding for The Record
% 40 timepoints were cropped at the end of The Boyfriend
% 40 timepoints were cropped at the end of The Shoe
% 50 timepoints were padded at the end of Keith Reynolds
% 20 timepoints were padded at the end of The Rock
% 20 timepoints were cropped at the end of The Prisoner
% 30 timepoints were cropped at the end of The Black Hole
% 40 timepoints were cropped at the end of Post-it Love
% 30 timepoints were cropped at the end of Bus Stop

%% data concatenation

% 1. we'll concatenate inidividual movie's agreement timecourse data
% 2. then, threshold the concatenated timecourse data (e.g., the 65th
% percentile)
% 3. divide the boundaries into three bins (low-mid-high)
% 4. since TRs from lobby 1 & lobby 2 should be considered, we'll align the
% timestamps again; in TR

% --- 1. data concatenation
concatD = [];
for m=1:length(movie_list)
    clear Mdata
    Mdata = agreeData{m};
    concatD = [concatD Mdata];
end

%% thresholding

numBins = 3; % three event boundary conditions; weak-mid-strong

clear Data fileID
Data = concatD; % 1-by-27840; in the 1/10th second resolution
fileID = 'smoothed';
concatRetro.timecourse = Data;

% --- 2. threshold the concatenated agreement data & divide the boundaries into
% three bins (i.e., three event boundary conditions; weak-mid-strong)

clear thresh percentile
thresh = perc;
percentile = prctile(Data, thresh);
concatRetro.thresh = thresh;
concatRetro.percentile = percentile;

clear extract_events etimes peaks
extract_events = bwlabeln(Data > percentile); % bwlabeln is a function that labels connected blocks of ones. i.e. 1 1 1 1 0 0 0 1 1 0 1 1  becomes 1 1 1 1 0 0 0 2 2 0 3 3
% ^ extract_events: 1-by-27840
etimes = zeros(max(extract_events),1); % # of boundaries identified above; n-by-1
peaks = zeros(max(extract_events),1); % n-by-1

for e = 1:max(extract_events) % loop over # of boundaries
    clear vec
    vec = nan(size(Data)); % 1-by-27840
    vec(extract_events == e) = Data(extract_events == e);
    [peaks(e), etimes(e)] = max(vec);
end

if size(etimes,1) > 1 % n-by-1
    etimes = etimes'; % now, 1-by-n
end
if size(peaks,1) > 1 % n-by-1
    peaks = peaks'; % now, 1-by-n
end

concatRetro.boundaries = etimes;
concatRetro.peaks = peaks;

clear binaryvec
binaryvec = zeros(size(Data));
binaryvec(etimes) = 1;
concatRetro.allbinaryvec = logical(binaryvec); % the entire timecourse; movie01-movie10; a total of 27840 timepoints (1-by-27840)
% ^ notes: the allbinaryvec is a logical array

%% three event boundary conditions
% --- 3. we'll divide the boundaries into 3 bins depending on its prominence/salience (i.e., agreement values)
% weak-mid-strong boundaries
% again, the event boundaries are in the 1/10th second resolution...
clear thresh1 thresh2
thresh1 = prctile(peaks, round((1/numBins)*100)); % 1/3; ~33rd percentile
thresh2 = prctile(peaks, round((2/numBins)*100)); % 2/3; ~67th percentile

for j=1:numBins

    clear bintag xtimepoints
    if j == 1
        bintag = 'weak';
        xtimepoints = etimes(peaks <= thresh1); % <= 33rd percentile
    elseif j == 2
        bintag = 'mid';
        xtimepoints = etimes(peaks > thresh1 & peaks <= thresh2); % > 33rd percentile & <= 67th percentile
    elseif j == 3
        bintag = 'strong';
        xtimepoints = etimes(peaks > thresh2); % > 67th percentile
    end

    clear binvec
    binvec = zeros(size(Data));
    binvec(xtimepoints) = 1;
    concatRetro.(bintag).binbinary = logical(binvec); % the entire timecourse; movie01-movie10; a total of 27840 timepoints (1-by-27840)
    % ^ notes: the binbinary is a logical array

    fprintf('%s %d boundaries\n', bintag, length(find(binvec == 1)));
end

% extract timecourse for each movie...
for xm=1:length(movie_list)

    clear movienum movietime
    movienum = sprintf('movie%02d', xm);
    movietime = movie_100ms(xm,1):movie_100ms(xm,2); % again, in the 1/10th sec.

    concatRetro.(movienum).binaryBoundaries = logical(binaryvec(movietime)); % allbinaryvec, binbinary or binaryrvec: for all movies; binaryBoundaries: for individual movies
    concatRetro.(movienum).peaks = Data(movietime); % smoothed timecourse

    for xbin=1:numBins
        clear bintag
        if xbin == 1
            bintag = 'weak';
        elseif xbin == 2
            bintag = 'mid';
        elseif xbin == 3
            bintag = 'strong';
        end

        clear xbinvec
        xbinvec = concatRetro.(bintag).binbinary;
        concatRetro.(movienum).(bintag).binaryBoundaries = xbinvec(movietime); % in the 1/10th sec.
    end
end

%% 1/10th sec to TR
% --- 4. align the timestamps; in TR...

tag_bins = {'weak', 'mid', 'strong'};

for x_b=1:numBins % bins: weak-mid-strong
    clear curr_b
    curr_b = tag_bins{x_b}; % current bin (boundary condition)?

    movie1 = [];
    movie2 = [];
    for x_m=1:length(movie_list)
        clear curr_m
        curr_m = sprintf('movie%02d', x_m); % current movie?

        clear xmovie_dur xdata xdata_sec xbounds
        xmovie_dur = 1:length(concatRetro.(curr_m).(curr_b).binaryBoundaries); % timepoints: from 1 to the end timepoint of each movie (in the 1/10th sec)
        xdata = xmovie_dur(concatRetro.(curr_m).(curr_b).binaryBoundaries); % timestamps of event boundaries for each movie (in the 1/10th sec)
        xdata_sec = xdata/10; % now, in sec. resolution
        xbounds = xdata_sec + monsets(x_m); % boundaries adjusted by movie onset within each scanning run (in sec.)
        % ^ for example, 41 sec. of the filmfest part 1 video was the onset
        % of the CMIYC video file; human observers watched individual movie video files & each video starts from "0".
        % thus, the event boundary placed at 3.5 sec of the CMIYC video
        % would be 44.5 sec in the filmfest part 1 video.

        if x_m <= 5 % movie-viewing run #1
            clear m1
            m1 = xbounds;
            movie1 = [movie1 m1]; % concatenate onsets of events within movie-viewing run #1 (in sec.)
        else % m > 5; movie-viewing run #2
            clear m2
            m2 = xbounds;
            movie2 = [movie2 m2]; % concatenate onsets of events within movie-viewing run #2 (in sec.)
        end
    end
    movie1 = movie1'; movie2 = movie2'; % now, n-by-1

    % sec. to TR;
    for xrun=1:numRun
        clear xR timestamps timestamps_tr
        xR = sprintf('run%02d', xrun);

        if xrun == 1
            timestamps = movie1;
        else
            timestamps = movie2;
        end

        % timestamps_tr = round(timestamps/TR)+1; % onsets; event boundaries
        %in TR (within each movie-viewing run); this formula was used in
        %Hongmi's paper; formula 1; YJLee 20231026
        timestamps_tr = ceil(timestamps/TR); % YJLee, 20230728; formula 2; I might want to use this different formula this time...
        concatRetro.TR.(curr_b).(xR) = timestamps_tr;
        concatRetro.sec.(curr_b).(xR) = timestamps;
    end
end

% save the outcome...
% save(fullfile(dir_bound,
% 'retro_boundaries_from_concat_smoothed_only.mat'), 'concatRetro'); %
% ^ formula 2 was used for this file creation
save(fullfile(dir_bound, 'retro_boundaries_from_concat_smoothed_only_20231027_formula2.mat'), 'concatRetro'); % YJLee 20231026