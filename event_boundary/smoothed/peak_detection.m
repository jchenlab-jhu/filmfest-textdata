% original author: Sebastian Michelmann
% 20200412, YJ applied the original code to the retrospective event boundaries data
% 1/10th second resolution
% 20201110, YJ modified the code to analyze FF2 dataset

% Yoonjung Lee
% Oct 17, 2022 for manuscript

% edited by Yoonjung Lee
% 20221024, onsets of movie titles were removed in individuals'
% spreadsheets before running this script. 

% YJLee 20231023
% data check for manuscript (both for the clustering and the cortical
% alignment project)

%% set-up
clear all; close all; %clc;

% wd = '/Volumes/yjlee_ssd/manuscripts/clustering/data/';
wd = '/Volumes/yjlee_ssd/projects/clustering/data/';
options = 'data_excluding_onsets_titles'; % two sub-folders: 1_'data_including_onsets_titles' or 2_'data_excluding_onsets_titles' % edited by YJ, 20221024
% options = 'data_including_onsets_titles'; % YJLee 20231023
dir_bound = fullfile(wd, 'retrospective_boundaries', options);
tools = fullfile(wd, 'manuscript_codes');
addpath(tools);

% --- movies
movie_list = {'1_catch_me_if_you_can', '2_the_record', '3_the_boyfriend', '4_the_shoe', ...
    '5_keith_reynolds', '6_the_rock', '7_the_prisoner', '8_the_black_hole', ...
    '9_post_it_love', '10_bus_stop'};

titles = {'Catch Me If You Can', 'The Record', 'The Boyfriend', 'The Shoe', ...
    'Kieth Reynolds', 'The Rock', 'The Prisoner', 'The Black Hole', ...
    'Post It Love', 'Bus Stop'};

% movie_list = {'1_catch_me_if_you_can', '2_the_record', '3_the_boyfriend', ...
%     '4_the_shoe', '5_keith_reynolds', '6_the_rock', '7_the_prisoner', ...
%     '8_the_black_hole', '9_post_it_love', '10_bus_stop', '11_paperman', ...
%     '12_inner_workings', '13_purl', '14_near_algodones', '15_futuer_boyfriend', ...
%     '16_in_the_times_it_takes_to_get_there', '17_the_lie_game', '18_runner', ...
%     '19_zima_blue', '20_from_life'};

% titles = {'Catch Me If You Can', 'The Record', 'The Boyfriend', 'The Shoe', 'Kieth Reynolds', ...
%     'The Rock', 'The Prisoner', 'The Black Hole', 'Post It Love', 'Bus Stop', ...
%     'Paperman', 'Inner Workings', 'Purl', 'Near Algodones', 'Future Boyfriend', ...
%     'In the Times ...', 'The Lie Game', 'Runner', 'Zima Blue', 'From Life'};

% parameters for peak boundary detection
w = 10; % e.g., if w=5, +- 500 ms (+- 5 1/10th-sec time points; +- 0.5 sec time points) on either side of the timestamp
sm_ = 20; % 2 seconds; a variable that holds the value 20; for data smoothing; fitting Gaussian curve
perc = [65 80 85 90 95]; % we'll look for a specific percentile value of a response distribution
for p=1:length(perc)
    ptag{p} = sprintf('perc%02d', perc(p));
end

% perc = [0.2 0.4 0.6]; % percentage of all observers; it won't be used...
% for p=1:length(perc)
%     ptag{p} = sprintf('perc%02d', perc(p)*100);
% end

fontsize = 12; % font size; for plotting
fontname = 'Arial';
colorarrays = [0, 0, 1;
    0.9290, 0.6940, 0.1250;
    0.4940, 0.1840, 0.5560;
    0, 0.5, 0;
    1, 0, 0]; % for different percentile thresholds
lwidth1 = 0.5;
lwidth2 = 1;
lwidth3 = 0.8;
msize = 4;

%% peak boundary detection
for xmovie=1:length(movie_list)
    clear movie_name movie_title data movie_tag
    movie_name = movie_list{xmovie}; % select a movie
    movie_title = titles{xmovie}; % select a movie title
    data = dir(fullfile(dir_bound, movie_name));
    movie_tag = sprintf('movie%02d', xmovie);
    
    fprintf('********** %s & time window %02dms\n', movie_title, w*100);
    data_files = {};
    for i=1:length(data)
        clear data_name tag
        data_name = data(i).name;
        if ~isempty(strfind(data(i).name, movie_name))
            fprintf('%s\n', data_name);
            data_files{end+1} = data_name;
        end
    end
    
    fprintf('----------\n');
    clear nsubs firstbound lastbound maxsec
    nsubs=length(data_files); % how many subjects? (e.g., 15 subjects for CMIYC)
    for n=1:nsubs
        clear NUM TXT RAW onsets_cell
        [NUM,TXT,RAW] = xlsread(fullfile(dir_bound, movie_name, data_files{n}));
        onsets_cell = TXT(2:end,3); % the total number of event boundaries identified by a subject
        
        % ev: boundaries in "sec" resolution
        for i=1:length(onsets_cell)
            clear timestr
            timestr = onsets_cell{i};
            [mm,ss,dd] = convert_time_msd(timestr);
            edata.(movie_tag).ev(n).onsets(i) = (mm*60)+ss+(dd*0.1);
        end
        
        if edata.(movie_tag).ev(n).onsets(1) == 0
            edata.(movie_tag).ev(n).onsets = edata.(movie_tag).ev(n).onsets(2:end); % remove the 0.0.0 "boundary"; the very beginning of a movie clip
        end
        firstbound(n) = edata.(movie_tag).ev(n).onsets(1); % the first event boundary for each subject
        lastbound(n) = edata.(movie_tag).ev(n).onsets(end); % the last event boundary for each subject
        fprintf('rater #%02d identified %02d segments.\n', n, length(edata.(movie_tag).ev(n).onsets));
    end
    maxsec = max(lastbound); % the last event boundary (from 'all' subjects data)
    
    fprintf('----------\n');
    clear find1stbound
    find1stbound = min(firstbound);
    fprintf('the very first event boundary detected by an observer was at %.1f sec.\n', find1stbound);
    
    clear evec_ind evec_all
    % convert data into 1/10th-sec resolution
    evec_ind = zeros(1,(round(maxsec)+5)*10); % zeros vector; 1 entry per 1/10th second; padding of 5 sec for plotting
    evec_all = zeros(nsubs,length(evec_ind)); % subject-by-time array; rows_subjects, columns_1/10th seconds
    
    clear fig; clear clf;
    fig = figure; clf;
    subplot(2,1,1);
    for n=1:nsubs
        clear onsets10
        onsets10 = edata.(movie_tag).ev(n).onsets*10; % convert boundaries from "sec" resolution to "1/10th-sec" resolution
        plot(onsets10,ones(1,length(onsets10))*n,'k+','LineWidth',1); hold on; % mark event boundaries judged by individuals
        
        for j=1:length(onsets10)
            clear range1 range2
            range1 = onsets10(j)+1:onsets10(j)+w; % e.g., + 5 1/10th-sec (+ 500 ms; + 0.5 sec) if w = 5
            evec_all(n,range1) = evec_all(n,range1)+1;
            if onsets10(j)~=1
                if onsets10(j)-w <= 0
                    range2 = 1:onsets10(j);
                else
                    range2 = (onsets10(j)-w):onsets10(j); % e.g., from - 5 1/10th-sec (- 500 ms; - 0.5 sec) to the current time point of an event boundary if w = 5
                end
            else % onsets10(j) == 1
                range2 = onsets10(j);
            end
            evec_all(n,range2) = evec_all(n,range2)+1;
        end
    end
    ebounds.(movie_tag).dallsubj = evec_all; % notes: if two event boundaries were placed in 500ms by the participant, that subject's row can contain values > 1 
    
    ylim([0 nsubs+1]);
    xlim([0 length(evec_ind)]);
    yticks(0:nsubs+1);
    grid on
    title(sprintf('%s (N = %02d)', movie_title, nsubs));
    xlabel('Time Point (One-Tenth Seconds)');
    ylabel('Participant');
    set(gca,'FontSize', fontsize, 'FontName', fontname);
    set(gca,'TickDir','out');
    
    % Sebastian's comments: dts1 is a subjects x time matrix, holding the individual responses as 0 and 1 that indicate, whether the space-bar was pressed in the surrounding second
    % (the name dts1 means data -subjects on run 1)
    % so if at 5000 ms sj 3 pressed the space bar, you would see dts1(3,4498:5502) as [0 0 0 1 1 1 1 1 ...... 1 1 1 0 0]
    % +- 500 ms window (e.g, 4501 ~ 5500, coded 1)
    
    clear dts smd
    dts = evec_all;
    dts(dts~=0) = 1; % an array having binary values (0_non-boundary time points, 1_boundary time points)
    smd = smoothdata(mean(dts(1:nsubs,:)),'gaussian',sm_); % smd1 is the smoothed agreement; the key variable that we'll keep using during analyses
    ebounds.(movie_tag).dsmoothed = smd;
    
    subplot(2,1,2);
    plot(mean(dts(1:nsubs,:)), 'Color', [0.3010, 0.7450, 0.9330], 'LineWidth', lwidth1); hold on;
    plot(smd, 'Color', [0.6350, 0.0780, 0.1840], 'LineWidth', lwidth2);

    xlim([0 length(evec_all)]);
    title('Peak Boundary Detection');
    xlabel('Time Point (One-Tenth Seconds)');
    ylabel('Agreement Across Participants');
    set(gca,'FontSize', fontsize, 'FontName', fontname);
    
    fprintf('----------\n');
    for t=1:length(perc)
        clear xp thres percentile extract_events
        xp = ptag{t};
        thres = perc(t);
        percentile = prctile(smd, thres);
        ebounds.(movie_tag).(xp).percentile = percentile;
        fprintf('the %02dth percentile of smoothed data is %.7f.\n', thres, percentile);
        extract_events = bwlabeln(smd > percentile); % bwlabeln is an spm function that labels connected blocks of ones. i.e. 1 1 1 1 0 0 0 1 1 0 1 1  becomes 1 1 1 1 0 0 0 2 2 0 3 3
        
        % extract_events = bwlabeln(smd > thres); % we can use this command line if we simply look for
        % time points where over X % of observers agreed.
        
        % this will collect the time-values, which is equivalent to the sample-points in my data
        clear etimes peaks
        etimes = zeros(max(extract_events),1); % # of boundaries identified above
        peaks = zeros(max(extract_events),1);
        for e = 1:max(extract_events) % smd, zvec all in the same dimension
            clear zvec
            zvec = zeros(size(smd));
            zvec(extract_events == e) = smd(extract_events == e);
            [peaks(e), etimes(e)] = max(zvec);
        end
        
        if size(etimes,1) > 1 % multiple rows x 1; n-by-1
            etimes = etimes';
        end
        if size(peaks,1) > 1 % multiple rows x 1; n-by-1
            peaks = peaks';
        end
        
        fprintf('>> %02d event boundaries were identified.\n', length(etimes));
        ebounds.(movie_tag).(xp).boundaries = etimes;
        ebounds.(movie_tag).(xp).peaks = peaks;
        
        % plotting final event boundaries
        hold on;
        plot(etimes, peaks+0.02*t, 'o', 'MarkerEdgeColor', colorarrays(t,:), 'MarkerFaceColor', colorarrays(t,:), 'MarkerSize', msize); 
    end % / retrospective event boundaries
    
    % for each movie, plot 216-seg event boundaries
    clear NUM TXT RAW onsets_cell bounds  
    [NUM,TXT,RAW] = xlsread(fullfile(wd, 'retrospective_boundaries', 'seg216', sprintf('%s.xlsx', movie_name)));
    onsets_cell = TXT(2:end,2);
    % boundaries in "sec" resolution
    for i=1:length(onsets_cell)
        clear timestr
        timestr = onsets_cell{i};
        [mm,ss,dd] = convert_time_msd(timestr);
        bounds(i) = (mm*60)+ss+(dd*0.1);
    end
    clear xbegin ons ons10
    xbegin = bounds(1);
    ons = bounds - xbegin;
    if ons(1) == 0
        ons = ons(2:end); 
    end
    ons10 = ons*10;
    if ~isempty(find(ons10 <= 0))
        fprintf('!!!!!! check your SEG-216 file !!!!!!\n');
    end
    
    fprintf('-- SEG-216 %02d events --\n', length(ons10));
    ebounds.(movie_tag).seg216 = ons10;
    
    for j=1:length(ons10)
        hold on;
        xline(ons10(j), ':', 'LineWidth', lwidth3);
    end
    set(gcf, 'Units', 'pixels');
    set(gca,'TickDir','out');
    set(gcf, 'Position', [0 0 1200 700]);
    hold off;
    legend({'non-smoothed', 'smoothed', ptag{1}, ptag{2}, ptag{3}, ptag{4}, ptag{5}});
    % saveas(fig, fullfile(wd, 'figures', sprintf('retrospective_boundaries_peaks_%02d_%s_w%02d_%s.fig', xmovie, movie_title, w, options)));
end

% save variables in .mat
% save(fullfile(wd, 'retrospective_boundaries', sprintf('filmfest_retrospective_boundaries_peaks_w%02d_%s.mat', w, options)), 'w', 'ebounds', 'edata');
save(fullfile(wd, 'retrospective_boundaries', sprintf('filmfest_retrospective_boundaries_peaks_w%02d_%s_20231023.mat', w, options)), 'w', 'ebounds', 'edata');