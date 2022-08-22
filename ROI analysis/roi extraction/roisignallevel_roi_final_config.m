%This configuration file is read by roisignallevel.m
%
%__________________________________________________________________________
% 2009-2010 Stanford Cognitive and Systems Neuroscience Laboratory
%
% $Id: roisignallevel_config.m.template, Kaustubh Supekar, 2018-03-16 $
% -------------------------------------------------------------------------

%-Please specify parallel or nonparallel
paralist.parallel = '0';

%-Subject list
paralist.subjectlist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/subjectlist/MathFUN_mindset_Scan_Subjectlist_ArtRep_PrePost_all.csv';

%-Run list
paralist.runlist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/subjectlist/run_list_addition.txt'; 

% I/O parameters
% - Raw data directory
paralist.rawdatadir = '/oak/stanford/groups/menon/rawdata/scsnl/';

% - Project directory
paralist.projectdir = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/';

% Please specify the folder containing SPM analysis results
paralist.stats_folder    = 'stats_spm12_swaor_VolRepair_addition_ArtRep_PM';

% ROI parameters
% Please specify the folder (full path) holding defined ROIs
paralist.roi_folder    = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/imaging/roi/ROIs_PREPOST_final/';

% Please specify the folder name to hold saved roi statistics
% You can change it to different studies or settings
paralist.roiresult_folder = 'roi_stats_PREPOST_final';

% Please specify roi list file (with file name extensions)
paralist.roilist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/imaging/roi/ROIs_PREPOST_final/roilist_PrePost_final.txt';

% Please specify the t statistic threshold
paralist.tscorethreshold = 2.33;

%-SPM version
paralist.spmversion = 'spm12';

%--------------------------------------------------------------------------
