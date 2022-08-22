% Configuration file for scsnl_gPPI.m
% _________________________________________________________________________
% 2013 Stanford Cognitive and Systems Neuroscience Laboratory

paralist.spmversion = 'spm12';
paralist.parallel = '1';
% Please specify the data server path
paralist.projectdir = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/';

%paralist.maskfile ='';  % this will give a whole brain map
% If the mask is specified, only voxels in the mask will be calculated
paralist.maskfile = '';

% Please specify the subject list file (.txt) or a cell array

paralist.subjectlist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/subjectlist/MathFUN_mindset_Scan_Subjectlist_ArtRep_1run.csv';

% Please specify the stats folder name (eg., stats_spm8) 
paralist.stats_folder = 'stats_spm12_swaor_VolRepair_addition_ArtRep_PM';

% Please speficy the gPPI  output folder; If you don't specify then results will save to ['stats_folder'_gPPI]
paralist.gPPI_output_folder = 'gPPI_swaor_VolRepair_addition_ArtRep_Pre.Post_PM_final';

% Please specify the .nii file(s) for the ROI(s)
paralist.roi_file_list = {'roilist_Pre.Post_final.txt'};

% Please specify the name of the ROI
paralist.roi_name_list = {'roilist_names_Pre.Post_final.txt'};

%% Please specifiy the way to extract time series (mean or eigenvector)
%paralist.extract_type = 'mean';
 paralist.extract_type = 'eig';

% Please specify the task to include
% tasks_to_include = { '1', 'task1', 'task2', 'task3'} -> must exist in all sessions
% tasks_to_include = { '0', 'task1', 'task2', 'task3'} -> does not need to exist in all sessions
%paralist.tasks_to_include = {'1','ON','OFF'};
paralist.tasks_to_include = {'1','problem_correct','problem_incorrect','decision_correct','decision_incorrect'};

paralist.contrastmat = 'gPPI_contrasts_1run.mat';

% option 1: save all files
% option 2: save only matrices
% option 3: save nothing, either you are a developer or enjoy digging scratch tmp files, or maybe both  
paralist.copy_type = '1' ;
%-------------------------------------------------------------------------%
% Please specify the confound names
paralist.confound_names = {};
%paralist.confound_names = {'R1', 'R2', 'R3', 'R4', 'R5', 'R6'};
