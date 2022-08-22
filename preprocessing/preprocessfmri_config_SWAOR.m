%-Configfile for preprocessfmri.m
%- for spm 12
%__________________________________________________________________________

paralist.spmversion = 'spm12'; 

%-Please specify parallel or nonparallel
%-e.g. for preprocessing and individualstats, set to 1 (parallel)
%-for groupstats, set to 0 (nonparallel)
paralist.parallel = '1';

%-Subject list
paralist.subjectlist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/subjectlist/MathFUN_mindset_Scan_Subjectlist_PreProcess_All.csv'
%---- example ----
%- PID, visit, session
%- 7014, 1 ,1 

%-Run list
paralist.runlist = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/subjectlist/run_list_addition.txt';
%--- example -----
%-- comparison_dot


%-The entire preprocessing to be completed
%-Choose from: 'swar',  'swavr', 'swaor', 'swgcar',  'swgcavr', 'swgcaor'
%-             'swfar', 'swfavr', 'swfaor', 'swgcfar', 'swgcfavr', 'swgcfaor'
%-"s" is smoothing; "w" is normalization; "a": slice timing correction ; "r": realignment
%-"c" is coregistration; "g" is use segmented t1 images while
%coregistration
%-"v" is the 1st version and "o" is the 2nd version of VolRepair pipeline
%-"f" is for fmri images that were acquired flipped
paralist.pipeline = 'swaor';

% I/O parameters
% - Raw data directory
paralist.rawdatadir = '/oak/stanford/groups/menon/rawdata/scsnl/';

% - Project directory - output of the preprocessing will be saved in the
% data/imaging folder of the project directory
paralist.projectdir = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/';
%%% ----------example -------------------
% '/oak/stanford/groups/menon/projects/shelbyka/2017_TD_MD_mathfun'


% - Output directory name
paralist.outputdirname = 'SWAOR_spm12';

% fMRI parameters
% - spm8 batch templates location
paralist.batchtemplatepath = '/oak/stanford/groups/menon/scsnlscripts/brainImaging/mri/fmri/preprocessing/spm12/preprocessfmrimodules/batchtemplates/';

% - prefix for the unnormalized file name. for scsnl lab. this value is
% usually empty
paralist.inputimgprefix = '';

% - TR value
paralist.trval = 2;

% - Custom slice timing
% - "customslicetiming" should be 0 or 1;
paralist.customslicetiming = 0;

% - if customslicetiming = 0, please set  "slicetimingfile" as '';
% - if customslicetiming = 1, please specify "slicetimingfile" and set sliceacq as ''; 
paralist.slicetimingfile = '';


%- if customslicetiming = 0, spm built-in  options are 'interleaved', 'ascend','descend'; 
% paralist.sliceacq= 'interleaved';
paralist.sliceacq= 'ascend';


% - smoothing kernel
paralist.smoothwidth       = [6 6 6];

% - bounding box
paralist.boundingboxdim     = [-90 -126 -72; 90 90 108];

% Coregistration Parameters:
%-Additinal subject list for swgc** pipelines due to better SPGR quality,
% one-to-one matched to paralist.SubjectList
paralist.spgrsubjectlist = ' ';
%%---- example of spgrsubjectlist.csv ------
% PID, visit, session
% 7014, 1 ,1 


% name of the skullstriped T1w volume:
% 'spgr' for original image
% 'skullstrip_spgr_spm12' for brains stripped with SPM12 pipeline
% 'watershed_spgr' for brains stripped with mri_watershed

paralist.spgrfilename = 'spgr';
