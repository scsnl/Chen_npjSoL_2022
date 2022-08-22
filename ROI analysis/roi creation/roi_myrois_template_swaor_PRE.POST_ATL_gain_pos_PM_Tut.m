paralist.parallel= 0 ;
%please put project dir for job submission output and error files
paralist.projectdir='/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/';

% This is the folder which you will save defined ROIs
roi_folder = '/oak/stanford/groups/menon/projects/lchen32/2018_MathFUN_mindset/data/imaging/roi/ROIs_PRE.Post_swaor_ATL_gain_PM_Tut/';

% Define ROIs by specifying name, coordinates and radius
myroi{1}.name = 'ACC_1';
myroi{1}.coords = [0 12 22];
myroi{1}.radius = 6;

myroi{2}.name = 'ACC_2';
myroi{2}.coords = [-2 22 16];
myroi{2}.radius = 6;

myroi{3}.name = 'R-Putamen';
myroi{3}.coords = [34 0 0];
myroi{3}.radius = 6;

myroi{4}.name = 'R-Hippocampus';
myroi{4}.coords = [34 -10 -18];
myroi{4}.radius = 6;


