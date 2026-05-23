%% This code estimates VAR of government spending shocks with standard identification

%% 0. PRELIMINARIES
%------------------------------------------------------------------
clear; close all; clc
warning off all
format short g

% define the working directory
home_folder = ("C:\Users\martynas\Desktop\uni\Year 3\AUTUMN\Time Series\Replication\MATLAB");

% add path to the package
addpath(genpath('\VAR-Toolbox-main\v3dot0'))

% add path to our own functions
addpath(genpath(sprintf('%s', home_folder)))

%% 1. LOAD AND STORE DATA
%******************************************************************
% Loading up the data set and preparing to store it in structure DATA
%------------------------------------------------------------------
% Load data from US macro data set
[datatables]=readtable('data/govdat3908.csv');

% Converting all columns to numeric
for v = 1:width(datatables) 
    col = datatables.(v);  % Extract column
    if iscell(col) || isstring(col) || ischar(col)
        % Convert string/cell to numeric
        datatables.(v) = str2double(col);
    end
end

dataraw=datatables{:,2:end};

datesnum = datatables{2:end,1};                           % vector of dates in numeric format
vnames_long = {'PDV of expected change in spending, billions of nominal dollars',...
    'PDV of expected change in spending Divided by GDP of previous quarter',...
    'Ramey-Shapiro Narrative variable Mistimed',...
    'Ramey-Shapiro Narrative variable','GDP price deflator','Real GDP','Real consumption',...
    'Real consumption of durables','Real consumption of nondurables',...
    'Real consumption of services','Real consumption of nondurables and services',...
    'Real investment','Real fixed investment','Real nonresidential investment',...
    'Real residential investment','Real government spending',...
    'Real federal spending','Real state and local spending',...
    'Capital Stock of Durables','Total hours worked',...
    'Military hours','Total population','Fraction of population aged 25-64 (over 16)',...
    'Fraction of population aged 25-64','Total hours (CES - Current Employment Statistics)',...
    'Nominal wage in manufacturing', 'Manufacturing price index',...
    'Consumer Price Index', 'Producer Price Index', 'Business price index',...
    'Nominal wage in business sector','BAA corporate bond rate',...
    '3-month Treasury bill rate','Romer exogenous shock variable',...
    'AMTSS','AMTBR',...
    'Survey of Professional Forecasters: lagged real federal spending',...
    'SPF: current quarter real federal spending forecast',...
    'SPF: 1-quarter ahead real federal spending forecast',...
    'SPF: 2-quarters ahead real federal spending forecast',...
    'SPF: 3-quarters ahead real federal spending forecast',...
    'SPF: 4-quarters ahead real federal spending forecast',...
    'SPF: lagged real state and local spending',...
    'SPF: current quarter real state and local spending forecast',...
    'SPF: 1-quarter ahead real state and local spending forecast',...
    'SPF: 2-quarters ahead real state and local spending forecast',...
    'SPF: 3-quarters ahead real state and local spending forecast',...
    'SPF: 4-quarters ahead real state and local spending forecast',...
    'SPF: lagged nominal defense spending',...
    'SPF: current quarter nominal defense spending forecast',...
    'SPF: 1-quarter ahead nominal defense spending forecast',...
    'SPF: 2-quarters ahead nominal defense spending forecast',...
    'SPF: 3-quarters ahead nominal defense spending forecast',...
    'SPF: 4-quarters ahead nominal defense spending forecast',...
    'SPF: lagged GDP price deflator',...
    'SPF: current quarter GDP price deflator forecast',...
    'SPF: 1-quarter ahead GDP price deflator forecast',...
    'SPF: 2-quarters ahead GDP price deflator forecast',...
    'SPF: 3-quarters ahead GDP price deflator forecast',...
    'SPF: 4-quarters ahead GDP price deflator forecast'}; % full variable names
vnames = datatables.Properties.VariableNames(2:end);      % variable mnemonic
nvar = length(vnames);                                    % number of variables in spreadsheet
data   = Num2NaN(dataraw);                                % matrix of data in spreadsheet

% Setting up logic to remove observations before 1947 (due to missing data
% in the data set)
mask = datesnum >= 1947;
data = data(mask,:);
datesnum = datesnum(mask);

% Store variables in the structure DATA
for ii=1:length(vnames)
    DATA.(vnames{ii}) = data(:,ii);
end
% Observations
nobs = size(data, 1);

%% 2. TREAT DATA
%******************************************************************
% Transforming the data by following the methods used by the authors
%------------------------------------------------------------------
% Creating per capita variables
percapita_pairs = {
        'rgdp', 'totpop', 'pc_rgdp';
        'rcons', 'totpop', 'pc_rcons';
        'rcnd', 'totpop', 'pc_rcnd';
        'rcsv', 'totpop', 'pc_rcsv';
        'rcdur', 'totpop', 'pc_rcdur';
        'rcndsv', 'totpop', 'pc_rcndsv';
        'rinv', 'totpop', 'pc_rinv';
        'rinvfx', 'totpop', 'pc_rinvfx';
        'rnri', 'totpop', 'pc_rnri';
        'rres', 'totpop', 'pc_rres';
        'tothours', 'totpop', 'pc_tothours';
        'tothoursces', 'totpop', 'pc_tothoursces';
        'rgov','totpop', 'pc_rgov';
        'rdef','totpop', 'pc_rdef';
    };

for ii = 1:size(percapita_pairs,1)
    num = percapita_pairs{ii,1};
    denom = percapita_pairs{ii,2};
    outname = percapita_pairs{ii,3};
    DATA.(outname) = DATA.(num) ./ DATA.(denom);
end

%Creating Real variables form nominal variables
realvars = {
   'nwbus', 'pbus', 'rwbus'; %rwbus = nwbus / pbus
   'nwmfg', 'pman', 'rwmfg'; %rwmfg = nwmfg / pman 
};
for ii = 1:size(realvars,1)
    nom = realvars{ii,1};
    deflator = realvars{ii,2};
    outname = realvars{ii,3};
    DATA.(outname) = DATA.(nom) ./ DATA.(deflator);
end

% Log transforming our variables of interest
logvars = {'pc_rgdp','pc_rcons', 'pc_rcnd', 'pc_rcsv', 'pc_rcdur', 'pc_rcndsv',...
    'pc_rinv', 'pc_rinvfx', 'pc_rnri','pc_rinvfx', 'pc_rnri', 'pc_rres',...
    'pc_tothours', 'pc_rgov', 'pc_rdef', 'rwbus', 'rwmfg', 'totpop',...
    'cpi', 'pgdp', 'pc_tothoursces'};
for ii = 1:length(logvars)
    varname = logvars{ii};
    newname = ['l' varname];
    DATA.(newname) = log(DATA.(varname));
end

% Constructing the SPF Shocks
fdlrdef1 = log(DATA.spf_ndef1 ./ DATA.spf_pgdp1) - log(DATA.spf_ndef0 ./ DATA.spf_pgdp0);
fdlrdef4 = log(DATA.spf_ndef4 ./ DATA.spf_pgdp4) - log(DATA.spf_ndef0 ./ DATA.spf_pgdp0);
fdlrfed1 = log(DATA.spf_rfed1 ./ DATA.spf_rfed0);
fdlrfed4 = log(DATA.spf_rfed4 ./ DATA.spf_rfed0);

% Forecast errors (shocks)
spfrdefshock1 = log(DATA.rdef ./ lag(DATA.rdef,1)) - lag(fdlrdef1,1);
spfrdefshock4 = log(DATA.rdef ./ lag(DATA.rdef,4)) - lag(fdlrdef4,4);
spfrfedshock1 = log(DATA.rfed ./ lag(DATA.rfed,1)) - lag(fdlrfed1,1);
spfrfedshock4 = log(DATA.rfed ./ lag(DATA.rfed,4)) - lag(fdlrfed4,4);

% Combine into SPF shocks
spfshock1 = spfrdefshock1;
spfshock1(isnan(spfshock1)) = spfrfedshock1(isnan(spfshock1));
spfshock4 = spfrdefshock4;
spfshock4(isnan(spfshock4)) = spfrfedshock4(isnan(spfshock4));

DATA.spfshock1 = spfshock1;
DATA.spfshock4 = spfshock4;

delete temp* nom denom outname deflator varname newname

%% 3. VAR ESTIMATION
%******************************************************************
% Select the list of endogenous variables...
Xvnames = {'lpc_rgov','lpc_rgdp', 'lpc_rcndsv', 'lpc_rinvfx', 'lpc_tothoursces','lrwbus', 'amtbr'};
% ... and corresponding labels to be used in plots
Xvnames_long = {'Log Per Capita Real Government Spending','Log Per Capita Real GDP',...
    'Log Per Capita Real Consumption of Nondurables and Services', 'Log Per Capita Real Fixed Investment',...
    'Log Per Capita Real Total Hours CES','Log of Real Compensation in Business','Barro-Redlick Tax Rate'};
% Number of endo variables
Xnvar = length(Xvnames);
% Create matrix X of variables to be used in the VAR
X = nan(nobs,Xnvar);
for ii=1:Xnvar
    X(:,ii) = DATA.(Xvnames{ii});
end
% Open a figure of the desired size and plot the selected variables
FigSize(26,60)
for ii=1:Xnvar
    subplot(4,2,ii)
    H(ii) = plot(X(:,ii),'LineWidth',3,'Color',cmap(1));
    title(Xvnames_long(ii));
    DatesPlot(datesnum(1),nobs,6,'q') % Set the x-axis labels
    grid on;
end
% Save figure
print(fullfile(home_folder,'graphics','MAINVAR_DATA.pdf'),'-dpdf');
close all

% Make a common sample by removing NaNs
[X, fo, lo] = CommonSample(X);
% Seting the deterministic variable in the VAR
det = 3; % linear + quadratic trend

% Set number of lags (using 4, as defined in the paper)
nlags = 4;

% Estimate VAR by OLS
[VAR, VARopt] = VARmodel(X,nlags,det);
% Print at screen the outputs of the VARmodel estimation
format short
disp(VAR)
disp(VAR.F)      % plotting coefficients
disp(VAR.sigma)  % plotting variance-covariance matrix of errors
disp(VAR.maxEig) % maximum eigenvalue that allows to check for the stability
abs(eig(VAR.Fcomp))
disp(VARopt)

% Update the VARopt structure with additional details
VARopt.vnames = Xvnames_long;
% Print at screen VAR coefficients and create table
[TABLE, beta] = VARprint(VAR,VARopt,2);

%% 4. IDENTIFICATION WITH ZERO CONTEMPORANEOUS RESTRICTIONS
%******************************************************************
% Update the VARopt structure to select zero short-run restrictions
VARopt.ident = 'short'; % Choleski identification
% Update the VARopt structure with additional details
VARopt.vnames = Xvnames_long;            % variable names in plots
VARopt.nsteps = 20;                      % max horizon of IRF
VARopt.FigSize = [30,55];                % size of window (figures)
VARopt.firstdate = datesnum(1);          % first date in plots
VARopt.frequency = 'q';                  % frequency of the data
VARopt.snames = {'\epsilon^{Gov}',...
                    '\epsilon^{1}',...
                    '\epsilon^{2}',...
                    '\epsilon^{3}',...
                    '\epsilon^{4}',...
                    '\epsilon^{5}',...
                    '\epsilon^{6}'};
VARopt.figname= 'graphics\SR_';% Compute impulse response
[IR, VAR] = VARir(VAR,VARopt);

% Compute structural shocks
eps_short = (VAR.B\VAR.resid')';    % u_t = B*eps_t => eps_t = inv(B)*u_t

% Compute error bands
[IRinf,IRsup,IRmed,IRbar] = VARirband(VAR,VARopt);
% Plot
% Plotting only the one impulse response figure that we are interested in
VARirplot(IRbar(:,:,1), VARopt, IRinf(:,:,1), IRsup(:,:,1));

%% Testing Granger-causality
% Prepating data for the Granger causality tests
gov_shock = eps_short(:,1);      
T = length(gov_shock);
wara_full = DATA.wara;        
wara = wara_full(end-T+1:end);  

% Granger causality test
p = 4;
[h1, pValue1] = gctest(gov_shock, wara, 'NumLags', p); % gov_shock -> wara
[h2, pValue2] = gctest(wara, gov_shock, 'NumLags', p); % wara -> gov_shock
[h3, pValue3] = gctest(spfshock1, gov_shock); % spfshock1 -> gov_shock
[h4, pValue4] = gctest(spfshock4,gov_shock); % spfshock4 -> gov_shock

AlternativeForGCTest = {'gov_shock -> wara'; 'wara -> gov_shock'; 'spfshock1 -> gov_shock';'spfshock4 -> gov_shock'};
RejectNull = [h1; h2; h3;h4];           % 1 = reject null, 0 = fail to reject
PValue = [pValue1; pValue2; pValue3; pValue4];     % corresponding p-values

ResultsTable = table(AlternativeForGCTest, RejectNull, PValue);

% Display
disp(ResultsTable); % screenshots used in the presentation
