
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Minimum distance estimation of equilibrium condition
%   for networks in conflict paper.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jonathan Old
%% Preliminaries
cd('/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/')

clear ;
rng(12345);

%% Part 1: Read in data
% Read in Enemity and Alliship data
years=13;
aminus = readtable('./regressions/A_minus.csv');
b = size(aminus);
c = b(1,1);
d = c+1;
ammat = aminus{1:c, 2:d};

aplus = readtable('./regressions/A_plus.csv');
apmat = aplus{1:c, 2:d};

% Read in Main dataset
 cdata = readtable('./regressions/data_panel_for_matlab.csv');

 disp('Data succesfully read in')
 %% Part 2: estimate
 
 %fun = @(x)sum((cdata{:,"TotFight"}-...
 %           (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))*...
 %           (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))* ...
 %           inv(eye(1040)+x(1)*kron(eye(13),apmat)+x(2)*kron(eye(13),ammat))* ...
 %           (1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1)))).^2) ;
 beta = 0.114;
 gamma = 0.083;


  fun = @(x)sum((cdata{:,"TotFight"}-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(c*years)+x(1)*kron(eye(years),apmat)-x(2)*kron(eye(years),ammat))* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"}))).^2) ;
A=[];
b=[] ;
Aeq=[];
beq=[];
ub=[0.999 0.999];
        lb=[0 0] ;
        nlcond=[];
        nonlcon=[];

% nonlcon = @unitcircle;
options = optimoptions( 'ga', Display=' off');
x1 = ga(fun, 2, A, b, Aeq, beq, lb, ub, nonlcon, options);
x1=[0,0];
% Local optimum, using global search
%options = optimoptions( 'fminunc', Display=' off');
  options = optimoptions('fmincon',Display='off');
        [x2,fval] = fmincon(fun,x1,A,b,Aeq,beq,lb,ub,nonlcon,options);

         results(1,1)=x2(1);
        results(1,2)=x2(2);

%% Part 2b: Matlab NLLS
 x = [cdata{:,"degree_plus"},cdata{:,"degree_minus"}];
        y = cdata{:,"TotFight"};
        mfun = @(b,x)((1-1/sum(1./(1+b(1)*x(:,1)-b(2)*x(:,2))))*(1/sum(1./(1+b(1)*x(:,1)-b(2)*x(:,2))))*inv(eye(c*years)+b(1)*kron(eye(years),apmat)-b(2)*kron(eye(years),ammat))*(1./(1+b(1)*x(:,1)-b(2)*x(:,2))) );
        beta0 = [0.01 0.01];
        mdl = fitnlm(x,y,mfun,beta0);


results(1,3)=mdl.Coefficients{1,1};
   results(1,4)=mdl.Coefficients{2,1};



writematrix(results,'./regressions/matlab_drc.csv');




