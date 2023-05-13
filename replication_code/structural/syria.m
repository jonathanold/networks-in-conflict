
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Minimum distance estimation of equilibrium condition
%   for networks in conflict paper.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Author: Jonathan Old
%% Preliminaries
cd('/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/original_data/')

clear ;
rng(12345);

%% Part 1: Read in data
% Read in Enemity and Alliship data
aminus = readtable('./A_minus_syria.csv');
ammat = aminus{1:106, 2:107};

aplus = readtable('./A_plus_syria.csv');
apmat = aplus{1:106, 2:107};

% Read in Main dataset
 cdata = readtable('./data_syria.csv');

 disp('Data succesfully read in')
 %% Part 2: estimate
 
 %fun = @(x)sum((cdata{:,"TotFight"}-...
 %           (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))*...
 %           (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))* ...
 %           inv(eye(1040)+x(1)*kron(eye(13),apmat)+x(2)*kron(eye(13),ammat))* ...
 %           (1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1)))).^2) ;
 beta = -0.114;
 gamma = 0.083;

   im = inv(eye(106)+beta.*apmat+gamma.*ammat);
   g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
   l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
  tf_sim = im*g.*l.*(1-l)+0.1*randn([106,1]);


  fun = @(x)sum((cdata{:,"tf_events"}-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(106)+x(1)*apmat+x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"}))).^2) ;
A=[];
b=[] ;
Aeq=[];
beq=[];
ub = [0.999 0.999];
lb = [-0.999 -0.999];

% nonlcon = @unitcircle;
nonlcon = [];
options = optimoptions( 'ga', Display=' off');
x1 = ga(fun, 2, A, b, Aeq, beq, lb, ub, nonlcon, options);
% Local optimum, using global search
%options = optimoptions( 'fminunc', Display=' off');
[x2,fval2] = fminunc(fun,x1);

options = optimoptions( 'patternsearch', Display=' off');
x3 = patternsearch(fun,[0 0],A,b,Aeq,beq,lb,ub,nonlcon,options);
% Local optimum, using patternsearch as starting value
[x4,fval4] = fminunc(fun,x3);

% 
[x5,fval5,exitflag] = particleswarm(fun,2,lb,ub);
[x6,fval6] = fminunc(fun,x5);








%% Part 2b: Matlab NLLS

x = [cdata{:,"degree_plus"},cdata{:,"degree_minus"}];
y = cdata{:,"tf_events"};
modelfun = @(b,x)b(1) + b(2)*x(:,1).^b(3) + ...
    b(4)*x(:,2).^b(5);
mfun = @(b,x)((1-1/sum(1./(1+b(1)*x(:,1)+b(2)*x(:,2))))*(1/sum(1./(1+b(1)*x(:,1)+b(2)*x(:,2))))*inv(eye(106)+b(1)*apmat+b(2)*ammat)*(1./(1+b(1)*x(:,1)+b(2)*x(:,2))) );
beta0 = [0 0];
mdl = fitnlm(x,y,mfun,beta0)


%% Function definition
function [c,ceq] = unitcircle(x);
c = x(1)^2 + x(2)^2;
ceq = [];
