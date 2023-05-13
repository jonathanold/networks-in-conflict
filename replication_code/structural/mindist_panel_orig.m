
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
aminus = readtable('./regressions/A_minus.csv');
ammat = aminus{1:80, 2:81};

aplus = readtable('./regressions/A_plus.csv');
apmat = aplus{1:80, 2:81};

% Read in Main dataset
 cdata = readtable('./regressions/data_panel_for_matlab.csv');

 disp('Data succesfully read in')
 %% Part 2: estimate
 
 %fun = @(x)sum((cdata{:,"TotFight"}-...
 %           (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))*...
 %           (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1))))* ...
 %           inv(eye(1040)+x(1)*kron(eye(13),apmat)+x(2)*kron(eye(13),ammat))* ...
 %           (1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)+x(2)*repmat(cdata{:,"degree_minus"},13,1)))).^2) ;
 beta = -0.114;
 gamma = 0.083;

   im = inv(eye(1040)+beta.*kron(eye(13),apmat)+gamma.*kron(eye(13),ammat));
   g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
   l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
  tf_sim = im*g.*l.*(1-l)+randn([1040,1]);


  fun = @(x)sum((cdata{:,"TotFight"}-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(1040)+x(1)*kron(eye(13),apmat)+x(2)*kron(eye(13),ammat))* ...
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
y = cdata{:,"TotFight"};
modelfun = @(b,x)b(1) + b(2)*x(:,1).^b(3) + ...
    b(4)*x(:,2).^b(5);
mfun = @(b,x)((1-1/sum(1./(1+b(1)*x(:,1)+b(2)*x(:,2))))*(1/sum(1./(1+b(1)*x(:,1)+b(2)*x(:,2))))*inv(eye(1040)+b(1)*kron(eye(13),apmat)+b(2)*kron(eye(13),ammat))*(1./(1+b(1)*x(:,1)+b(2)*x(:,2))) );
beta0 = [0 0];
mdl = fitnlm(x,y,mfun,beta0)

%% Part 3: Other stiuff

  i=0;
  results = zeros(1,6);
 % Loop over values of beta and gamma
for beta = -0.12:0.00088:-0.05
    for gamma = 0.08:0.00198:0.15
       % beta = -0.12;
       % gamma = 0.08;

        i=i+1;
        results(i,1)=beta;
        results(i,2)=gamma;
        % Simulate total fighting (tf_sim) with values of beta and gamma
        im = inv(eye(800)+beta.*kron(eye(10),apmat)+gamma.*kron(eye(10),ammat));
        g = (1./(1+beta*repmat(cdata{:,"degree_plus"},10,1)+gamma*repmat(cdata{:,"degree_minus"},10,1))); % g is correct
        l = 1-1/sum(1./(1+beta.*repmat(cdata{:,"degree_plus"},10,1)+gamma.*repmat(cdata{:,"degree_minus"},10,1))); % l is correct
        tf_sim = im*g.*l.*(1-l)+randn([800,1])*0.1;
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},10,1)+x(2)*repmat(cdata{:,"degree_minus"},10,1))))*...
            (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},10,1)+x(2)*repmat(cdata{:,"degree_minus"},10,1))))* ...
            inv(eye(800)+x(1)*kron(eye(10),apmat)+x(2)*kron(eye(10),ammat))* ...
            (1./(1+x(1)*repmat(cdata{:,"degree_plus"},10,1)+x(2)*repmat(cdata{:,"degree_minus"},10,1)))).^2) ;

        % Find global minimum
        x0=[0.11,0.1];
        %gs = GlobalSearch;
        %problem = createOptimProblem('fmincon','x0',[0,0],...
         %   'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        %x1 = run(gs,problem);
        disp(beta);
        disp(gamma);
        % Find local minimum around global minimum
        [x2,fval] = fminunc(fun,x0);
        results(i,3)=fval;
        results(i,4)=x2(1);
        results(i,5)=x2(2);
        results(i,6)=mg;
    end
end

writematrix(results,'./regressions/matlab_simulation_panel.csv');


%% Function definition
function [c,ceq] = unitcircle(x);
c = x(1)^2 + x(2)^2;
ceq = [];
