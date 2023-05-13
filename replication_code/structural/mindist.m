
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

%%
% Part 1
% Read in Enemity and Alliship data
aminus = readtable('./regressions/A_minus.csv');
%aminus = readtable('./regressions/A_minus_sim.csv');

ammat = aminus{1:80, 2:81};
em = eig(ammat);

aplus = readtable('./regressions/A_plus.csv');
%aplus = readtable('./regressions/A_plus_sim.csv');

apmat = aplus{1:80, 2:81};
ap = eig(apmat);
apmatxx = eye(80)+apmat+ammat;
[p,q,r,s] = dmperm(apmatxx);


% Read in Main dataset
% cdata = readtable('./regressions/data_simulated.csv');
% cdata = readtable('./regressions/data_simulated_from_original_network.csv');
  cdata = readtable('./regressions/data_reduced_2010.csv');


  
%% Force beta to be zero
i=0;
beta=0;
  results = zeros(1,6);
    for gamma = -0.9:0.0011:0.9
        i=i+1;
        results(i,1)=beta;
        results(i,2)=gamma;

        % Simulate total fighting (tf_sim) with values of beta and gamma
        im = inv(eye(80)+beta.*apmat+gamma.*ammat);
        g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
        l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
        tf_sim = im*g.*l.*(1-l);
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(80)+x(1)*apmat+x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"}))).^2) ;

        % Find global minimum
        x0=[0.11,0.1];

        %% Genetic algorithm, other global optimizers, pattern search - use that as initial value of fminunc
        %% 10k times, 8k observations
        %gs = GlobalSearch;
        %problem = createOptimProblem('fmincon','x0',[0,0],...
         %   'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        %x1 = run(gs,problem);
        
        % Find local minimum around global minimum
        [x2,fval] = fminunc(fun,x0);
        disp(beta);
        disp(gamma);

        results(i,3)=fval;
        results(i,4)=x2(1);
        results(i,5)=x2(2);
        results(i,6)=mg;
    end

writematrix(results,'./regressions/matlab_simulation_onlyg.csv');





  
%% Force gamma to be zero
i=0;
gamma=0;
  results = zeros(1,6);
    for beta = -0.9:0.0011:0.9
        i=i+1;
        results(i,1)=beta;
        results(i,2)=gamma;

        % Simulate total fighting (tf_sim) with values of beta and gamma
        im = inv(eye(80)+beta.*apmat+gamma.*ammat);
        g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
        l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
        tf_sim = im*g.*l.*(1-l);
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(80)+x(1)*apmat+x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"}))).^2) ;

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

writematrix(results,'./regressions/matlab_simulation_onlyb.csv');





%% Simulate for whole range
i=0;
  results = zeros(1,6);
for beta = -0.9:0.0498:0.9
    for gamma = -0.9:0.051:0.9
        i=i+1;
        results(i,1)=beta;
        results(i,2)=gamma;

        % Simulate total fighting (tf_sim) with values of beta and gamma
        im = inv(eye(80)+beta.*apmat+gamma.*ammat);
        g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
        l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
        tf_sim = im*g.*l.*(1-l);
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(80)+x(1)*apmat+x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"}))).^2) ;

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

writematrix(results,'./regressions/matlab_simulation.csv');




%% Simulate in vicinity of results from paper


  i=0;
  results = zeros(1,6);
 % Loop over values of beta and gamma
for beta = -0.12:0.00101:-0.05
    for gamma = 0.08:0.00099:0.15
        i=i+1;
        results(i,1)=beta;
        results(i,2)=gamma;

        % Simulate total fighting (tf_sim) with values of beta and gamma
        im = inv(eye(80)+beta.*apmat+gamma.*ammat);
        g = (1./(1+beta*cdata{:,"degree_plus"}+gamma*cdata{:,"degree_minus"})); % g is correct
        l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}+gamma.*cdata{:,"degree_minus"})); % l is correct
        tf_sim = im*g.*l.*(1-l)+randn([80,1])*0.1;
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(80)+x(1)*apmat+x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}+x(2)*cdata{:,"degree_minus"}))).^2) ;

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

writematrix(results,'./regressions/matlab_simulation_loc.csv');


