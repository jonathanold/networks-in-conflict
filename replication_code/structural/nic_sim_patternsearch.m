disp(pwd);
disp("Test");
    % Genetic algorithm, other global optimizers, pattern search - use that as initial value of fminunc
    % 10k times, 8k observations

disp("Starting Matlab...");

cd('/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/')

clear ;
rng(12345);

%%
% Part 1
% Read in Enemity and Alliship data
aminus = readtable('./regressions/A_minus.csv');
ammat = aminus{1:80, 2:81};

aplus = readtable('./regressions/A_plus.csv');
apmat = aplus{1:80, 2:81};

% Read in Main dataset
  cdata = readtable('./regressions/data_reduced_2010.csv');

%% Simulate in vicinity of results from paper

  i=0;
  results = zeros(1,6);
  
  % beta is parameter for allies: Negative (because of complementarities/free-riding
  beta  = -0.114;
  % gamma is parameter for enemies
  gamma = 0.083;
  
  disp("Starting Monte-Carlo Simulation...");

 % Loop over values of beta and gamma
    for runs = 1:1:500
        for obs = 1:1:100
        i=i+1;
        if i/1000-floor(i/1000)==0
            disp(i);
        elseif i<=1000 && i/100-floor(i/100)==0
             disp(i);
        elseif i<=100 && i/10-floor(i/10)==0
             disp(i);   
        else 

        end
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
        x0=[0.0 , 0.0];
        
        % Constraint matrix:
        %A = [ 1, 0;
           %  -1, 0;
            %  0, 1;
             % 0,-1];
        A =[];
       % b = [1;1;1;1];
        b =[];
        Aeq=[];
        beq=[];
        ub=[0.999 0.999];
        lb=[-0.999 -0.999] ;
        nonlcon=[];

        options = optimoptions( 'patternsearch', Display=' off');
        x1 = patternsearch(fun,[0 0],A,b,Aeq,beq,lb,ub,nonlcon,options);                        
        % gs = GlobalSearch;
        % problem = createOptimProblem('fmincon','x0',[0,0],...
        %    'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        % x1 = run(gs,problem);
        % disp(i);
        
        
        % Find local minimum around global minimum
        options = optimoptions('fminunc',Display='off');
        [x2,fval] = fminunc(fun,x1,options);
        results(i,3)=fval;
        results(i,4)=x2(1);
        results(i,5)=x2(2);
        results(i,6)=mg;
    end
    end
    %% Write results
writematrix(results,'./regressions/matlab_simulation_1000runsX100t_patternsearch.csv');
disp("Done! ");