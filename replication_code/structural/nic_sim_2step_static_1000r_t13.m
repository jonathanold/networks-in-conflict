disp(pwd);
disp("Test");
    % Genetic algorithm, other global optimizers, pattern search - use that as initial value of fminunc
    % 10k times, 8k observations

disp("Starting Matlab...");

cd('/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Networks in Conflict/regressions')

clear ;
rng(12345);

%%
% Part 1
% Read in Enemity and Alliship data
aminus = readtable('./A_minus.csv');
ammat = aminus{1:80, 2:81};

aplus = readtable('./A_plus.csv');
apmat = aplus{1:80, 2:81};

% Read in Main dataset
  cdata = readtable('./data_reduced_2010.csv');

%% Simulate in vicinity of results from paper

  i=0;
  results = zeros(1,6);
  
  % beta is parameter for allies: Negative (because of complementarities/free-riding
  beta  = 0.114;
  % gamma is parameter for enemies
  gamma = 0.083;
  
  disp("Starting Monte-Carlo Simulation...");

 % Loop over values of beta and gamma
    for runs = 1:1:100
        for obs = 1:1:10
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
        im = inv(eye(80*13)+beta.*kron(eye(13),apmat)-gamma.*kron(eye(13),ammat));
        g = (1./(1+beta*repmat(cdata{:,"degree_plus"},13,1)-gamma*repmat(cdata{:,"degree_minus"},13,1))); % g is correct
        l = 1-1/sum(1./(1+beta.*repmat(cdata{:,"degree_plus"},13,1)-gamma.*repmat(cdata{:,"degree_minus"},13,1))); % l is correct
        tf_sim = im*g.*l.*(1-l)+randn([80*13,1])*0.1;
         
        

        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)-x(2)*repmat(cdata{:,"degree_minus"},13,1))))*...
            (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)-x(2)*repmat(cdata{:,"degree_minus"},13,1))))* ...
            inv(eye(80*13)+x(1)*kron(eye(13),apmat)-x(2)*kron(eye(13),ammat))* ...
            (1./(1+x(1)*repmat(cdata{:,"degree_plus"},13,1)-x(2)*repmat(cdata{:,"degree_minus"},13,1)))).^2) ;

                % Define minimum distance function

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
        lb=[0 0] ;
        nlcond=[];
        nonlcon=[];
        options = optimoptions('ga',Display='off');
         x1 = ga(fun,2,A,b,Aeq,beq,lb,ub,nlcond,options);
                                    
        % gs = GlobalSearch;
        % problem = createOptimProblem('fmincon','x0',[0,0],...
        %    'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        % x1 = run(gs,problem);
        % disp(i);
        
        % x1=[0,0];
        % Find local minimum around global minimum
        options = optimoptions('fmincon',Display='off');
        [x2,fval] = fmincon(fun,x1,A,b,Aeq,beq,lb,ub,nonlcon,options);
        results(i,3)=fval;
        results(i,4)=x2(1);
        results(i,5)=x2(2);
        results(i,6)=mg;
        disp(x2);
    end
    end
    %% Write results
writematrix(results,'./matlab_simulation_1000runsX13t_local_correct.csv');
disp("Done! ");