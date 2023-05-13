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
b = size(aminus);
c = b(1,1);
d = c+1;


aplus = readtable('./A_plus.csv');
apmat = aplus{1:80, 2:81};

% Read in Main dataset
  cdata = readtable('./data_reduced_2010.csv');

%% Simulate in vicinity of results from paper

  i=0;
  results = zeros(1,10);
  
  % beta is parameter for allies: Negative (because of complementarities/free-riding
  beta  = 0.114;
  % gamma is parameter for enemies
  gamma = 0.083;
  
  disp("Starting Monte-Carlo Simulation...");

 % Loop over values of beta and gamma
    for runs = 1:1:1000
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
        im = inv(eye(80)+beta.*apmat-gamma.*ammat);
        g = (1./(1+beta*cdata{:,"degree_plus"}-gamma*cdata{:,"degree_minus"})); % g is correct
        l = 1-1/sum(1./(1+beta.*cdata{:,"degree_plus"}-gamma.*cdata{:,"degree_minus"})); % l is correct
        tf_sim = im*g.*l.*(1-l)+randn([80,1])*0.1;
         
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"})))*...
            (  1/sum(1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"})))* ...
            inv(eye(80)+x(1)*apmat-x(2)*ammat)* ...
            (1./(1+x(1)*cdata{:,"degree_plus"}-x(2)*cdata{:,"degree_minus"}))).^2) ;

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
        % x1 = ga(fun,2,A,b,Aeq,beq,lb,ub,nlcond,options);
                                    
        % gs = GlobalSearch;
        % problem = createOptimProblem('fmincon','x0',[0,0],...
        %    'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        % x1 = run(gs,problem);
        % disp(i);
        
        % x1=[0,0];
        % Find local minimum around global minimum
        % options = optimoptions('fmincon',Display='off');
        % [x2,fval] = fmincon(fun,x1,A,b,Aeq,beq,lb,ub,nonlcon,options);



        results(i,3)=0.123 ;%fval;
        results(i,4)=0 ;%x2(1);
        results(i,5)=0 ;%x2(2);
        results(i,6)=0 ;%mg;

        x = [cdata{:,"degree_plus"},cdata{:,"degree_minus"}];
        y = tf_sim;
        mfun = @(b,x)((1-1/sum(1./(1+b(1)*x(:,1)-b(2)*x(:,2))))*(1/sum(1./(1+b(1)*x(:,1)-b(2)*x(:,2))))*inv(eye(c)+b(1)*apmat-b(2)*ammat)*(1./(1+b(1)*x(:,1)-b(2)*x(:,2))) );
        beta0 = [0 0];
        mdl = fitnlm(x,y,mfun,beta0);
        
        results(i,7)=mdl.Coefficients{1,1};
        results(i,8)=mdl.Coefficients{2,1};

         results(i,9)=mdl.Coefficients{1,2};
        results(i,10)=mdl.Coefficients{2,2};
    end
    end
    %% Write results
writematrix(results,'./matlab_simulation_1000runsX100t_local_correct_nlls.csv');
disp("Done! ");