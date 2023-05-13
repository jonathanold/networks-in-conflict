disp("Initialize Matlab");
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
  
  beta = -0.114;
  gamma = 0.083;
  
  disp("Start Monte Carlo Simulation.");
 % Loop over values of beta and gamma
    for runs = 1:1:1000
        i=i+1;
        if i/10-floor(i/10)==0
            disp(i);
        else if i<=20
                disp(i);
            end
        end
        results(i,1)=beta;
        results(i,2)=gamma;
        % Simulate total fighting (tf_sim) with values of beta and gamma
     
        im = inv(eye(8000)+beta.*kron(eye(100),apmat)+gamma.*kron(eye(100),ammat));
        g = (1./(1+beta*repmat(cdata{:,"degree_plus"},100,1)+gamma*repmat(cdata{:,"degree_minus"},100,1))); % g is correct
        l = 1-1/sum(1./(1+beta.*repmat(cdata{:,"degree_plus"},100,1)+gamma.*repmat(cdata{:,"degree_minus"},100,1))); % l is correct
        tf_sim = im*g.*l.*(1-l)+repmat(randn([80,1]),100,1);
    
        mg = min(g);
        % Define minimum distance function
        fun = @(x)sum((tf_sim-...
            (1-1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},100,1)+x(2)*repmat(cdata{:,"degree_minus"},100,1))))*...
            (  1/sum(1./(1+x(1)*repmat(cdata{:,"degree_plus"},100,1)+x(2)*repmat(cdata{:,"degree_minus"},100,1))))* ...
            inv(eye(8000)+x(1)*kron(eye(100),apmat)+x(2)*kron(eye(100),ammat))* ...
            (1./(1+x(1)*repmat(cdata{:,"degree_plus"},100,1)+x(2)*repmat(cdata{:,"degree_minus"},100,1)))).^2) ;

        % Find global minimum
        x0=[0.114,0.083];
        %gs = GlobalSearch;
        %problem = createOptimProblem('fmincon','x0',[0,0],...
         %   'objective',fun,'lb',[-0.99,-0.99],'ub',[0.99,0.99]);
        %x1 = run(gs,problem);
        % disp(i);
        % Find local minimum around global minimum
        options = optimoptions('fminunc',Display='off');
        [x2,fval] = fminunc(fun,x0,options);
        results(i,3)=fval;
        results(i,4)=x2(1);
        results(i,5)=x2(2);
        results(i,6)=mg;
    end
writematrix(results,'./regressions/matlab_simulation_1000runsPanel100.csv');
