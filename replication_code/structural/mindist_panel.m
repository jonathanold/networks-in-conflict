
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

%% Simulate in vicinity of results from paper


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




