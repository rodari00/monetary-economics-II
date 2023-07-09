%% Problem Set Monetary Economics II (ECON8862)
%  Name: Federico Rodari  
%  Date: 17-01-2023
%  Status: finished
%  Comments: used borrowed functions for a couple points.
%% 3) MODEL SOLUTION
clc; clear;
tic;

% Setup -------------------------------------------------------------------

%(b) Assign value to parameters

alpha=0.7;     % capital share
p=1.2;         %
delta=0.1;     % depreciation rate
r=0.04;        % real interest rate
eta_0=0.08;    % intercept of the dividend function
eta_1=0.028;   % slope of the dividend function
gamma_c=0.05;  % friction from adjustment costs
gamma_fc=0;    % friction from fixed costs
rho_z=0.8;     % persistence of technology
sigma_z=0.2;   % variance of technology

% (c) Create grid for technology (log(z)) using Tauchen's method
Ns = 11;
std=5;
[s_grid,P]=tauchen(Ns,0,rho_z,sigma_z,std); % s_grid (Ns x 1) disctete grid
                                            % P (Ns x Ns)     transiton matrix

% (d) Create grid for capital (endogenous state variable)
Nk = 100;
k_max =600;
k_grid = zeros(1,Nk);
k_grid(1) = k_max; 

for v = 2:Nk
    k_grid(v)=(1-delta)*k_grid(v-1); % compute capital dynamics (depletion)
end
k_grid = fliplr(k_grid)';
k_choice = k_grid';

% Value Function Iteration ------------------------------------------------

% Guess function
V_old=zeros(Ns,Nk);   % guess of value function matrix
V_ind=zeros(Ns,Nk);   % to index the choice of capital
V_new=zeros(Ns,Nk);   % value function matrix to be updated
k_prime=zeros(Ns,Nk); % policy function matrix
tol=10e-5;            % tolerance level
maxiter=1000;         % maximal number of iteration on VFI

% (e) Compute VFI using the standard discretization method (could use
%     interpolation)
iter=1; % initialize iteration
err=1;  % initialize error

while(err>tol) && (iter<maxiter)
    
    % loop over all possible states combinations and compute the updated
    % value function
    for zz=1:Ns % technology
        for kk=1:Nk % capital
            
        % compute profit function
        y=exp(s_grid(zz)).*k_grid(kk).^alpha; % output
        inv=p*k_choice-p*(1-delta)*k_grid(kk); % investment
        
        % average costs
        AC=gamma_c/2*k_grid(kk).*(inv./k_grid(kk).*p).^2+gamma_fc.*y.*double(k_choice~=k_grid(kk).*(1-delta));
        
        % cashflow
        cashflow=y-inv-AC;
        eqIC=(eta_0+eta_1*abs(cashflow)).*double(cashflow<0); % 'double' is a dummy indicator
        
        % dividends 
        div=cashflow-eqIC;
        
        RHS=div+(1/(1+r))*P(zz,:)*V_old;
        [V_new(zz,kk),V_ind(zz,kk)]=max(RHS);
        k_prime(zz,kk)=k_grid(V_ind(zz,kk));
        end

    end

    err=max(abs(V_new(:)-V_old(:)));

    if(mod(iter,50)==1)
        disp(['On VF iter' num2str(ss) '|V-Vold|=' num2str(err)])
    end

    V_old=V_new;
    iter=iter+1;
end

k_ind=V_ind;

% Plot policy function and value function
plot(k_grid,k_prime(6,:),'LineWidth',1)
hold on
plot(k_grid,k_grid)
xlabel('k','interpreter','latex')
ylabel('k','interpreter','latex')
%title('$\textrm{Policy Function}$','interpreter','latex')
grid on
hold off
saveas(gcf,'policy-function.eps','epsc')

plot(k_grid,V_new(6,:),'LineWidth',1)
xlabel('k','interpreter','latex')
ylabel('V','interpreter','latex')
%title('$\textrm{Value Function}$','interpreter','latex')
grid on
saveas(gcf,'value-function.eps','epsc')

%% 4) THE ERGODIC DISTRIBUTION OF THE MODEL
tol=1e-8;

% (g) Solve for the ergodic distribution mu
mu0=ones(Ns,Nk)/(Ns*Nk); % initialized distribution (uniform distribution)
mu1=zeros(Ns,Nk);
s_mu0=sum(sum(mu0)); %check that weights all add up to 1

% Setup convergence condition
while(err>tol) && (iter<maxiter)

    for z=1:Ns % technology
        for k=1:Nk % capital
            k_star=V_ind(z,k);
            mu1(:,k_star)=mu1(:,k_star)+mu0(z,k).*P(z,:)';
        end
    end

err=max(abs(mu1(:)-mu0(:))); % distributional error

mu0=mu1;
iter=iter+1;

end

mutot=sum(sum(mu1));

% (h) Plot the distribution over capital and three different states

plot(k_grid,mu1(6,:)/mutot,'LineWidth',1)
hold on
plot(k_grid,mu1(7,:)/mutot,'LineWidth',1)
plot(k_grid,mu1(5,:)/mutot,'LineWidth',1)
xlabel('k','interpreter','latex')
ylabel('$\mu(\bar{z},k)$','interpreter','latex')
%title('$\textrm{Ergodic distribution}$', 'interpreter','latex')
legend('$\textrm{Ss}$','$+1 \;\textrm{sd}$','$-1 \;\textrm{sd}$',  'interpreter','latex')
grid on
hold off
saveas(gcf,'ergodic-distribution.eps','epsc')

% (i.1) Compute average investment rate
inv_lev=zeros(Ns,Nk);
for i=1:Ns
    for j=1:Nk
       inv_lev(i,j)=p*k_prime(i,j)-p*(1-delta)*k_grid(j); 
    end
end
inv_tot=sum(sum(inv_lev));
k_tot=sum(k_grid);
irate=inv_tot/k_tot;

% (i.2) Compute volatility of investment
inv_var=sqrt(var(inv_lev(:)));

% (i.3) Compute average capital return
div_lev=zeros(Ns,Nk);
  for i=1:Ns
        for j=1:Nk     
            y=exp(s_grid(i)).*k_grid(j).^alpha;
            inv=p*k_prime(i,j)-p*(1-delta)*k_grid(j);
            AC=gamma_c/2*k_grid(j).*(inv./k_grid(j).*p).^2+gamma_fc.*y.*double(k_prime(i,j)~=k_grid(j).*(1-delta));
            cashflow=y-inv-AC;
            eqIC=(eta_0+eta_1*abs(cashflow)).*double(cashflow<0);
            div_lev(i,j)=cashflow-eqIC;
        end
  end
div_tot=sum(sum(div_lev));
k_tot=sum(k_grid);
krate=div_tot/k_tot;

disp(['The average investment rate is =' num2str(irate)])
disp(['The volatility of investment is =' num2str(inv_var)])
disp(['The average capital return is =' num2str(krate)])

%% 5) ESTIMATION

% I am using a built in function for generating the series

% (a-b-c) 
T=5000; % number of periods
Burn=100; % nomber of burnin observations
N=3000; % number of simulations

Z=[];
I=[];
K=[];
D=[];

for ss=1:N

    rng(ss)
    
    % Initialize the simulations
    sim_ind=zeros(T,1);
    sim_inv=zeros(T,1);
    sim_div=zeros(T,1);
    sim_y=zeros(T,1);
    
    % Shock to technology process
    z_start_ind=6; % start from s.s.
    sim_z_ind=shock(z_start_ind,P,T);
    sim_z=s_grid(sim_z_ind);
    
    % Endogenous process
    s_start_ind=1; 
    sim_ind(1)=s_start_ind;

    for t=2:T
        s_t=sim_ind(t-1);
        z_t=sim_z_ind(t-1);
    
        sim_ind(t)=k_ind(z_t,s_t);
    end
    
    sim_k=k_grid(sim_ind,1);
    
    for t=2:T-1
        z_v=sim_z(t);
        k_v=sim_k(t);
    
        k_prime_v=sim_k(t+1);
        
        y_v=exp(z_v)*k_v.^alpha;
        inv_v=p*k_prime_v-p*(1-delta)*k_v;
        AC=gamma_c/2*k_v*(inv_v/k_v).^2+gamma_fc.*y_v.*double(k_prime_v~=k_v.*(1-delta));
    
        cashflow=y_v-inv_v-AC;
        eqIC=(eta_0+eta_1*abs(cashflow)).*double(cashflow<0);
        div_v=cashflow-eqIC;
    
        sim_inv(t)=inv_v;
        sim_div(t)=div_v;
        sim_y(t)=y_v;
        
    end

% Store the results of the simulation (excluding the burnin)
Z(:,ss)=sim_z(Burn:(T-1));
I(:,ss)=sim_inv(Burn:(T-1));
K(:,ss)=sim_k(Burn:(T-1));
D(:,ss)=sim_div(Burn:(T-1));

end

% Plot the results  of the first simulation

% Technology
plot(exp(Z(:,1)),'LineWidth',0.5)
xlabel('$\textrm{Time}$','interpreter','latex')
ylabel('$\textrm{Capital}$','interpreter','latex')
saveas(gcf,'technology-dynamics.eps','epsc')

% Capital
plot(K(:,1),'LineWidth',0.5)
xlabel('$\textrm{Time}$','interpreter','latex')
ylabel('$\textrm{Capital}$','interpreter','latex')
saveas(gcf,'capital-dynamics.eps','epsc')

% Investment
plot(I(:,1),'LineWidth',0.5)
xlabel('$\textrm{Time}$','interpreter','latex')
ylabel('$\textrm{Investment}$','interpreter','latex')
saveas(gcf,'investment-dynamics.eps','epsc')

% Dividend
plot(D(:,1),'LineWidth',0.5)
xlabel('$\textrm{Time}$','interpreter','latex')
ylabel('$\textrm{Dividend}$','interpreter','latex')
saveas(gcf,'dividend-dynamics.eps','epsc')
%% Matching moments

% Compute the matching moments for each simulation (3000 x 3)
moment = [mean(I./K)',sqrt(var(I))',mean(D./K)'];

% Predetermined parameters
set=[p,r,eta_0,eta_1,gamma_fc]; 

% To-be-matched parameters
par=[gamma_c,alpha,delta];

% The target moments I need to match are (E(I/K),var(I),E(D/K))
% The NBER features the following target moments in the data
% E(I/K) =  0.061
% sqrt(Var(I)) = 100
% E(D/K) = 0.005

target=[0.061,100,0.005];

[mprod_grid,mk_grid]=ndgrid(s_grid,k_grid); % create a Ns x Nk grid
state_grid=[mprod_grid(:),mk_grid(:)];
Nstate=size(state_grid,1);

choice_grid=k_grid;
Nchoice=length(choice_grid);

[err,mom_model]=moment_matching(par,set,target,s_grid,k_grid,state_grid,choice_grid,P);

fun = @(x) moment_matching(x,set,target,s_grid,k_grid,state_grid,choice_grid,P);
options = optimoptions('fmincon','Display','iter');
lbnd = [0,0.5,0];
ubnd = [1.2,0.9,0.2];

% Run minimization
[est_param,err] = fmincon(fun,par,[],[],[],[],lbnd,ubnd,[],options);

% Get moments estimated
[err,moms] = fun(est_param);

disp(['The matched average investment rate moment is=' num2str(moms(1))])
disp(['The matched volatility of investment moment is=' num2str(moms(2))])
disp(['The matched average capital return moment is=' num2str(moms(3))])
disp(['The estimated gamma c is =' num2str(est_param(1))])
disp(['The estimated alpha is =' num2str(est_param(2))])
disp(['The estimated delta is =' num2str(est_param(3))])

toc;