function [err, mom_model] = moment_matching(par,set,target,s_grid,k_grid,state_grid,choice_grid,P)

Ns=length(s_grid);
Nk=length(k_grid);

gamma_c=par(1);
alpha=par(2);
delt=par(3);

p=set(1);
r=set(2);
eta_0=set(3);
eta_1=set(4);
gamma_fc=set(5);

% I now solve the model 

y = exp(state_grid(:,1)).*state_grid(:,2).^alpha;
inv = p*choice_grid'-p*(1-delt)*state_grid(:,2);
AC = gamma_c/2*state_grid(:,2).*(inv./state_grid(:,2)).^2+gamma_fc.*y;
cashFlow=y-inv-AC;
eqIC=(eta_0+eta_1*abs(cashFlow)).*double(cashFlow<0);
div=cashFlow-eqIC;
Vold=zeros(Ns,Nk);
tol=10e-5;
maxiter=1000;
iter=1;
vferr=1;
while (vferr>tol) && (iter<maxiter)
     EVold=repmat(1/(1+r)*P*Vold,[Nk,1]);
     RHS=div+EVold;
     [mVnew,idx]=max(RHS,[],2);
     V=reshape(mVnew,[Ns,Nk]);
     vferr=max(abs(V(:)-Vold(:)));
     Vold=V;
     iter=iter+1;    
end
mCapitalPrimeIdx=reshape(idx,[Ns,Nk]);

% I now simulate the model, for T=5000 and N=3000

T=5000;    
N=3000;   
Burn=100;

II = [];
KK = [];
DD = [];

for ss = 1:N
    z_start_ind=6;
    zsim_ind=shock(z_start_ind,P,T);
    zsim=s_grid(zsim_ind);
    statesim_ind = zeros(T,1);
    isim = zeros(T,1);
    dsim = zeros(T,1);
    ysim = zeros(T,1);
    stateinit = 1;
    statesim_ind(1) = stateinit;
    for t = 2:T
        state_t = statesim_ind(t-1);
        z_t = zsim_ind(t-1);
        statesim_ind(t) = mCapitalPrimeIdx(z_t, state_t);
    end
    ksim = k_grid(statesim_ind,1);
    for t = 2:T-1   
        zval = zsim(t);
        kval = ksim(t);
        kprimeval = ksim(t+1);
        ival = p*kprimeval - p*(1-delt)*kval;
        yval = exp(zval)*kval^alpha;
        AC = gamma_c/2 * kval * ((ival/p)/kval).^2;
        cashFlow=yval-ival-AC;
        eqIC=(eta_0+eta_1*abs(cashFlow)).*double(cashFlow<0);
        dval=cashFlow-eqIC;
        isim(t) = ival;
        dsim(t) = dval;
        ysim(t) = yval;
    end

    II(:,ss)=isim(Burn:(T-1));
    KK(:,ss)=ksim(Burn:(T-1));
    DD(:,ss)=dsim(Burn:(T-1));  
end

% I now match the moments

moment = [mean(II./KK)',sqrt(var(II))',mean(DD./KK)'];
mom_model = mean(moment);

err_mom = (target(:)-mom_model(:))./mom_model(:);
err=err_mom'*err_mom;
end