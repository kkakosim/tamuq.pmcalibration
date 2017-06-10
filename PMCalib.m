function PMCalib()
load('aqCalib.mat');
Diam=[0.265 0.290 0.325 0.375 0.425 0.475 0.540 0.615 0.675 0.750 0.900 1.150 1.450 1.800 2.250 2.750 3.250 3.750 4.500 5.750 7.000 8.000 9.250]';
Dens=[2690 2690 2690 2690 2690 2690 2690 2690 2690 2690 2690 2860 2860 2860 2860 2305 2305 2305 2305 2305 2305 2305 2305]'; 
Dens=[2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340 2340]'; 


%PM1mod=aqCalibNUM(:,1:11)*Mass(1:11);
[D(3),err(3)]=getD(Dens(1:11),Diam(1:11),aqCalibNUM(:,1:11),aqCalibPM(:,3)); %PM1
[D(2),err(2)]=getD(Dens(12:15),Diam(12:15),aqCalibNUM(:,12:15),aqCalibPM(:,2)-aqCalibPM(:,3)); %PM2.5
[D(1),err(1)]=getD(Dens(16:23),Diam(16:23),aqCalibNUM(:,16:23),aqCalibPM(:,1)-aqCalibPM(:,2)); %PM10

Dens(1:11)=D(3);
Dens(12:15)=D(2);
Dens(16:23)=D(1);
Mass=((4*pi/3)*(Diam./2).^3).*Dens/1000000;

PM1mod=aqCalibNUM(:,1:11)*Mass(1:11);
PM25mod=aqCalibNUM(:,1:15)*Mass(1:15);
PM10mod=aqCalibNUM(:,1:23)*Mass(1:23);

plot(PM10mod,aqCalibPM(:,1),'o');
save('aqCalibMod.mat','PM1mod','PM25mod','PM10mod');

end
function fit=ObjFnc(Dens,Diam,PMnum,PMobs)
    Mass=((4*pi/3)*(Diam./2).^3).*Dens/1000000;
    PMmod=PMnum*Mass;
%     cost_func = 'NMSE';
%     fit = 1-goodnessOfFit(PMmod,PMobs,cost_func);
    cost_func = 'MSE';
    fit = goodnessOfFit(PMmod,PMobs,cost_func);
    %fit=sqrt(sum((PMobs-PMmod).^2/std(PMobs)^2));
end
function [D,err]=getD(Dens,Diam,NUM,PM)
    err=ObjFnc(Dens,Diam,NUM,PM);
    f = @(D)ObjFnc(D,Diam,NUM,PM);
    lb = ones(length(Dens),1)*1500;
    ub = ones(length(Dens),1)*3000;
    A = [];
    b = [];
    Aeq = [];
    beq = [];
    nonlcon = [];
    options = psoptimset('MaxIter',3000);
    [D,err]=patternsearch(f,2340,A,b,Aeq,beq,lb,ub,nonlcon,options);
end
function [idxo prtA]=randDivide(M,K) 
    [n,m]=size(M);
    np=(n-rem(n,K))/K;
    B=M;
    [c,idx]=sort(rand(n,1));
    C=M(idx,:);
    i=1;
    j=1;
    ptrA={};
    idxo={};
    n-mod(n,K)
    while i<n-mod(n,K)
        prtA{j}=C(i:i+np-1,:);
        idxo{i}=idx(i:i+np-1,1);
        i=i+np;
        j=j+1;
    end
    prtA{j}=C(i:n,:);
end 