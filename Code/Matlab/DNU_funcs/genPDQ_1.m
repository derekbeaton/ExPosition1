function [p, q, Dv, Dd, ng, tau] = genPDQ(datain, M, W,I,J)

%[u , v, d, tau] = pickSVD(datain,I,J);
[s, d, tau, u, v] = gensvd(datain,M,W);


% now get data into PDQ
% variables are messed up in here, but the same values a in R's
P =u;d =d;D=diag(d);Q =v;tau=tau*100;
p=P;q=Q;Dv=d;Dd=D;ng=length(d);

end