function [s,d,t,Fi,Fj,P,Q] = coreCA(X)

    %%a simple version of (stochastic) CA.
    colSums = sum(X);
    rowSums = sum(X,2);
    grandSum = sum(colSums);
    Ox = X/grandSum;
    m = rowSums/grandSum;
    w = (colSums/grandSum);
    
    Ex = m*w;
    Zx = Ox - Ex;

    M = (1./m)';
    W = 1./w;

    [s,d,t,P,Q] = gensvd(Zx,M,W);
    
    Fi = repmat(M,length(s),1)' .* P .* repmat(s',size(P,1),1);
    Fj = repmat(W,length(s),1)' .* Q .* repmat(s',size(Q,1),1);
    
end