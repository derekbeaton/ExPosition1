%function [Fi, di, ri, ci, Fj, dj, rj, cj, t, d, s,M,W,P,Q] = corePCA(DATA,M,W)
%function [Fi,  Fj, t, d, s, P, Q] = corePCA(DATA,M,W,center,scale)
function [s,d,t,Fi,Fj,P,Q] = corePCA(DATA,M,W)

    % M and W aren't being used for now
    if ~exist('M'), M = ones(1, size(DATA,1)); end
    if ~exist('W'), W = ones(1, size(DATA,2)); end
%     if ~exist('center'), center = true; end
%     if ~exist('scale'), scale = true; end    

    % scaling data
    %%TODO: Replace with expo_scale(). The Matlab version can be more
    %%efficient than the R version.
    [DATA center scale]= zscore(DATA); % center and scale are not returned
    
    [s,d,t,P,Q] = gensvd(DATA, M, W);

    Fi = repmat(M,length(s),1)' .* P .* repmat(s',size(P,1),1);
    Fj = repmat(W,length(s),1)' .* Q .* repmat(s',size(Q,1),1);

    
%%TODO: a generalized function (a la additionals()) to do these steps.
%     di=sum(fi.^2,2);
%     ri=repmat((1./di),1,nf ) .* ( fi.^2);
%     ci=(fi.^2)./repmat(l',I,1);
% 
%     dj=sum(fj.^2,2);
%     rj=repmat((1./dj),1,nf ).*( fj.^2);
%     cj=(fj.^2)./repmat(l',J,1);

end