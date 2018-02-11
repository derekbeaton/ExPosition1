function [s,d,t,P,Q] = gensvd(X,M,W)
    
    
    [ret_M,m_vec,ret_W,w_vec] = makeMW4gsvd(M,W);
    
    if m_vec==1
        newX = repmat(ret_M,size(X,2),1)' .* X;
    else
        newX = ret_M * X;
    end

    if w_vec==1
        newX = newX .* repmat(ret_W,size(X,1),1);
    else
        newX = newX * ret_W;
    end
    
    %newX = repmat(sqrt(M),size(X,2),1)' .* X .* repmat(sqrt(W),size(X,1),1);
    
    %%TODO: do some tests to see if svd is as fast as eig(). Knowing
    %%matlab, it probably is.
    [P,s,Q] = svd(newX,'econ');
    s = diag(s);
    keepem = find(s.^2 > eps);
    
    
    if m_vec == 1
        P = repmat(1./ret_M,length(keepem),1)' .* P(:,keepem);
    else
        P = 1/ret_M * P(:,keepem);
    end
    
    if w_vec ==1
        Q = repmat(1./ret_W,length(keepem),1)' .* Q(:,keepem);
    else
        Q = 1/ret_W * Q(:,keepem);
    end
   
    s = s(keepem);
    d = s.^2;
    t = d/sum(d);

end



function [ret_M,m_vec,ret_W,w_vec] = makeMW4gsvd(M,W)
    
    m_vec=0;
    w_vec=0;
    %%Test if diagonal -- if they are, convert them to vectors.
    if(isdiag(M))
       M = diag(M); 
    end
    if(isdiag(W))
       W = diag(W);
    end
    
    %%Get Sizes.
    [I,A] = size(M);
    [J,B] = size(W);
    
    %%Easy case -- if either M or W are 1xN or Nx1, I deal with just the
    %%vector.
    if(sum([I,A] == 1))
        %%M is a vector
        if sum(M > 0)==length(M)
            ret_M = sqrt(M);
            m_vec = 1;
        else
            error('gensvd:masses','negative masses found');
        end
    end
    
    if(sum([J,B] == 1))
        if sum(W > 0)==length(W)
            ret_W = sqrt(W);
            w_vec = 1;
        else
            error('gensvd:masses','negative weights found');
        end
    end    
    
    %%if either are not already a vector
    if m_vec ~= 1
        [~,mp] = chol(M);
        if mp~=0
            error('gensvd:psd','Masses are not positive definite');
        else
            ret_M = sqrtm(M);
            m_vec=0;
        end
    end
    if w_vec ~= 1
        [~,mw] = chol(W);
        if mw~=0
            error('gensvd:psd','Weights are not positive definite');
        else
            ret_W = sqrtm(W);
            w_vec=0;
        end
    end
    
end