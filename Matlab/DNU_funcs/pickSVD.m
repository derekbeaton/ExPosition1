function [u, v, d, tau] = pickSVD(datain,I,J)


k=min(I,J);
[V, S, U] = svd(datain,'econ');
d=diag(S);
V = V(:, 1:min(length(d), size(V,2)));
U = U(:, 1:min(length(d), size(U,2)));
keepem = find(d.^2 > 2 * eps);
tau = d(keepem).^2 / sum(d(keepem).^2);
%keepem =keepem(1:min(length(keepem), k));
u = V(:, keepem); % p in ExPos
v = U(:, keepem);
d = d(keepem);
end