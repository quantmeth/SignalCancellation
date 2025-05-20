function [crit,corr]=as_Crit(p,G,combine,cible)
if iscell(G)
    combine=G{2};
    cible=G{3};
    G=G{1};
end
try
[cr,corr]=asCrit(p,G,combine,cible);
catch
    keyboard
end
% [cr,corr]=asCrit(p,varargin(:));
crit=-cr;