function [p,X2,df]=asFit(AS)
% X2=asFit(AS);
% utilise les champs .pretinent, .R et .reprodR
% pour calculer le 'fit' de la solution
% qu'il faudra apprécier en différence avec une référence et en degrés de
% liberté
% AS.pertinent=1:AS.nv;  % temporairement ???
brG=AS.brancheG;
brC=AS.brancheC;
nv=numel(AS.pertinent);
R=AS.R(AS.pertinent,AS.pertinent);
S=AS.VG(brG).FC(brC).reprodR(AS.pertinent,AS.pertinent);
S=S+diag(1-diag(S));
if det(S)<=0
    X2=-1.0;
    df=0;
    p=1e-6;
    return
end
X2=(AS.N-1)*(log(det(S))-log(det(R))+trace(R/S)-nv);
if numel(AS.VG(brG).Gr)==1 
    co=0;
else 
    co=triU(AS.VG(brG).FC(brC).CorFct);
end
df=nv*(nv-1)/2-sum(AS.VG(brG).Fct(:)~=0)-sum(co~=0);
p=1-chi2cdf(X2,df);
