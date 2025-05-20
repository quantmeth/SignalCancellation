function AS=asOrphelines(AS)
% AS=asOrphelines(AS);
% utilise AS.R pour identifier ses colonnes dont la plus grande valeur
% absolue (excluant la diagonale) n'est pas significative (p>.01)
R=AS.R-eye(AS.nv);
mxR=max(abs(R));
z2=mxR.^2*(AS.N-1);
AS.pOrpheline=1-chi2cdf(z2,1).^(AS.nv-1);
% f=find((AS.pOrpheline>.01) + (mxR<.1));
f=find(AS.pOrpheline>AS.seuils(1));
AS.orphelines=f;
if ~isempty(f)
    AS.GS(:,f)=0; % pour rendre toutes les corrélations nulles sans mélanger les rangs des variables
end
AS.pertinent=setdiff(1:AS.nv,AS.orphelines);