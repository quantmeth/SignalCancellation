function AS=asPairesIndicatrices(AS)
% AS=asPairesIndicatrices(AS);
% Identifier les variables indicatrices, s'il y en a pour la branche AS.branche
AS.Cpaires=nchoosek(AS.pertinent,2); % tous les sous-ensembes de 2 parmi les v
nc=size(AS.Cpaires,1);
AS.Crit=zeros(nc,1);
AS.Corr=zeros(AS.nv,nc);
AS.Ppaires=zeros(nc,1);
AS.doublet=[];
for k=1:nc
    AS=asAnnulePaire(AS,k);  % optimiser les variables de la rangée k de AS.Cpaires
end
AS.z2=(AS.Corr).^2*(AS.N-1);