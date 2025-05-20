function AS=asCorrFct(AS,brG)
% AS=asCorrFct(AS);
% estime les corrélations entre les variables des facteurs (groupes dans AS.VG(brB).Gr)
% et corrige les corrélations des variables pour devenir celles des groupes
% ce qu'on utilisera dans asCoplanaire.m
% Ce n'est que plus tard qu'on ramènera à 0 celles qui ne sont pas significatives
AS.VG(brG).CorEstim=[];
ng=numel(AS.VG(brG).Gr);
AS.VG(brG).CorFct=zeros(ng); % on remplira le triangle supérieur puis on complètera
AS.VG(brG).pCorFct=zeros(ng); % on remplira le triangle supérieur
AS.VG(brG).CorEstim=zeros(ng);
for j=1:ng-1
    for k=(j+1):ng  % pour chaque paire de facteurs (j,k)
        [r,pr,cc]=fctCor(AS,brG,j,k);
        AS.VG(brG).CorFct(j,k)=r;
        AS.VG(brG).pCorFct(j,k)=pr;
        AS.VG(brG).CorEstim(j,k)=cc;
    end
end
