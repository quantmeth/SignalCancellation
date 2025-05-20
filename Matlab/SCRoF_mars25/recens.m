function cens=recens(coplan)
% cens=recens(coplan);
% coplan est une matrice (k,4) avec la probabilité assocée à la coplanarité
% du trio en colonne 1, ou (k,3) sans cette probabilité
% si de longueur 4, le premier item, une probabilité, est ignoré
% les 0 dans coplan ne sont pas recensés
A=coplan;
if any(A(:,1)<1)
    A=A(:,2:end);
end
A=A(:);
g=unique(A);
g(g==0)=[];
cens=[g,g];
for k=1:numel(g)
    cens(k,2)=sum(A==g(k));
end
