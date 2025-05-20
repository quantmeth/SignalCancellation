function [crit,corr]=asCrit(p,G,combine,cible)
% [crit,corr]=asCrit(p,G,combine,cible);
% p(n) est un vecteur à optimiser en y accolant un -1.0 fixe
% G(v,?) est une matrice en triangle supérieur avec les variables en colonnes
% (les axes ou les cas en rangées), typiquement un GrammSchmidt de la
% matrice de corrélations.
% combine(n+1) donne les rangs des variables à combiner pour en annuler le signal
% cible est une liste de variables cibles (défaut: toutes)
% crit est le plus grand carré des corrélationa de SPo avec les variables cibles
% corr(1,v), si présent, contient les corrélations des v variables avec SPo
if nargin<4
    v=size(G,2);
    cible=setdiff(1:v,combine);
end
pp=[p(:);-1];
SP=G(:,combine)*pp;
SP=SP./sqrt(SP'*SP); 
corr=SP'*G;   % corr(1,v)
corr(setdiff(1:numel(corr),setdiff(cible,combine)))=0;
crit=max(corr.^2);
if ~isreal(crit)
    crit=99e9;
end