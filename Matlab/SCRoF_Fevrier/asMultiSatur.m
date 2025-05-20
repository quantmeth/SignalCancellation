function [prob,Satur,Grp]=asMultiSatur(AS,brG,v,np)
% [prob,Satur,Grp]=asMultiSatur(AS,brG,v,np);  (Version décembre 2024)
% Trouve la meilleure annulation de la variable v par np (=2 ou 3) grappes
% de AS.VG(brG).Gr, excluant les grappes déclarées facteurs doublets
% Retourne la probabilité associée (la pire probabilité corrigée)
% de même que les np saturations sur les np grappes
% Vérifie d'abord si v donnait p>AS.seuils(1) pour au moins une autre
% variable. Si oui, la grappe de cette variable est toujours inclue dans Grp
% brG est le rang du scénario sur lequel travailler
nv=AS.nv;
rP=asPaireAvec(AS,v);
cr=min(AS.Crit(rP));
p=1-chi2cdf(cr,numel(AS.pertinent)-2);
if p>AS.seuils(1)
    vg=setdiff(AS.Cpaires(AS.Crit==cr,:),v);
    G=asGrappeDe(AS,brG,vg); % vide si la meilleure annulation par paire n'est pas dans une grappe 
else
    G=[];
end


var=AS.VG(brG).Var;   % variable représentative de chaque grappe, pour évaluation primaire
V=var(:)==AS.doublet(:)';
var(sum(V,2)>0)=[];   % sauf des grappes doublets
C=nchoosek(var,np); % tous les croisements de np variables représentant leurs facteurs
if ~isempty(G)      % sauf ceux ne contenant pas celle de la grappe G si G pas vide
    C(~any(C==var(G),2),:)=[];
end
nc=size(C,1);
Crit=zeros(nc,1);
Corr=zeros(nv,nc);
P=zeros(nc,np);
prob=zeros(nc,1);
dl=numel(AS.pertinent)-np;
for j=1:nc
    melange=[C(j,:),v];
    AS=asTuples(AS,melange);
    Crit(j)=AS.tmp.Crit;
    P(j,:)=AS.tmp.Poids;
    Corr(:,j)=AS.tmp.Corr;
    prob(j)=1-chi2cdf(Crit(j)*(AS.N-1),dl);
end
% ici, évaluer les annulations de signal
f=find(prob==max(prob),1);
if isempty(f) || prob(f)<AS.seuils(1) % solution d'annulation pas trouvée
    prob=prob(f);
    Satur=zeros(1,np);
    Grp=zeros(1,np);
    return
end
% Calculer tous les croisements des variables des groupes impliqués
% et utiliser la moyenne des saturations des croisements de prédicteurs
Grp=asGroupesDe(AS,brG,C(f,:));  % groupes des variables qui annulent le signal de reste(1)
try
    crois=croise(AS.VG(brG).Gr{Grp(1)}',AS.VG(brG).Gr{Grp(2)}');
    for c=3:numel(Grp)  % si plus que 2 prédicteurs
        crois=croise(crois,AS.VG(brG).Gr{Grp(c)}');
    end
    nt=size(crois,1);
    CritTuple=zeros(nt,1);
    saturTuple=zeros(nt,np);
    for f=1:nt
        melange=[crois(f,:) v];
        % if any(melange<0)
        %     keyboard
        % end
        AS=asTuples(AS,melange);
        CritTuple(f)=AS.tmp.Crit;
        Po=AS.tmp.Poids;
        % if any(abs(log10(Po))>10)
        %     break;
        % end
        saturTuple(f,:)=Po'.*sum(AS.VG(brG).Fct(melange(1:np),:),2)';
    end
    pp=max(CritTuple);
    prob=1-chi2cdf(pp*(AS.N-1),dl).^numel(CritTuple);
    Satur=mean(saturTuple);
catch AA
    keyboard
end
end
