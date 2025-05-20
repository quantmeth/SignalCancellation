function AS=asValideCoplan(AS,brG)
% AS=asValideCoplan(AS,brG);
% à l'appel, AS.VG n'a qu'un niveau.
% Détecte s'il y a une incompatibilité entre les triplets coplanaires initiaux.
% Ce serait le partage de 2 grappes par seulement une paire de triplets.
% En tous les cas, on veut garder AS.VG(1) avec toutes ses grappes pour en
% avoir les corrélations.
% S'il arrive qu'un de deux triplets avec la même grappe a p>.25 et l'autre p<.25,
% on retire de AS.VG(1).coplan la ligne avec p<.25
% Si les deux lignes ont leur probabilité du même côté de .25, on ajoute
% deux VG avec chacun des plans, le plus haut p est joujours gardé, l'autre l'est séparément
% seulement si p du même côté de .25
% Si on a deux paires indépendantes de plans partageant chacune un facteur,
% on veut les croisements des deux paires indépendantes, chacune gardant
% ses deux plans ou pas selon les probabilités
coplan=AS.VG(brG).coplan; % VG(1) est fait de toutes les grappes comme autant de facteurs
nc=numel(coplan);
if nc<2
    return
end
s=AS.seuils(2);
cens=recens(coplan);
f=find(cens(:,2)==2);
if isempty(f)
    return
end
q=subsets(f,2,2);
for k=numel(q):-1:1 % ne garder que les paires ayant les deux mêmes grappes
    l1=lignesQuiOnt(coplan,cens(q{k}(1),1));
    l2=lignesQuiOnt(coplan,cens(q{k}(2),1));
    if any(l1~=l2)
        q(k)=[];
    end
end
for k=1:numel(q)
    f=q{k}(1);
    [L,P]=lignesQuiOnt(coplan,cens(f,1));
    g{k}=L;
    if P(2)<.25 && P(1)>.25
        g{k}(2)=[];
        AS.VG(brG).coplan(L(2),:)=[];
        % keyboard % exclure le triplet fautif de AS.VG(brG).coplan
    end
end
if numel(g)>1
    COPL=assemble(coplan,g);
% keyboard % n'effectuer la boucle que si coplan modifié dans COPL
    for j=1:numel(COPL)
        AS.VG(end+1)=AS.VG(brG);
        AS.VG(end).coplan=COPL{j};
    end
end
end

function [L,P]=lignesQuiOnt(coplan,gr)
% retourne les rangs de lignes (L) dans coplan qui contienne la grappe gr
% et les probabilités (P) associées à ces lignes
L=[];
P=[];
for k=1:size(coplan,1)
    if any(coplan(k,2:end)==gr)
        L=[L k];
        P=[P coplan(k,1)];
    end
end
end

function COPL=assemble(coplan,G)
% COPL=assemble(coplan,G);
% COPL sera un cell array de cell arrays combinant un élément de chaque
% coplan selon g quicontent un ou deux rangs dans coplan
for k=1:numel(G{end})
    COPL{k}=coplan(G{k},:);
end
for j=1:numel(G)-1
    deja=COPL;
    g=0;
    for k=1:numel(deja)
        for i=1:numel(G{j})
            g=g+1;
            COPL{g}={coplan(G{j},:) deja{k}};
        end
    end
end
end
