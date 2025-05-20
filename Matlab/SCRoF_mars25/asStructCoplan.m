function [SCEN,GR]=asStructCoplan(AS,brG)
% [SCEN,GRP]=asStructCoplan(AS,brG);
% Prépare la production de toutes les structures de scènes découlant de AS.VG(brG).coplan
% SCEN{ng} (k,4) contient les rangs de grappes formant des triplets ou des quatrains.
% SCEN et GR sont dans AS polutôt que AS.VG() pour des raisons historiques.
% Des triplets mutuellement incompatibles sont dans des SCEN{} distincts.
% Les probabiltés dans la colonne 1 de coplan sont déjà prises en compte.
% GR est la liste des groupements
% Ces structures sont à décliner selon les paires de grappes désignées comme facteurs.

% if isfield(AS,'GRP')
%     error('Il ''est pas prévu de devoir modifier AS.SCEN et AS.GRP après AS.VG(1)');
% end
seuil=AS.seuils(2); % AS.seuils(1) est toujours dépassé si coplan existe
coplan=AS.VG(brG).coplan;
coPlus=classeTriplets(coplan); % ajoute code de restriction aux triplets
coP=coPlus;  % servira à produire le GRP à retourner
% FCT=facteursCoplan(coPlus);
GRP=[];
[q,codes]=rangXk(coPlus,4); % identifier les quatrains s'il y en a
% d'abord mettre dans GRP les quatrains s'il y en a
for k=q(:)'
    gr=unique(coPlus(coPlus(:,end)==codes(k),2:4));
    GRP=[GRP;[.5 gr(:)']];
end
% Ensuite, ajouter tous les triplets
lGRP=size(GRP,1);
p=find(mod(coP(:,end),10)<4);
% coP(p,end)=0;
GRP=[GRP;coP(p,:)];
GR=GRP(:,2:end);  % en garder une copie pour la sortie, sans les probabilités associées
GR(lGRP+1:end,end)=0;
%  gérer les triplets incompatibles (enlever p<.25 si autre a p>.25)
q=rangXk(GRP,3); % paires incompatibles
incomp=[];
for k=q(:)'
    t=find(GRP(:,end)==codes(k));
    p=GRP(t,1);
    if sum(p>.25)==1
        GRP(t(1),end)=1;    % celui qu'on garde
        % FCT=setdiff(FCT,coPlus(t(2),2:4));
        GRP(t(2),:)=[];     % celui qu'on ne garde pas
        GR(t(2),:)=[];
    else
        incomp=[incomp,t];  % On les gardera mais chacun doublera l'autre
    end
end
% Réduire GRP aux groupes (quatrains ou triplet) avec p>seuil
p=find(GRP(:,1)<seuil);
GRP(p,:)=[];
codSur=1:size(GRP,1);  % vide si aucun p>.25
for k=1:size(incomp,2)
    codSur(codSur==incomp(2,k))=[]; % pointeur à seulement le premier de deux triplets incompatibles parmi les p>.25
end
% fabriquer SCEN
% D'abord ne pointer qu'au premier de deux triplets incompatibles, aussi
% pour les sous-ensembles à ajouter
for k=1:size(incomp,2)
    p(p==incomp(2,k))=[];
end
SS=subsets(p);
SCEN{1}=codSur;
if ~isempty(SS{1})
    for k=1:numel(SS)
        SCEN{end+1}=[SCEN{1} SS{k}];
    end
end
if isempty(SCEN{1}),SCEN(1)=[];end
% dédoubler les scènes avec triplets incompatibles
if ~isempty(incomp)
    for k=1:numel(SCEN)
        A=SCEN{k}'==incomp(1,:);
        if any(A(:))
            SCEN{end+1}=SCEN{k};
            f=find(sum(A,2)>0);
            g=find(sum(A,1)>0);
            SCEN{end}(f)=incomp(2,g);
        end
    end
end
end

function [rg,codes]=rangXk(mat,k)
% retourne les rangs des lignes de mat dont le dernier élément modulo 10 est k
codes=unique(mat(:,end));
rg=find(mod(codes,10)==k);
end