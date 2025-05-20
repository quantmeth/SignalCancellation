function AS=asGereCoplanaire(AS,brG)
% AS=asGereCoplanaire(AS,brG);
% À l'entrée, AS.VG(brG) contient les champs Gr et coplan.
% Le rôle de la gestion des grappes trouvées coplanaires par triplet est de
% corriger dans les divers scénarios ajoutés résultant de ces coplanarités les
% champs de AS.VG(*) concernés. Cela implique de remplacer dans le champ Fct
% les saturations des variables devenant bifactorielles sur leur grappe d'origine
% par des saturations sur les deux grappes retenues comme facteurs pour le plan,
% de retirer les grappes bifactorielles du champ Gr et des matrices liées aux corrélations
% entre facteurs (lignes et colonnes correspondant aux grappes bifactorielles).
%
% Après le travail effectué par cette fonction, il reste, pour chaque scénario à
% établir les saturations des variables pas encore expliquées et à leur associer
% diverses possibilités de patrons de corrélations.
%
% Met d'abord les triplets coplanaires par ordre décroissant de probabilité
% et gère les cas mutuellement incompatibles.
% Pour tout regroupement de variable ayant au moins un groupe coplanaire
% établit la structure des scènes, répartit les scènes dans chacune de leur
% déclinaisons avec deux grappes à garder comme facteurs, ajoute les
% saturations bifactorielles des variables des grappes déclassées et enlève
% les parties de matrices de saturations ou de correlations qui
% correspondent à ces grappes déclasées.
% Enlève aussi de AS.VG{brG].Gr les grappes maintenant exclues

if ~isfield(AS,'GRP')
    [AS.SCEN,AS.GRP]=asStructCoplan(AS,brG);
    %     brG=1;
    % else
    %     brG=numel(AS.VG);  % nécessairement le dernier? C'est à vérifier
end
AS.declin=asCoplanDeclinaisons(AS,brG);
AS=SubstitutionsDeSaturations(AS);
for sc=1:numel(AS.SCEN)  % pour chaque cellule de SCEN et de declin
    plans=AS.GRP(AS.SCEN{sc},:); % les plans visée par cette scène
    declin=AS.declin{sc};        % ses déclinaisons
    for de=1:size(declin,1)  % pour chaque déclinaison
        AS.VG(end+1)=AS.VG(brG);  % initialiser un nouveau scénario
        AS.VG(end).Creat='Coplan';
        AS.VG(end).Parent=brG;
        grBifact=[];                % liste des grappes déclassées
        varBifact=[];               % liste des variables déclassées
        for p=1:size(plans,1)  % pour chaque plan de la scène sc
            plan=plans(p,:);
            plan(plan==0)=[];
            % for s=1:numel(AS.satur{sc})  % on veut le satur pour la ligne courante de AS.GRP
                % subs=AS.satur{sc}{declin(de)};
                subs=AS.satur{sc}{de};
                facteurs=plan(subs(1,2:3));
                variables=subs(2:end,1)';
                grBifact=[grBifact setdiff(plan,facteurs)];
                varBifact=[varBifact variables];
                AS.VG(end).Fct(variables,facteurs)=subs(2:end,2:3);
            % end
        end
        AS.VG(end).coplan=grBifact;
        AS.VG(end).Var(grBifact)=[];   % me semble dangereux, présumant du contenu de .Var
        if isfield(AS.VG(end),'GrCoplan')
            AS.VG(end).GrCoplan{end+1}=varBifact;
            % AS.VG(end).GrCoplan{end+1}=variables;
        else
            AS.VG(end).GrCoplan{1}=varBifact;
            % AS.VG(end).GrCoplan{1}=variables;
        end
        AS.VG(end).Gr(grBifact)=[];
        AS.VG(end).Fct(:,grBifact)=[];
        % champs={'CorEstim','CorBrEstim','CorFct','CorBrFct','pCorFct'}; % 2 premiers autrefois de type cell
        champs={'CorFct','CorEstim','pCorFct'};
        for c=1:numel(champs)
            Q=AS.VG(end).(champs{c});
            Q(:,grBifact)=[];
            Q(grBifact,:)=[];
            AS.VG(end).(champs{c})=Q;
        end
    end
end


%
% signes=matSignes(3+plans(g,end)>0); % matrice de signes (3,3) ou (6,4)
% grp=plans(g,:);    % lire le plan et enlever son 0 s'ily a lieu
% grp(grp==0)=[];    % pour avoir même nb de colonnes que signes
% rg=find(signes(declin(de,g),:)<0);
% grappes=grp(rg);   % on en a 2 pour un quatrain
% grBifact=[grBifact grappes];
% for k=1:numel(grappes)
%     varBifact=[varBifact AS.VG(brG).Gr{grappes(k)}];
% end
% subs=AS.satur{AS.SCEN{sc}}{de};
% AS.VG{brG}.Fct()
%
% var=AS.VG(brG).Gr{plans(g,signes(g,:)<0)};
% rendu ici, on pourrait ajouter dans AS.VG(brG).Fct les saturations des variables
%


% AS=asOrdonneCoplan(AS,1);
% AS=asValideCoplan(AS,1);
% maxGr=numel(AS.VG(1).Gr);
% coplan=AS.VG(1).coplan;
% Liste=asStructureCoplan(coplan);
%
% cens=recens(AS.VG(brG).coplan);
%         if sum(cens(:,2)==3)==4
%             quat=unique(AS.VG(brG).coplan(:,2:end))';
%             AS.VG(brG).coplan=[1 quat];
%             quat=[quat 1];
%             ss=subsets(1:4,2,2);
%             scen=zeros(6,5);
%             for k=1:6
%                 scen(k,:)=quat;
%                 scen(k,ss{k})=-scen(k,ss{k});
%             end
%         else
%             scen=asGrappesScenarios(AS);  % les rangs des grappes à transformer en variables multifactorielles sont en négatif
%         end
%
% for brG=1:numel(AS.VG)  % en principe VG(1) a toutes ses grappes et donc on n'a pas à y gérer de coplanarité
%     % mais si on commence à brG=2, brG grandit sans fin !!!
%     % if numel(AS.VG(brG).Gr)<maxGr
%     %     continue
%     % end
%     if isempty(AS.VG(brG).coplan)
%         return
%     end
%     if brG==1
%         cens=recens(AS.VG(brG).coplan);
%         if sum(cens(:,2)==3)==4
%             quat=unique(AS.VG(brG).coplan(:,2:end))';
%             AS.VG(brG).coplan=[1 quat];
%             quat=[quat 1];
%             ss=subsets(1:4,2,2);
%             scen=zeros(6,5);
%             for k=1:6
%                 scen(k,:)=quat;
%                 scen(k,ss{k})=-scen(k,ss{k});
%             end
%         else
%             scen=asGrappesScenarios(AS);  % les rangs des grappes à transformer en variables multifactorielles sont en négatif
%         end
%         AS.VG(brG).scen=scen;
%         % nSc=numel(AS.VG);  % pour commencer à modifier seulement les AS.VG ajoutés ici
%         for k=1:size(scen,1)
%             AS.VG(end+1)=AS.VG(brG);
%             AS.VG(end).scen=scen(k,:);
%         end
%         return
%     end
%     % pour chaque 4 entrées de scen calculer les saturations des grappes nommées en négatif
%     % for s=1:size(scen,1)
%     % if brG==1
%     %     continue
%     % end
%     scen=AS.VG(brG).scen;
%     for j=2:4:numel(scen)
%         tri=scen(j:j+2);
%         cible=-tri(tri<0);
%         fct=tri(tri>0);
%         va=AS.VG(brG).Gr{cible};
%         satur=saturations(AS,brG,fct,cible);
%         AS.VG(brG).Fct(va,fc)=satur;
%     end
% end
% for sc=nSc+1:numel(AS.VG)
%     grp=find(AS.VG(sc).scen<0);
%     if ~isempty(grp)
%         AS.VG(sc).GrCoplan=AS.VG(sc).Gr(grp);
%         AS.VG(sc).Gr(grp)=[];
%         AS.VG(sc).Var(grp)=[];
%         AS.VG(sc).Creat='Coplan';
%         AS.VG(sc).Parent=scen(end);
%         AS.VG(sc).Fct(:,grp)=[];
%         AS.VG(sc).CorFct(grp,:)=[];
%         AS.VG(sc).CorFct(:,grp)=[];
%         AS.VG(sc).CorEstim(grp,:)=[];
%         AS.VG(sc).CorEstim(:,grp)=[];
%         AS.VG(sc).pCorFct(grp,:)=[];
%         AS.VG(sc).pCorFct(:,grp)=[];
%     end
% end
% end
