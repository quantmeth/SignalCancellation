function AS=asBranches(AS)
% AS=asBranches(AS);
% Explore la structure d'arbre et ajoute dans AS le champ .scenario
% contenant les divers scénarios produits
branches=[];
for g=1:numel(AS.VG)
    if (numel(AS.VG(g).Gr)>0) % && (isempty(cp) || numel(cp{1})==2) % ignorer les scénarios avortés ou incorrects
        if isempty(AS.VG(g).FC(1).Fct)  % il vaudrait mieux comprendre de qui a occasionné cela
            AS.VG(g).FC(1)=[];
        else
            for c=1:numel(AS.VG(g).FC)
                Fct=AS.VG(g).FC(c).Fct;
                Co=AS.VG(g).FC(c).CorFct;
                f=size(Fct,2);
                if any(diag(Fct*Co*Fct')>.99)
                    f=-f;
                end
                branches=[branches;[AS.VG(g).FC(c).Fit g c f]];
            end
        end
    end
end
nb=size(branches,1);
% Il peut arriver que AS.VG(1) n'aie pas de coplanarité mais que AS.VG(>1)
% l'aie avec le même nombre maximum de grappes que AS.VG(1)
rG=0;   % rang de 1ere grappe coplanaire (nécessairement au maximum de grappes, je crois)
cpl=0;   % ?coplanaire
for b=1:nb   % en principe, AS.VG(1).Gr a le maximum de grappes
    if ~cpl & numel(AS.VG(branches(b,4)).coplan)>3
        if abs(branches(b,end))>=abs(branches(1,end))
            rG=b;
            break
        end
    end
end
if rG>1
    branches([1 rG],:)=branches([rG,1],:);
end
for b=nb:-1:2  % VG(1) restera toujours en premier
    if abs(branches(b,1))<AS.seuils(1)
        branches(b,:)=[];
    end
end
if isfield(AS.VG(1),'coplan') && ~isempty(AS.VG(1).coplan) && AS.VG(1).coplan(1,1)>AS.seuils(2)
    nf=abs(branches(1,end)); % max de grappes
    f=find(abs(branches(:,end))==nf);
    branches(f(2:end),:)=[];  % exclure les scénarios avec nf facteurs si au moins un trio coplanaire
end
AS.scenarios=branches;