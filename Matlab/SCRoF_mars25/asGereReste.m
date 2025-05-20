function AS=asGereReste(AS,brG)
% AS=asGereReste(AS,brG);
% Cherche des annulations du signal des variables de AS.VG(brG).reste par les
% variables de 2 ou 3 grappes, avec préférence pour moins de prédicteurs si
% la différence est flagrante
if ~isfield(AS.VG(brG),'probMultiSatur')
    AS.VG(brG).probMultiSatur=[];
end
reste=AS.VG(brG).reste;
nr=numel(reste);
for r=nr:-1:1    % pour chaque variable à expliquer
    prob=zeros(3,1);
    Satur{3}=[];
    Grp{3}=[];
    v=reste(r);
    sol=zeros(2,1);
    for np=2:min(3,numel(AS.VG(brG).Gr))    % pour 2 et 3 prédicteurs
        [prob(np),Satur{np},Grp{np}]=asMultiSatur(AS,brG,v,np);
        if prob(2)>AS.seuils(2)
            break    % pas besoin d'estimer avec 3 prodicteurs
        end
    end
    if prob(2)>AS.seuils(1)
        sol(1)=2;
    end
    if prob(3)>max(prob(2),AS.seuils(1))
        sol(sol==0)=3; % pourrait mettre 3 sans les deux entrées de sol
    end
    if sol(1)==0
        AS.VG(brG).probMultiSatur=[AS.VG(brG).probMultiSatur;v max(prob)];
        continue % on n'a pas trouvé de solution pour cette variable
    end
    reste(r)=[];
    AS.VG(brG).reste=reste;
    Fct=AS.VG(brG).Fct;
    Fct(v,Grp{sol(1)})=Satur{sol(1)};
    if sol(2)~=0 && sol(2)~=sol(1)
        AS.VG(end+1)=AS.VG(brG); % copier avant de modifier .Fct
        AS.VG(end).Fct(v,Grp{sol(2)})=Satur{sol(2)};
        AS.VG(end).Creat='MultiSatur';
        AS.VG(end).Parent=brG;
        AS.VG(end).probMultiSatur=[AS.VG(end).probMultiSatur;v prob(sol(2))];
    end
    AS.VG(brG).Fct=Fct;
    AS.VG(brG).probMultiSatur=[AS.VG(brG).probMultiSatur;v prob(sol(1))];
end
