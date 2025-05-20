function satur=saturations(AS,brG,grp,facteurs)
% satur=saturations(AS,brG,grp,facteurs);
% AS la structure en évolution, brG le rang de AS.VG qui contient la liste des grappes
% grp est un triplet ou quatrain coplanaire
% facteurs les rangs de ses deux grappes à garder comme facteurs
% Si facteurs est absent, ce sont les rangs des deux grappes donnés en
% positif
% calcule les saturtion des v variables des grappes autres que les deux de fct
% dans une matrice satur(v+1,3) avec [0 fct] en rangée 1 et [o var]' en colonne 1
if nargin<4
    facteurs=find(grp>0);
    grp=abs(grp);
end
if numel(facteurs)~=2
    error('Le paramètre ''facteurs'' doit être de longueur 2 OU le paramètre ''grp'' ne doit avoir que 2 rangs positifs');
end
fct=grp(facteurs);
gb=setdiff(grp,fct); % grappes bifactorielles (2 grappes pour un quatrain)
gb(gb==0)=[];
bifac=AS.VG(brG).Gr{gb(1)};
if numel(gb)>1
    bifac=[bifac AS.VG(brG).Gr{gb(2)}];
end
nv=numel(bifac); 
satur=zeros(nv+1,3);
satur(1,2:3)=facteurs;
satur(2:end,1)=bifac';
pred=croise(AS.VG(brG).Gr{fct(1)}',AS.VG(brG).Gr{fct(2)}');
np=size(pred,1);
% nv=numel(bifac);
for k=1:nv
    sat=zeros(np,2);
    for p=1:np
        % combine=sort([pred(p,:) bifac(k)]); % les résultats ont été entreposés par asTuples avec les grappes en ordre
        combine=[pred(p,:) bifac(k)]; % la grappe soupçonnée à rendre bifactorielle est en dernier %  1er mare 2025
        A=asTuples(AS,combine);
        c=find(combine==bifac(k)); % rang de cible
        p1=combine(combine==pred(p,1));  % rang du premier prédicteur
        p2=combine(combine==pred(p,2));  % rang du deuxième prédicteur
        sd=setdiff(1:3,c);
        st=[A.tmp.Poids', 1];
        st=st/st(c);
        if c<3
            st(:,3-c)=-st(:,3-c);
        end
        s1=AS.VG(brG).Fct(p1,fct(1)); %saturation pred 1 sur son facteur
        s2=AS.VG(brG).Fct(p2,fct(2)); %saturation pred 2 sur son facteur
        if s1+s2==0
            keyboard; % s1 ou s2 pas extrait correctement
        end
        sat(p,:)=st(:,sd).*[s1,s2];
    end
    satur(k+1,2:3)=mean(sat);
end