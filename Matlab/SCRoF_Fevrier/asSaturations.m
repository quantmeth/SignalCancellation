function AS=asSaturations(AS,brG)
% AS=asSaturations(AS,brG);
ng=numel(AS.VG(brG).Gr);
AS.VG(brG).Var=zeros(ng,1);  % variable qui pourra représenter chaque facteur
for gr=1:ng
    var=sort(AS.VG(brG).Gr{gr});
    AS.VG(brG).Gr{gr}=var;  % s'assurer que les variables sont en ordre croissant pour les retrouver dans AS.Cpaires
    n=numel(var);
    satur=zeros(n-1,n);   % pour pouvoir examiner l'homogénéité des n-1 estimations de chaque variable
    rg=zeros(n,1);  % les rangs où écrire les saturations de chaque variable
    for j=1:n-1
        for k=j+1:n  % pour chaque paire des variables du facteur
            if var(j)<0 || var(k)<0
                break;   % ignorer une variable de rang rendu négatif (trouvée orpheline)
            end
            sat=asSatPaire(AS,var([j k]));
            if satur(1,j)~=0 && sign(sat(1))~=sign(satur(1,j))  % assurer polarités constantes
                sat=-sat;
            end
            rg([j k])=rg([j k])+1;
            satur(rg(j),j)=sat(1);
            satur(rg(k),k)=sat(2);
        end
    end
    if any(var<0)
        AS.pertinent(AS.pertinent==-var(var<0))=[];
    end
    if size(satur,1)>1
        satur=mean(satur);
    end
    if sum(satur)<0
        satur=-satur;
    end
    AS.VG(brG).Fct(AS.VG(brG).Gr{gr},gr)=satur';
    f=find(abs(satur)==max(abs(satur)),1); % désigner variable représentative
    f=var(f);
    AS.VG(brG).Var(gr)=f;
end