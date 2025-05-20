function AS=SubstitutionsDeSaturations(AS)
% AS=SubstitutionsDeSaturations(AS);
ngrp=size(AS.GRP,1);
satur{ngrp}=[];
for k=1:ngrp
    grp=AS.GRP(k,:);
    grp(grp==0)=[];
    signes=matSignes(numel(grp));
    ns=size(signes,1);
    sat{ns}=[];  % une liste parce que le nombre de lignes des matrices est le nb de variables +1
    for s=1:ns
        sat{s}=saturations(AS,1,grp.*signes(s,:));
    end
    satur{k}=sat;
end
AS.satur=satur;
