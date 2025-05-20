function AS=asDistances(AS)
% AS=asDdistances(AS);
pe=AS.pertinent;
Dist=zeros(AS.nv);
for k=1:numel(AS.Crit)
    Dist(AS.Cpaires(k,1),AS.Cpaires(k,2))=AS.Crit(k);
end 
AS.Dist=triU(Dist(pe,pe))';