function AS=asTuples(AS,k)
% AS=asTuples(AS,k);
% AS.GS(v,v) est la GrammSchmidt (en triangle supérieur) de R(v,v)
% Si k est un scalaire, optimise les variables de la rangée k de AS.Cpaire pour en minimiser le signal
% Autrement, optimise les valeurs de k(1:end-1) pour minimiser le signal dans k(end)
% Le champ AS.tmp retourne alors le critère, les corrélations avec les
% autres variables et les poids optimisés
% Les résultats des optimisations d'annulation sur trois variables ou plus sont entreposée dans AS.Tuples{np}
% pour ne pas avoir à refaire les optimisations quand redemandé
if ~isfield(AS,'pertinent')
    AS.pertinent=1:AS.nv;
end
cible=AS.pertinent;
if isscalar(k)
    melange=AS.Cpaires(k,:);
else
    melange=k;
    if isfield(AS,'Tuples')
        mel=rangMelange(AS,melange);
        if ~isempty(mel)
            nm=numel(melange);
            AS.tmp.Crit=AS.Tuples{nm}.cr(mel);
            AS.tmp.Corr=AS.Tuples{nm}.cor(mel,:)';
            AS.tmp.Poids=AS.Tuples{nm}.Po(mel,:)';
            return
        end
    else
        AS.Tuples{1}=[];
    end
end
mul=[.5 1 2];
nm=numel(mul);
cr=zeros(nm,1);
cor=zeros(AS.nv,nm);
Po=zeros(numel(melange)-1,nm);
mel=abs(melange);
P=AS.R(mel(1:end-1),mel(end));  % initialiser aux corrélations avec cible
for e=1:nm
    Po(:,e)=NM_AA(mul(e)*P,AS.GS,mel,cible); % relance l'optimisation à partir de la solution antérieure
    [~,cor(:,e)]=asCrit(Po(:,e),AS.GS,mel,cible);
    cr(e)=cor(:,e)'*cor(:,e); % bien qu'on ait minimisé la valeur absolue maximale des corrélations
end
try
    ma=max(abs(Po)); % parfois le meilleur critère a des poids excessifs
    f=find(ma<10);
    if isempty(f)
        f=find(ma==min(ma));
    else
        f=find(cr==min(cr(f)),1);
    end
catch
    keyboard
end
if numel(k)==1
    AS.Crit(k)=cr(f);
    AS.Corr(:,k)=cor(:,f);
    AS.Ppaires(k,:)=Po(:,f)';
else
    AS.tmp.Crit=cr(f);
    AS.tmp.Corr=cor(:,f);
    AS.tmp.Poids=Po(:,f);
    nm=numel(melange);
    if nm>numel(AS.Tuples)
        AS.Tuples{nm}.melange=0;
    end
    ptr=find(AS.Tuples{nm}.melange==0,1);
    if isempty(ptr)
        ptr=numel(AS.Tuples{nm}.melange)+1;
    end
    AS.Tuples{nm}.melange(ptr)=encodeMelange(AS,melange);
    AS.Tuples{nm}.cr(ptr)=cr(f);
    AS.Tuples{nm}.cor(ptr,:)=cor(:,f)';
    AS.Tuples{nm}.Po(ptr,:)=Po(:,f)';
end
end