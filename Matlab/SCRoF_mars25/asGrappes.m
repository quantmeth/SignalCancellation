function AS=asGrappes(AS)
% AS=asGrappes(AS);
% Calcule les distances (en X2)
% Effectue les regroupements (linkage)
% Établit le seuil de coupure en terme de probabilité (prob d'erreur de type I)
% du lien plutôt que celle du pire
% Détermine quels liens sont dans la fourchette AS.seuils(1)=.001 à AS.seuils(2)=.25
% et crée des branches en fonction de cela, enlevant les liaisons dans
% l'ordre inverse de leur formation
AS=asDistances(AS);
Z=linkage(AS.Dist,'complete');
nv=numel(AS.pertinent);
Gr{nv}=[];
for k=1:nv
    Gr{k}=AS.pertinent(k);
end
nZ=size(Z,1);
pr=zeros(nZ,1);
nz=0;
for k=1:nZ
    a=Gr{Z(k,1)};
    b=Gr{Z(k,2)};
    nx=numel(a)*numel(b);
    nz=nz+1;
    pr(k)=1-chi2cdf(Z(k,3),nv-2).^nx;  % probabilité ajustée
    Gr{end+1}=sort([a b]);
end
AS.GrBrut=Gr(nv+1:end);
AS.Z=[Z pr];
if max(pr)<AS.seuils(1)
    AS.VG(1).Gr=[];
    AS.VG(1).Creat='Initial (vide)';
    AS.VG(1).Parent=0;
    AS.VG(1).coplan=[];
    return
end
c=find(pr>AS.seuils(2),1,'last'); % garder d'abord toutes les agrégations considérées certaines
if isempty(c)
    c=-nv;
end
Gr0=Gr(1:(nv+c));
[AS.VG(1).Gr,AS.VG(1).reste]=asNettoie(Gr0); % réunir; crée souvent AS.VG(1)
if c>0
    AS.VG(1).Creat='Initial';
else
    AS.VG(1).Creat='Initial (vide)';
end
AS.VG(1).Parent=0;
% AS.VG(1).pCoupure=min(pr(pr>AS.seuils(1)));
f=find((pr>AS.seuils(1) & pr<AS.seuils(2)));
if isempty(f)
    return
end

% if any(Z(f,1:2)<=nv)  % si variable individuelle, vérifier si uni- ou multi-factorielle
%     [AS,f]=asAgregeOuReste(AS,f);
% end

% if any(f<0) % variable intégrée à une grappe
%     force=-f(f<0);
%     f(f<0)=[];
% else
%     force=[];
% end
ff=subsets(f);
ff=exclutSequentiels(Z,ff);
% if ~isempty(force)
%     for k=1:numel(ff)
%         for j=1:numel(ff{k})
%             ff{k}(j)=[ff{k}(j) force];
%         end
%     end
% end
ng=numel(AS.VG(1).Gr); % deviendra max nb de grappes
mg=1;                  % rang du scénario avec le max de grappes
for k=1:numel(ff)
    AS.VG(end+1)=AS.VG(1);
    AS.VG(end).LienInclus=Gr(ff{k}+nv);
    AS.VG(end).Creat='Grappes';
    AS.VG(end).Parent=1;
    Gr1=[Gr0(1,:),Gr(1,ff{k}+nv)];
    [AA,BB]=asNettoie(Gr1);
    if numel(AA)<AS.minFct && c>0
        AS.VG(end)=[];
    else
        AS.VG(end).Gr=AA;
        AS.VG(end).reste=BB;
    end
    if numel(AS.VG(end).Gr)>ng
        ng=numel(AS.VG(end).Gr);
        mg=numel(AS.VG);
    end
end
if mg>1
    AS.VG(mg).Parent=2;
end
o=[mg 1:numel(AS.VG)];
o(find(o==mg,1,'last'))=[];
AS.VG=AS.VG(o);
end
