function H=dendr(AS,rang)
% dendr(AS,rang);
% rang (défaut 1) est le rang du scénario (par ordre de mérite)
% produit le dendrogramme des regroupements à partir de paires dans SCRoF
% si rang(2) on utilise AS.VG(rang(1).FC(rang(2))
figure
if nargin<2
    rang=1;
end
if numel(rang)==2
    br=rang(1);
    co=rang(2);
else
    br=AS.scenarios(rang,4);
    co=AS.scenarios(rang,5);
end
Z=AS.Z;
Z(:,3)=sqrt(Z(:,3));
AxeY=sprintf('sqrt(X^2)');
H=dendrogram(Z);
for k=1:numel(H)  % remplacer les couleurs par noir
        H(k).Color=[0 0 0];
end
H=dendrGroupes(H,AS,[br,co]);
lb=xticklabels;
v=str2num(lb);
nv=AS.pertinent(v); 
% noms{numel(AS.pertinent)}='';  % réserver l'espace
for k=1:numel(nv)
    noms{k}=num2str(nv(k));
end
xticklabels(noms);
ylabel(AxeY);
fit=AS.VG(br).FC(co).Fit;
titre=sprintf('N=%d  X^2(%d)=%.2f p=%.3f',AS.N,fit([3,2,1]));
if isfield(AS,'titre')
    titre=[AS.titre '  ' titre];
else
    titre=[sprintf('(VG:%d FC:%d)',br,co) ' ' titre];
end
title(titre)