function H=dendrGroupes(H,AS,VG)
% H=dendrGroupes(H,AS,rang);
% H contient un pointeurs à chaque ligne du dendrogramme
% VG est le rang du scénario à exprimer ou [rang,cor]
% Met en gras les lignes de groupes
% avec en gris pour les groupes devenus multivariés
Gr=AS.VG(VG(1)).Gr;
GrBrut=AS.GrBrut;
% nv=numel(AS.pertinent);
% Z=AS.Z(:,[1 2]);
for k=1:numel(Gr)
    H=dendrLarge(H,Gr{k},GrBrut);
end
if isfield(AS.VG(VG(1)),"GrCoplan") && ~isempty(AS.VG(VG(1)).GrCoplan)
    Cp=AS.VG(VG(1)).GrCoplan;
    for c=1:numel(Cp)
        H=dendrLarge(H,Cp{c},GrBrut,.6);
    end
end

function H=dendrLarge(H,V,GB,c)
for k=1:numel(GB)
    OK=GB{k}==V';
    if any(OK(:))
        H(k).LineWidth=1.5;
        if nargin>3
            H(k).Color=c*[1 1 1];
            H(k).LineWidth=3.5;
        end
    end
    if numel(GB{k})==numel(V) && all(GB{k}==V)
        return
    end
end