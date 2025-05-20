function H=dendrGroupes(H,AS,rg)
% H=dendrGroupes(H,AS,rang);
% H contient un pointeurs à chaque ligne du dendrogramme
% rg est le rang du scénario à exprimer ou [rang,cor]
% Met en gras les lignes de groupes
% avec en gris pour les groupes devenus multivariés
Gr=AS.VG(rg(1)).Gr;
GrBrut=AS.GrBrut;
% nv=numel(AS.pertinent);
% Z=AS.Z(:,[1 2]);
for k=1:numel(Gr)
    H=dendrLarge(H,Gr{k},GrBrut);
end
if isfield(AS.VG(rg(1)),"GrCoplan") && ~isempty(AS.VG(rg(1)).GrCoplan)
    Cp=AS.VG(rg(1)).GrCoplan;
    for c=1:numel(Cp)
        grap=separeEnGrappes(AS,rg(1),Cp{c});
        for g=1:numel(grap)
            H=dendrLarge(H,grap{g}{:},GrBrut,.6);
        end
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

function grap=separeEnGrappes(AS,brG,var)
br=AS.VG(brG).Parent;
gr=asGrappeDe(AS,br,var);
for k=1:numel(gr)
    grap{k}=AS.VG(br).Gr(gr(k));
end

