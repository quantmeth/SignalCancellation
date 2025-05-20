function AS=agregeParCorr(AS)
% AS=agregeParCorr(AS);
% examine les agrégations avec probabilités entre les deux seuils.
% Si deux grappes d'au moins deux variables ont une corrélation qui
% dépasse le seuil calculé par maxCorrFct(p), les scénarios avec ces deux
% grappes sont éliminés, sachant que leur agrégation est acceptée dans un
% autre scénario
AS.exception=[];
f=find((AS.Z(:,end)>AS.seuils(1)).*(AS.Z(:,end)<=AS.seuils(2)));
nv=numel(AS.pertinent);
ote=[];
for k=f(:)'
    if min(AS.Z(k,1:2))>nv
    % if numel(a)>1 && numel(b)>1
        a=AS.GrBrut{AS.Z(k,1)-nv};
        b=AS.GrBrut{AS.Z(k,2)-nv};
        ga=asGrappeDe(AS,1,a(1));   % remplace asGroupesDe le 2 mars 2025
        gb=asGrappeDe(AS,1,b(1));   % idem
        R=AS.R(a,b);
        F=AS.VG(1).Fct;
        P=F(a,ga)*F(b,gb)';
        r=P(:)\R(:);
        if r>maxCorrFct(AS.Z(k,end))
            AS.exception=[AS.exception,k,AS.Z(k,end),r];
            AS.Z(k,end)=AS.seuils(2)+.0001;
            ote=[ote,1];
            for j=2:numel(AS.VG)
                if asGrappeDe(AS,j,a(1))~=asGrappeDe(AS,j,b(1))  % encore ici aux deux places
                    ote=[ote,j];
                end
            end
        end
    end
end
AS.VG(unique(ote))=[];
