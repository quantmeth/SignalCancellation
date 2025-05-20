function AS=SCRoF(R,N,seuils)
% AS=SCRoF(R,N,seuils);    ou AS=SCFA(dat,seuils);
% seuils (défaut: [.001,.25]) est optionnel
% R peut être une matrice de covariances
% AS est une structure avec divers champs dont
% .R, .N, .GS, .Fct, .CorFct, .Gr, .Ppaires, .Cpaires
AS.seuils=[.001 .25]; % si on voulait les changer, c'est simle de le faire ici
AS.dat=R;
[n,nv]=size(R);
if n>nv  % matrice de données en entrée
    if nargin>1
        AS.seuils=N;
    end
    N=n;
    R=cov(R);
else
    if nargin>2
        AS.seuils=seuils;
    end
end
AS.seuils=sort(AS.seuils);
AS.et=sqrt(diag(R));  % ceci est destiné à pouvoir exprimer la solution factorielle en termes des variables d'origine
iet=1./AS.et;
AS.R=R.*(iet*iet');
if det(AS.R)<=0
    fprintf('\nLa matrice de corrélation n''a pas un déterminant positif.'\n);
    return
end
AS.N=N;
[~,AS.NEST,~,~,AS.PA]=NEST(AS.R,AS.N);
% AS.minFct=1;
AS.minFct=sum(AS.NEST<=AS.seuils(1));
fprintf('NEST indique au moins %d fct, suggère %d, AP_50_95 suggèrent %d %d fct',AS.minFct,sum(AS.NEST<.05),AS.PA)
AS.nv=numel(iet);
GS=chol(AS.R);
AS=asOrphelines(AS);
AS.GS=GS;
if ~isempty(AS.orphelines)
    f=AS.orphelines;
    AS.GS(:,f)=0; % pour rendre toutes les corrélations (sommes de produits) nulles sans mélanger les rangs des variables
end
AS.pertinent=setdiff(1:AS.nv,AS.orphelines);
AS=asPairesIndicatrices(AS);
AS=asGrappes(AS);  % défini aussi AS.reste
if ~isfield(AS,'VG') || isempty(AS.VG(1).Gr)
    fprintf('\nAucune annulation du signal par paire.\n')
    return
end
AS=asInitFct_Cor(AS); % initialiser les champs .Fct et .Cor de chaque scénario
if numel(AS.VG(1).Gr)<3
    AS.VG(1).coplan=[];
    AS.VG(1).GrCoplan=[];
end
AS=asCoplanaire(AS);
brG=0;
while brG<numel(AS.VG)
    brG=brG+1;
    if ~isempty(AS.VG(brG).coplan) && AS.VG(brG).coplan(1,1)<1
        AS=asGereCoplanaire(AS,brG);
    end
    if isempty(AS.VG(brG).coplan) || AS.VG(brG).coplan(1,1)>=1 || AS.VG(brG).coplan(1,1)<AS.seuils(2)
        AS=asGereReste(AS,brG); % si coplan vide ou rang de grappe ou prob. < .25
    end
end
AS=asTestCorrFct(AS);
AS=asBranches(AS);
if isfield(AS,'tmp')
    AS=rmfield(AS,'tmp');
end
if isfield(AS,'Tuples')
    for k=3:numel(AS.Tuples)
        AS.Tuples{k}.melange=decodeMelange(AS,AS.Tuples{k}.melange);
    end
end
SCRoFreport(AS);
% que faire si AS.reste n'est pas vide?
% asVariablesRestantes.m était destiné à cela et n'a pas été mis à jour
% (pour les branches entre autres)
