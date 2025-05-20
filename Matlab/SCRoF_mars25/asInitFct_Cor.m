function AS=asInitFct_Cor(AS)
% AS=asInitFct_Cor(AS);
% initialise les champ .Fct et .Cor de chaque scénario à partir des
% variables s'annulant par paire
for brG=1:numel(AS.VG)
    ng=numel(AS.VG(brG).Gr); % pour le maximum de groupes
    AS.VG(brG).Fct=zeros(AS.nv,ng);
    % Estimer saturations des facteurs pour les variables unifactorielles
    AS=asSaturations(AS,brG);   % prépare aussi AS.Var: une variable par groupe
% quand pas de grappe avec p>seuils(2) il faut remplir .reste
    f=find(max(abs(AS.VG(brG).Fct),[],2)>0);
    AS.VG(brG).reste=setdiff(AS.pertinent,f');
    if numel(AS.VG(brG).Gr)>1
        AS=asCorrFct(AS,brG);
    else
        AS.VG(brG).CorFct=1;
        AS.VG(brG).CorEstim=1;
        % AS.VG(1).
    end
end