function AS=asVariablesMulti(AS)
% AS=asVariablesMulti(AS);
% Utilise une variable de chaque groupe (facteur) pour expliquer (par
% annulation du signal) les variables dans AS.reste
brG=0;
while brG<numel(AS.VG)
    brG=brG+1;
    % if brG>1 && numel(AS.VG(brG).Gr)==numel(AS.VG(1).Gr)
    %     continue   % pour voir si ça évite le problème
    % end
    ng=numel(AS.VG(brG).Gr)-size(AS.doublet,1);
    if ng==0
        continue    % ne pas traiter un scénario avorté
    end
    try             % avroter le scénario en cas d'erreur
        if ng<2
            return
        end
        for np=2:min(ng,3)   % nombre de prédicteurs
            if isempty(AS.VG(brG).reste)
                break
            else
                while ~isempty(AS.VG(brG).reste) && max(AS.VG(brG).reste)>0
                    AS=asMultiSatur(AS,brG,np);
                    while ~isempty(AS.VG(brG).reste) && AS.VG(brG).reste(1)<0 && any(AS.VG(brG).reste>0)
                        AS.VG(brG).reste=[AS.VG(brG).reste(2:end) AS.VG(brG).reste(1)];
                    end
                end
            end
            AS.VG(brG).reste=abs(AS.VG(brG).reste); % remettre en positifs les variables pas résolues avec le np courant
        end
    catch AA
        AS.VG(brG).erreur=[AA.message,' dans as.VariablesMulti'];
        AS.VG(brG).Gr={};
    end
end