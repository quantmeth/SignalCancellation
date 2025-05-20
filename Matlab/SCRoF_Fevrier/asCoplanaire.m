function AS=asCoplanaire(AS)
% AS=asCoplanaire(AS);
% si trois groupes n'occupent qu'un plan, en enlever un
for brG=1:numel(AS.VG)
    ng=numel(AS.VG(brG).Gr);
    if ng<3 % || (numel(AS.VG(1).Gr)
        continue
    end
    % AS.VG(brG).ProbCoplan=[];
    try % que toute erreur avorte le traitement de brG, vidant AS.VG(brG).Gr
        % AS.VG(brG).Ptr=[];
        % AS.VG(brG).Sat=[];
         AS.VG(brG).coplan=[];
        AS.VG(brG).GrCoplan=[];
        Gr=AS.VG(brG).Gr;
        ng=numel(Gr);
        if ng>2
            trios=nchoosek(1:ng,3);
            if ~isfield(AS,'Tuples') || numel(AS.Tuples)<3
                AS.Tuples{3}.melange=zeros(size(trios,1),1);
            end
            for tri=trios'  % pour chaque trio de groupes
                np=zeros(3,1);
                for k=1:3
                    np(k)=numel(Gr{tri(k)});
                end
                if all(np>2) % On ne teste que les trios dont au moins une grappe n'a que deux items
                    continue
                else
                    np=prod(np);
                    % Sat=zeros(np,3);
                    % Sat(:,3)=1;
                    % Ptr=zeros(np,3);
                    % Cri=zeros(np,1);
                    nt=0;
                    pire=0;
                    for i=1:numel(Gr{tri(1)})
                        for j=1:numel(Gr{tri(2)})
                            for k=1:numel(Gr{tri(3)})
                                nt=nt+1;
                                % Ptr(nt,:)=[i j k];
                                % melange=[Gr{tri(1)}(i) Gr{tri(3)}(j) Gr{tri(2)}(k)];
                                melange=[Gr{tri(1)}(i) Gr{tri(2)}(j) Gr{tri(3)}(k)];
                                if any(melange<0) % ignorer un groupe déjà à exclure
                                    pire=-1;
                                else
                                    AS=asTuples(AS,melange);
                                    cr=AS.tmp.Crit;
                                    if cr>pire   % sur l'ensemble des triplets de variables
                                        pire=cr;
                                    end
                                    % Sat(nt,1:2)=AS.tmp.Poids; % .Sat est mal nommé; ce sont des poids qu'il faudra pondérer par des saturatuions
                                    % Cri(nt)=cr;
                                end
                            end
                        end
                    end
                    if pire>0
                        pire=pire*(AS.N-1);        % pire est le plus grand de nt X2(nv-3)
                        pr=chi2cdf(pire,numel(AS.pertinent)-3);  % devient 1-pF
                        pr=1-pr.^nt;       % puis redevient p corrigé pour le maximum de nt valeurs indépendantes
                        if pr>AS.seuils(1) % && pr<AS.seuils(2)
                            % AS.VG(brG).GrCoplan{end+1}=Gr{g};
                            AS.VG(brG).coplan=[AS.VG(brG).coplan;[pr tri']];
                            % AS.VG(brG).Ptr{end+1}=Ptr;
                            % AS.VG(brG).Sat{end+1}=Sat;
                        end
                    end
                end
            end
        end
        AS=asValideCoplan(AS,brG);
        % if brG==1  % faire hériter le champ coplan aux suivant qui ont moins de grappes
        %     cop=AS.VG(1).coplan(:,2:end);
        %     cop=unique(cop(:));
        %     grap=[];
        %     for k=cop'
        %         grap=[grap,AS.VG(1).Gr{k}];
        %     end
        %     for k=2:numel(AS.VG)
        %         A=grap'==AS.VG(k).reste;
        %         if ~any(A(:)) && numel(AS.VG(k).Gr)<numel(AS.VG(1).Gr)
        %             AS.VG(k).coplan=AS.VG(1).coplan;
        %             % ajuster les rangs de grappes
        %             ote=setdiff(AS.VG(k).reste,AS.VG(1).reste);
        %             for j=1:numel(AS.VG(1).Gr)
        %                 if any(AS.VG(1).Gr{j}==ote(1)) % parce que pas forcément le même nombre de varables
        %                     f=find(AS.VG(k).coplan==j);
        %                     if ~isempty(f)
        %                         AS.VG(k).coplan(f)=[];
        %                         AS.VG(k).coplan(f:end)=AS.VG(k).coplan(f:end)-1;
        %                     end
        %                 end
        %             end
        %         end
        %     end
        % end
    catch AA
        AS.VG(brG).erreur=[AA.message,' dans as.Coplanaire'];
        AS.VG(brG).Gr={};
    end
end
end