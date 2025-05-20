function AS=asTestCorrFct(AS)
% AS=asTestCorrFct(AS);
% Annule les corrélations non significatives et produit des
% branches pour toutes les corrélations entre les deux seuils
% Enlève d'abord les tests et corrélations des groupements exclus pour coplanarité
if isempty(AS.VG(1).Gr)
    return
end
pe=AS.pertinent;
R=AS.R(pe,pe);
for brG=1:numel(AS.VG)
    if isempty(AS.VG(brG).Gr)
        continue
    end
    if ~isempty(AS.doublet)
        pex=setdiff(pe,AS.doublet(:));
    end
    Fct=AS.VG(brG).Fct(pe,:);
    FctX=Fct;
    Db=AS.doublet(:);
    D=Fct(Db,:);
    FctX(Db,:)=0;
    FctX(FctX~=0)=2;
    FctX(Db,:)=D;
    ng=numel(AS.VG(brG).Gr);
    if ng==0
        continue    % ne pas traiter un scénario avorté
    end
    try             % avorter le scénario en cas d'erreur
        if ng==1
            CORR=0;
            pr=1;
        else
            [pr,rg]=triU(AS.VG(brG).pCorFct);
            pr=correctionFDR(pr); % Correction de False Discovery Rate sur les tests des corrélations
            for k=1:numel(pr)
                AS.VG(brG).pCorFct(rg(k,1),rg(k,2))=pr(k);
            end
            f=find(pr>AS.seuils(2));  % annuler les corrélations avec p>seuils(2)
            if ~isempty(f)
                for i=f'
                    AS.VG(brG).CorFct(rg(i,1),rg(i,2))=0;
                end
            end
            CORR=AS.VG(brG).CorFct;  % garder la matrice de toutes les corrélations avec p<=seuils(2)
        end
        if exist('F','var')
            clear F;
        end
        F{1}=[];    % aucune corrélation exclue si F{1} pas remplacé
        f=find((pr>=AS.seuils(1)) .* (pr<=AS.seuils(2)));
        if isempty(f)
            cc=1;
        else
            cc= 2; % 1+ (brG==1);   % s'assurer de tester la première structure avec toutes ses corrélations gardées
            if numel(f)==1
                F{cc}=f;
            else
                s=subsets(f);
                F(cc:(numel(s)+cc-1))=s;
            end
        end
        for k=1:numel(F)
            CO=CORR;
            s=F{k};
            for c=s
                CO(rg(c,1),rg(c,2))=0;
            end
            fc=find(abs(CO)>.95);
            if ~isempty(fc)
                CO(fc)=AS.VG(brG).CorEstim(fc);
            end
            CO=CO+CO'+eye(ng);
            % AS=asOptim_Fit(AS,brG,CO);
            AS.VG(brG).FC(k).Fct=AS.VG(brG).Fct;
            [AS.VG(brG).FC(k).Fct(pe,:),AS.VG(brG).FC(k).CorFct,X2,dl]=SEM(Fct,FctX,CO,R,AS.N);
            S=AS.VG(brG).FC(k).Fct*AS.VG(brG).FC(k).CorFct*AS.VG(brG).FC(k).Fct';
            commun=diag(S);
            S=S+diag(1-diag(S));
            AS.VG(brG).FC(k).reprod=S;
            % if any(sum(AS.VG(brG).FC(k).Fct.^2,2)>.999) || X2<0
            if any(commun>.99) || X2<0
                p=0;
                X2=0;
                dl=1+(X2<0);  % dl=1 signale communauté > 1; dl=2 pour X2 négatif
            else
                p=1-chi2cdf(X2,dl);
            end
            AS.VG(brG).FC(k).Fit=[p,X2,dl];
            % cc=cc+1;
        end
    catch AA
        AS.VG(brG).erreur=[AA.message,sprintf(' dans as.TestCorrFct.FC, brG=%d',brG)];
        if exist('cc','var')
            AS.VG(brG).erreur=[AS.VG(brG).erreur,sprintf(', cc=%d',cc)];
        end
        AS.VG(brG).FC(k).erreur=[AA.message,' dans as.TestCorrFct'];
        keyboard % vérifier qu'on n'enlève pas un modèle OK mais incorrect avec d'autres corrélations
        AS.VG(brG).Gr={};
    end
end