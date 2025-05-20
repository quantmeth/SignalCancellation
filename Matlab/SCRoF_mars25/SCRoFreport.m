function [Fct,Fcor]=SCRoFreport(AS,rang)
% [Fct,Fcor]=SCRoFreport(AS,rang);
% Si rang absent, imprime le résumé des scénarios
% Si rang a dimension (1), c'est un pointeur dans AS.scenarios
% Si exprimé en négatif, les écarts types sont appliqués à la matrice de
% patrons (solution de covariances plutôt que de corrélations)   % modif 1 mars 2025
% Pour une solution pas dans la sortie imprimée, rang=[brG,c0]
% et on rapporte pour AS.VG(brG).FC(co)
% Imprime (et retourne) la matrice de patron et celle de
% corrélation des facteurs pour la solution spécifiée par rang
if nargin<2
    Sc=AS.scenarios;
    fprintf('\nScénarios d''intérêt:')
    ns=size(Sc,1);
    for k=1:ns
        s=Sc(k,:);
        cp=AS.VG(s(4)).GrCoplan;
        fprintf('\n%d:\tp=%.4f X2(%d)=%.3f VG%d FC%d  %df',k,abs(s(1)),s(3),s(2),s(4:6));
        if s(5)==1   % pour AS.VG(k).FC(1)
            for j=1:numel(cp)  % variables multifactorielles
                fprintf(' bf:');
                fprintf('%d,%d ',cp{j}); 
            end
            if AS.VG(s(4)).Parent==0
                fprintf(' %s ',AS.VG(s(4)).Creat)
            else
                fprintf(' %s VG%d',AS.VG(s(4)).Creat,AS.VG(s(4)).Parent)
            end
            % if ~isempty(AS.VG(s(4)).reste)
            if s(1)<0
                fprintf(' excluant ')
                fprintf(' v%d',AS.VG(s(4)).reste)
            end
        else
            fprintf('   "')
        end
        if k==1 && ~isempty(AS.VG(1).coplan)
            fprintf(' plan:')
            for j=1:size(AS.VG(1).coplan,1)
                fprintf('(%0.3f:%d,%d,%d)',AS.VG(1).coplan(j,:));
            end
        end
    end
    fprintf('\n')
    return
end
if nargin==1
    return
end
if numel(rang)==1
    br=AS.scenarios(abs(rang),:);   % modif 1 mars 2025
    brG=br(4);
    co=br(5);
else
    brG=rang(1);
    co=rang(2);
end
Fct=AS.VG(brG).FC(co).Fct;
if rang<0       % modif 1 mars 2025
    Fct=AS.et.*Fct;
end
nf=size(Fct,2);
fprintf('Fct:')
for k=1:size(Fct,1)
    fprintf('\n%d',k)
    for j=1:nf
        fprintf('\t%.3f',Fct(k,j));
    end
end
if numel(AS.VG(brG).Gr)>1
    Fcor=AS.VG(brG).FC(co).CorFct;
    printCor(Fcor,'fCorr');
end
dendr(AS,abs(rang));     % modifié 1 mars 2025
fprintf('\n')
end

function sig=printCor(F,titre,N)  % n'est plus utilisé (nov. 2024) avec le 3e argument
if nargin>1
    fprintf('\n%s:\n',titre)
end
nf=size(F,2);
if nargin>2
    rcrit=sqrt(chi2inv(.95^(1/(nf-1)),1)/(N-1));
else
    rcrit=1;
end
for k=1:nf
    fprintf('\t%d',k);
end
sig=[];
for j=1:nf
    fprintf('\n%d',j);
    for k=1:nf
        fprintf('\t%.3f',F(j,k));
        if j~=k && abs(F(j,k))>rcrit
            fprintf('*');
            if k>j
                sig=[sig; [j k F(j,k)]];
            end
        end
    end
end
fprintf('\n');
for k=1:size(sig,1)
    fprintf('\n%2d %2d %.3f',sig(k,:))
end
fprintf('\n');
end