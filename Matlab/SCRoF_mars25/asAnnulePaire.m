function AS=asAnnulePaire(AS,k)
% AS=asAnnulePaire(AS,k);
% AS.GS(v,v) est la GrammSchmidt (en triangle supérieur) de R(v,v)
% k est un scalaire
% Optimise les variables de la rangée k de AS.Cpaire pour en
% minimiser le signal
% remplit AS.Crit(k), AS.Ppaires(k) et AS.Corr(:,k)
% AS.Crit(k) est déjà le X2: N-1 fois la somme des corrélations au carré

% limite=.98; % limite sur la plus grande saturation en valeur absolue
G=AS.GS;
cible=AS.pertinent;
melange=AS.Cpaires(k,:);
lP=0;
pd=probNonDoublet(AS,melange);
if pd<.05
    r=AS.R(melange(1),melange(2));
    K=-sign(r);  % le poids fixe pour la 2e variable
    lP=0;  % logarithme de P=1
    options=optimset('MaxFunEvals',1e4,'MaxIter',1e4,'TolFun',1e-4,'TolX',1e-4);
    lP=fminsearch(@(lP) CritLog(lP,G,melange,cible,K),lP,options);
    P=exp(lP);
    % r=AS.R(melange(1),melange(2));
    % ri=r/limite;
    % rs=limite/r;
    % if r>0
    %     if P>rs
    %         P=rs;
    %     elseif P<ri
    %         P=ri;
    %     end
    % else
    %     if P<rs
    %         P=rs;
    %     elseif P>ri
    %         P=ri;
    %     end
    % end
else
    AS.doublet=[AS.doublet;melange];
    K=1;
end
[~,corr]=CritLog(lP,G,melange,cible,K);
AS.Crit(k)=corr*corr'*(AS.N-1);
AS.Corr(:,k)=corr;
AS.Ppaires(k)=exp(lP);
end

function [crit,corr]=CritLog(px,G,combine,cible,K)
pp=[exp(px);K];
SP=G(:,combine)*pp;
SP=SP./sqrt(SP'*SP); 
corr=SP'*G;   % corr(1,v)
corr(setdiff(1:numel(corr),setdiff(cible,combine)))=0;
crit=max(corr.^2);
if ~isreal(crit)
    crit=99e9;
end
end