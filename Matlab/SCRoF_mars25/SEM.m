function [Fct,Fcor,X2,df]=SEM(Fct,FctX,Fcor,R,N)
% [Fct,Fcor]=SEM(Fct,FctX,Fcor,R,N);
% Reçoit une estimation initiale de la matrice de patrons et de corrélations,
% FctX est comme Fct mais contient 2.0 là où le paramètre est à optimiser;
% d'autres cellules peuvent avoir des constantes à prendre en compte dans
% l'estimation du critère
% Suivent la matrice de corrélation des données (orpheliens exclues) et la taille
% d'échantillon
% et optimise les entrées dand ces matrices qui ne sont pas nulles
% retourne aussi le X2 de fit et ses degrée de liberté.

[cr,rg]=triU(Fcor);
rg(cr==0,:)=[];
P=.95*[Fct(FctX(:)==2);cr(cr(:)~=0)]; % l'initialisation ajoute 5% à chaque variable ce qui peut donner des cas de Heywood
f=sum(FctX(:)==2);
[nv,nf]=size(Fct);
np=sum(FctX(:)~=0)+sum(cr(:)~=0);
df=nv*(nv-1)/2-np;
options=optimset('MaxFunEvals',1e5,'MaxIter',1e5,'TolFun',1e-4,'TolX',1e-4,'Display','off');
[P,X2]=fminsearch(@(P) SEM_crit(P,R,FctX,rg),P,options); 
X2=(N-1)*(X2-log(det(R))-nv);
Fct(FctX(:)==2)=P(1:f);
Fcor=refaitCor(P(f+1:end),nf,rg);
end

function X2=SEM_crit(P,R,Fct,rg)
% if any(isnan(Fct(:))) || any(abs(P)>.99)  % il faut tenir compte des
% corrélations entre facteurs
%     X2=9999;
%     return
% end
f=sum(Fct(:)==2);
Fct(Fct(:)==2)=P(1:f);
nf=size(Fct,2);
C=refaitCor(P(f+1:end),nf,rg);
S=Fct*C*Fct';  % matrice de covariances des variables
commun=diag(S);
S=S+diag(1-diag(S));
detS=det(S);
% if any(sum(Fct.^2,2)>.98) || detS<0 || rcond(S) < eps || rcond(R/S) < eps
if any(commun>.99) || detS<0 || rcond(S) < eps || rcond(R/S) < eps
    X2=9999;
else
    X2=log(detS)+trace(R/S);
end
end

function C=refaitCor(P,nf,rg)
C=zeros(nf);
for k=1:size(rg,1)
    C(rg(k,1),rg(k,2))=P(k);
end
C=C+C'+eye(nf);
end