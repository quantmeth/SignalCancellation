function [r,pr,cc,st]=fctCor(AS,br,i,j)
% [r,pr,cc,st]=fctCor(AS,br,i,j);
% retourne la corrélation r entre les grappes i et j du groupe br
% la probabilité pr et la corrélation canonique cc sont issues de canoncorr
% des variables
% tandis que r estime la corrélation du signal des variables
% utilisant AS.GS et AS.N
Gr=AS.VG(br).Gr;
% test de signification de la corrélation
X=prepGS(AS.GS(:,Gr{i}),AS.N);
Y=prepGS(AS.GS(:,Gr{j}),AS.N);
[~,~,cc,~,~,st] = canoncorr(X,Y);
cc=cc(1);
pr=st.pF(1);
% calcul de la corrélation
R=AS.R(Gr{i},Gr{j});
F=AS.VG(br).Fct;
P=F(Gr{i},i)*F(Gr{j},j)';
r=P(:)\R(:);

function MM=prepGS(M,N)
MM=sqrt(.5)*M;
MM=[MM;-MM];
[n,v]=size(MM);
n=N-n;
MM=[MM;zeros(n,v)];
