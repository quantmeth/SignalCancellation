function [a,b]=svdR(R,nf,arg)
% [a,b]=svdR(R,nf,arg);
% appelle svds si nf pr�sent ou svd autrement
% puis s'assure du signe des composantes et r�ordonne les facteurs au besoin
% la polarit� des facteurs est fix�e pour que la somme des cubes des saturations soit >=0
if nargin>1
    if nargin<3, arg='largest';end
    [a,b,c]=svds(R,nf,arg);
else
    [a,b,c]=svd(R);
end
Q=diag(a'*c);
b=diag(b)'.*Q';
% donner une polarit� aux vecteurs pour moyenne positive de la somme des cubes
q=sum(a.^3);  
a=a*diag(sign(q));
