function [a,b]=svdR(R,nf,arg)
% [a,b]=svdR(R,nf,arg);
% appelle svds si nf présent ou svd autrement
% puis s'assure du signe des composantes et réordonne les facteurs au besoin
% la polarité des facteurs est fixée pour que la somme des cubes des saturations soit >=0
if nargin>1
    if nargin<3, arg='largest';end
    [a,b,c]=svds(R,nf,arg);
else
    [a,b,c]=svd(R);
end
Q=diag(a'*c);
b=diag(b)'.*Q';
% donner une polarité aux vecteurs pour moyenne positive de la somme des cubes
q=sum(a.^3);  
a=a*diag(sign(q));
