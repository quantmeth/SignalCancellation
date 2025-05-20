function corrig=correctionFDR(prob)
% corrig=correctionFDR(prob);
% prob est un vecteur de probabilités pour des tests à estimer par False
% Discovery Rate (FDR)
[pr,oo]=sort(prob);
n=numel(pr):-1:1;
% corrig=1-(1-pr).^(1./n');
corrig=1-(1-pr).^(n');
try
corrig(oo)=corrig;
catch
    keyboard
end