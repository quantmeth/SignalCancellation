function rP=asPaireAvec(AS,v)
% rP=asPaireAvec(AS,v);
% Retourne les rangs de AS.Cpaires qui impliquent la variable v
P=AS.Cpaires;
rP=find((P(:,1)==v) + (P(:,2)==v));