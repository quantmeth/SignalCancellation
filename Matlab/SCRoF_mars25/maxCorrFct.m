function rmax=maxCorrFct(p)
% rmax=maxCorrFct(p)
% pour des probabilités p entre .001 et .25 associée à l'agrégation de deux
% sous-grappes, retourne les corrélations maximales rmax qu'on veuille tolérer
% entre les deux sous-grappes pour ne pas les forcer à s'agréger
L=log([.001 p .25]);
rmax=.5+.3*(L(2:end-1)-L(end))./(L(1)-L(end));