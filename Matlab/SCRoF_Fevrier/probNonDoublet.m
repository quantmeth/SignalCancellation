function pr=probNonDoublet(AS,melange)
% pr=probNonDoublet(AS,melange);
% retourne une probabilité nette (ajusteé selon le nombre de corrélations
% dont le maximum en valeurs absolues est utilisé ici)
var=setdiff(AS.pertinent,melange);
mc=max(max(abs(AS.R(var,melange)))); % correl maximale des deux variables de mélange avec toutes les autres pertinentes
x2=mc.^2*(AS.N-1);
pr=1-chi2cdf(x2,1).^(2*numel(var));