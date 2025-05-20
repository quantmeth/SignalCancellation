function [x, fmax0, nmax]=NM_AA(x, G,melange,cible)
% [x, fmax, nmax]=NM_AA(fun, x, varargin);
% Utilise nmsmax de N. Highan, avec initialisation modifiée par moi
% Pour MAXIMISER le critère (contraitrement à fminsearch) 
% En entrée, x est la première initialisation
% En sortie x est la solution, fmin le critère et nmax le nombre de minima
% locaux auquel la solution a échappé avant d'avoir convergé fois (défaut 3)
% fois à une solution essentiellement la même
fois=3;
stopit(5)=0;
stopit(1)=1e-3;  % deviendra 1e-4 quand on reviendra à une même solution
stopit(2)=inf;
stopit(3)=inf;
stopit(4)=1;
if iscell(G)
    melange=G{2};
    cible=G{3};
    G=G{1};
end
[x0, fmax0] = nmsmax(@as_Crit, x, stopit, [], G,melange,cible);
x=x0;
encore=fois;
nmax=0;
while encore
    [x, fmax] = nmsmax(@as_Crit, x, stopit, [], G,melange,cible);
    if fmax-fmax0>1e-3 && max(abs(x0-x))>1e-3  % amélioration
        x0=x;
        fmax0=fmax;
        encore =fois;
        nmax=nmax+1;
    else     % on retrouve la même solution
        encore=encore-1;
        if fmax0>fmax
            fmax0=fmax;
            x0=x;
        end
        stopit(1)=1e-4;
        stopit(4)=1-stopit(4);  % alterner les initialisations 
    end
end