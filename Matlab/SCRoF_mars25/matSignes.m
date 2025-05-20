function signes=matSignes(k)
% signes=matSignes(k);
switch k
    case 3
        signes=[1 1 -1;1 -1 1;-1 1 1];
    case 4
        signes=[1 1 -1 -1;1 -1 1 -1;1 -1 -1 1;-1 1 1 -1;-1 1 -1 1;-1 -1 1 1];
    case 5
        signes=[1 1 -1 -1 -1;1 -1 1 -1 -1;
                1 -1 -1 1 -1;1 -1 -1 -1 1;
                -1 1 1 -1 -1;-1 1 -1 1 -1;
                -1 1 -1 -1 1;-1 -1 1 1 -1;
                -1 -1 1 -1 1;-1 -1 -1 1 1];
    otherwise
        error('matSignes(k) prévu seulement pour k entre 3 et 5')
end
