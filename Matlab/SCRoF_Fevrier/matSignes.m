function signes=matSignes(k)
% signes=matSignes(k);
if k==4
    signes=[1 1 -1 -1;1 -1 1 -1;1 -1 -1 1;-1 1 1 -1;-1 1 -1 1;-1 -1 1 1];
else
    signes=[1 1 -1;1 -1 1;-1 1 1];
end
