function coPlus=Doubl(coPlus)
% coPlus=Doubl(coPlus);
% Chaque ligne de coPlus contient une probabilité, trois rangs de grappes et un 0 ou code
d4=find(coPlus(:,end)==4,1,'last');
coP=coPlus(coPlus(:,end)==0,2:4);
cens=recens(coP);
d=0;
while any(cens(:,2)>1)
    f=find(coPlus(:,end)==0);
    r=find(cens(:,2)>1,1);
    r=cens(r,1);
    li=lignesAvec(coP,r);
    L=[];
    g=coP(li(1),:);
    if numel(li)>2
        keyboard % si ça arrive, l'utilisation de li comme pointeur plus bas pourrait être incorrecte
    end
    for k=2:numel(li)   % numel(li) n'est-il pas forcément 2?
        uni=unique([g,coP(li(k),:)]);
        if numel(uni)==5
            L=[f(1) f(k) 2+d];
        end
        if numel(uni)==4
            L=[f(1) f(k) 3+d];
        end
    end
    coPlus(li+d4,end)=L(3);
    d=d+10;
    coP=coPlus(coPlus(:,end)==0,2:4);
    cens=recens(coP);
end
