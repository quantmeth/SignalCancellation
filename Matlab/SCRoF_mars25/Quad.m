function coPlus=Quad(coPlus)
% coPlus=Quad(coPlus);
% Chaque ligne de coPlus contient 1 probabilité, 3 rangs de grappes 1 0 ou code
% la probabilité des quatrains est forcée à .5
coP=coPlus(coPlus(:,end)==0,2:4);
cens=recens(coP);
d=0;
while any(cens(:,2)>2)
    r=find(cens(:,2)>2,1);
    r=cens(r,1);
    li=lignesAvec(coP,r); % il manque la ligne avec les 3 autres que r
    L=li(1);
    g=coP(li(1),:);
    for k=2:numel(li)
        if numel(unique([g,coP(li(k),:)]))==4
            L=[L,li(k)];
        end
    end
    % autre=setdiff(unique(coP(L,:)),r)'*[1;10;100];
    sd=setdiff(unique(coP(L,:)),r)';
    mu=10.^(0:numel(sd)-1)';
    autre=sd*mu;
    L=[L find(coP*[1;10;100]==autre)];
    f=find(coPlus(:,end)==0);
    coPlus(f(L),end)=4+d;
    coPlus(f(L),1)=.5;
    d=d+10;
    coP=coPlus(coPlus(:,end)==0,2:4);
    cens=recens(coP);
end