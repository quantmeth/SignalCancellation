function ff=exclutSequentiels(Z,ff)
% ff=exclutSequentiels(Z,ff);
% exclut de ff un regroupement qui présume d'un autre pas inclu
% Z fut préparée par linkage.m
% ff contient les groupes de lignes dans Z correspondant à des sous ensembles
% d'agrégations considérées incertaines (.001>p>.25)
d=size(Z,1);
sur=1:d+(ff{1});
for f=2:numel(ff)-1  % premier et derniers sous-ensembles jamais en cause
    g=ff{f};
    z=g+d+1;
    for k=1:numel(g)  % tester chaque élément de g (ou de z)
        gr=[sur setdiff(z,z(k))];
        if ~any(Z(g(k),1)==gr) || ~any(Z(g(k),2)==gr)
            ff{f}=[];
        end
    end
end
for f=numel(ff)-1:-1:2
    if isempty(ff{f})
        ff(f)=[];
    end
end
