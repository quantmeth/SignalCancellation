function [Gr,reste]=asNettoie(Gr)
% Gr=asNnettoie(Gr);
% le dernier regroupement de Gr est considéré gardé
% Tous les sous-ensembles qui précèdent sont enlevés
m=numel(Gr);
for k=m:-1:1
    for j=Gr{k}
        for i=1:k-1
            if any(Gr{i}==j)
                Gr{i}=[];
            end
        end
    end
end
for k=m-1:-1:1
    if isempty(Gr{k})
        Gr(k)=[];
    end
end
reste=[];
for k=numel(Gr):-1:1
    if numel(Gr{k})==1
        reste=[Gr{k} reste];
        Gr(k)=[];
    end
end
% réordonner les groupes selon le rang de leurs variables
ng=numel(Gr);
prem=zeros(ng,1);
for k=1:ng
    Gr{k}=sort(Gr{k});
    prem(k)=Gr{k}(1);
end
[~,oo]=sort(prem);
Gr(:)=Gr(oo);
