function SS=subsets(lst,mi,ma)
% SS=subsets(lst,min,max);
% min (def:1) and max (def:numel(lst)) give the range of subset sizes
% SS{} contains all subsets of lst containing between min and max elements
n=numel(lst);
if n<2
    SS{1}=lst;
    return
    % keyboard
    % error('L''argument ''lst'' de subset doit contenir au moins deux valeurs');
end
if nargin<2
    mi=1;
end
if nargin<3
    ma=n;
end
SS{1}=[];
s=0;
lst=sort(lst); % pour assurer correspondance avec AS.Cpaires, si ça compte
for k=mi:ma
    L=nchoosek(lst,k);
    for j=1:size(L,1)
        s=s+1;
        SS{s}=L(j,:);
    end
end