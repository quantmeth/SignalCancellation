function lign=lignesAvec(mat,val)
%  lign=lignesAvec(mat,val);
% retourne les rangs des lignes de mat qui contiennent un élément de val
nl=size(mat,1);
val(val==0)=[];
lign=1:nl;
for k=nl:-1:1
    A=val(:)==mat(k,:);
    if ~any(A(:))
        lign(k)=[];
    end
end