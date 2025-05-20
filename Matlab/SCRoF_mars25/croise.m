function tuple=croise(A,B)
% retourne tous les croisements des lignes de A avec celles de B
[ra,ca]=size(A);
[rb,cb]=size(B);
tuple=[];
ligne=zeros(ca+cb,1);
for j=1:ra
    ligne(1:ca)=A(j,:);
    for k=1:rb
        ligne(ca+1:end)=B(k,:);
        tuple=[tuple;ligne'];
    end
end
