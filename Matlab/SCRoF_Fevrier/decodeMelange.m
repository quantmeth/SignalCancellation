function melange=decodeMelange(AS,code)
% melange=decodeMelange(AS,code);
if code==0
    melange=code;
    return
end
mx=max(code);
p=AS.nv.^(0:3);
while p(end)>mx
    p(end)=[];
end
nm=numel(code);
melange=zeros(nm,numel(p));
for m=1:nm
    cod=code(m);
    for k=numel(p):-1:1
        cd=mod(code(m),p(k));
        melange(m,k)=(cod-cd)/p(k);
        cod=cd;
    end
end