function Rr=reduitM(R,nf)
%  Rr=reduitM(R,nf);
% applique à R la réduction par régression multiple
Rr=R-diag(1./diag(inv(R)));
if nargin>1
    [F,b]=svdR(Rr,nf);
    if any(b<0)
        b=max(0,b);
        % keyboard;
    end
    Rr=F*diag(b)*F';
end

