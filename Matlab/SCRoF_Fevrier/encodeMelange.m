function code=encodeMelange(AS,melange)
% code=encodeMelange(AS,melange);
nm=numel(melange);
p=AS.nv.^(0:nm-1);
code=p*melange';
