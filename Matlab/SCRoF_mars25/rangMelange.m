function mel=rangMelange(AS,melange)
% mel=rangMelange(AS,melange);
% if ~isfield(AS,'Tuples')
%     mel=[];
%     return
% end
code=encodeMelange(AS,melange);
nm=numel(melange);
if numel(AS.Tuples)<nm
    AS.Tuples{nm}.melange=0; %POC very weird, useless?
    mel=[];
else
    mel=find(AS.Tuples{nm}.melange==code);
end