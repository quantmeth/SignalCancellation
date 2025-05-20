function declin=asCoplanDeclinaisons(AS,brG)
% AS.declin=asCoplanDeclinaisons(AS,brG);
% if brG>1
    % juste mettre à jour ce qui a changé depuis AS.VG(1)
    % keyboard
% end
ns=numel(AS.SCEN); 
declin{ns}=[];
for k=1:ns    % pour chaque SCEN{} établir declin{k}
    c=recens(AS.GRP(AS.SCEN{k},:));
    doubl=c(c(:,2)>1,1);
    ngr=numel(AS.SCEN{k});
    garde{ngr}=[];
    for j=1:ngr
        gr=AS.GRP(AS.SCEN{k}(j),:);
        gr(gr==0)=[];
        signes=matSignes(numel(gr));
        garde{j}=1:size(signes,1);
        for i=1:numel(doubl)
            garde{j}=setdiff(garde{j},find(signes(:,gr==doubl(i))<0));
        end
        for i=numel(garde{j}):-1:1
            if numel(AS.VG(brG).Gr{gr(garde{j}(i))})>2
                garde{j}(i)=[];
            end
        end
    end
    croise=garde{1}(:);
    croise=croise(:);
    for j=2:ngr
        croise=10*croise+garde{j};
        croise=croise(:);
    end
    clear garde
    declin{k}=croise;
end