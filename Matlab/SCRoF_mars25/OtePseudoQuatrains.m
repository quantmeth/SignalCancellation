function AS=OtePseudoQuatrains(AS)
% AS=OtePseudoQuatrains(AS);
% Examine aussi les champs .coplan pour y détecter de faux quatrains, qui
% sont constitués d'annulation des mêmes deux grappes (qui auraient dû être
% agrégées) avec d'autres grappes qui n'apparaissent qu'une fois.
% Un vrai quatrain devrait décrire quatre directions dans un même plan, où
% quatre grappes apparaissent chacune trois fois. On devrait avoir quatre
% triplets coplanaires, mais j'avais analysé qu'il puisse arriver qu'il en
% manque un.
brG=1;
while brG<=numel(AS.VG)
    copl=AS.VG(brG).coplan;
    if size(copl,1)>1
        coPlus=classeTriplets(copl);
        codes=unique(coPlus(:,end));
        rg=find(mod(codes,10)==4);
        for k=numel(rg)  % on pourrait avoir deux plans de quatrains, alors les codes seraient 4 et 14
            co=coPlus(coPlus(:,end)==codes(rg(k)),2:end-1);
            cens=recens(co);
            if any(cens(:,2)==1)
                AS.VG(brG)=[];
                brG=brG-1;
            end
        end
    end
    brG=brG+1;
end

