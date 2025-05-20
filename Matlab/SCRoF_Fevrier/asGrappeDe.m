function gr=asGrappeDe(AS,brG,var)
% gr=asGrappeDe(AS,brG,var);
% retourne dans gr le(s) rang(s) de grappes qui conti(enn)ent une des
% variables de var
gr=[];
Gr=AS.VG(brG).Gr;
for v=1:numel(var)
    for g=1:numel(Gr)
        if any(Gr{g}==var(v))
            gr=[gr g];
        end
    end
end
gr=unique(gr);