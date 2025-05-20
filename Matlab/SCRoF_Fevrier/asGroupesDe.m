function gd=asGroupesDe(AS,brG,var)
% retourne les rangs des groupes des variables var dand AS.KV(br).VG(br2).Gr
% brG=AS.brancheG;
Gr=AS.VG(brG).Gr;
nv=numel(var);
gd=zeros(nv,1);
for k=1:nv
    for j=1:numel(Gr)
        if any(Gr{j}==var(k))
            break
        end
    end
    if any(Gr{j}==var(k))
        gd(k)=j;
    else
        error('La variable v%d n''est dans aucun groupe de AS.VG(%d)',var(k),brG)
    end
end