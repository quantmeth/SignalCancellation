function AS=asUnFacteur(AS,brG)
% AS=asUnFacteur(AS,brG);
AS.VG(brG).FC(1).CorFct=1;
AS.brancheC=1;
AS.VG(brG).FC(1).reprodR=AS.VG(brG).Fct*AS.VG(brG).Fct';
[p,X2,df]=asFit(AS); % pour les valeurs courantes de .brancheG et .brancheC
AS.VG(brG).FC(1).Fit=[p,X2,df];
