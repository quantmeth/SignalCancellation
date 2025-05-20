function tri=ordonneTrio(AS,brG,trio,varargin)
% tri=ordonneTrio(AS,brG,trio,varargin);
% fonction créée le 2 mars 2025
% trio est un trio de rangs de GRAPPES et non de variables, sauf si un
% quelconque 4e argument est fourni
% met en 3e l'élément du trio qui semble entre les deux autres pour
% faciliter l'annulation
ordre=[1 2 3; 1 3 2;2 3 1];
if ~isempty(varargin)
    tri=asGrappeDe(AS,brG,trio);
    if numel(tri)<numel(trio)    % si une variable pas dans une grappe
        tri=trio;
        return
    end
else
    tri=trio;
end
co=abs(AS.VG(brG).CorFct(tri,tri));
co=co+co';
c=co([4 7 8]);  % les trois corrélations de trois grappestrio=tri

f=find(c==min(c));
tri=trio(ordre(f(1),:)); % grappe ou variable la plus centrale en dernier
