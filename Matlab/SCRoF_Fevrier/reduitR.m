function [Rr,axes,Heywood]=reduitR(R,k,ech)
% [Rr,axes,Heywood]=reduitR(R,k);
% change R en mettant dans sa diagonale les communalit�s selon les k
% premi�res composantes principales
% d'apr�s Factor.Analysis de la proc�dure EFA de Ruscio et Kaczetow 2008
% avec modifications pour g�rer les cas de Heywood.
% En sortie, Heywood est 0 ou le rang du cas de Heywood rencontr�
% ech, si pr�sent, est la transformation d'�chelles des variables qu'a subi R
if any(diag(R)<=0), error('ReduitR n''accepte pas une patrice avec variance non positive'); end
if nargin>2
    d=ech.^2;
else
    d=diag(R);  % La limite � ne pas d�passer, autreement, c'est un cas de Heywood
end
converg=.000001;
if all(diag(R)==d) % si on a une pleine matrice
    Rr=reduitM(R);   % initialisation pour assurer que les variables orphelines ne soient pas gard�es
else
    Rr=R;
end
pre=diag(Rr);
diagOK=pre; % �tre s�r d'exister si la premi�re it�ration produisait un cas de Heywood
Heywood=0;
for j=1:500   % max nb d'it�rations
    [V,e]=svdR(Rr,k);
    while any(e<0)
        Rr=Rr+diag(.1*diag(Rr));
        if any(diag(Rr)>d)
            di=diag(Rr);
            Rr=Rr+diag(min(di,d)-di);
        end
        [V,e]=svdR(Rr,k);
    end
    axes=V*diag(sqrt(e));
    commun=sum(axes.^2,2);
    %     if Heywood
    %         break
    %     end
    if any(commun>d)
        Heywood=find(commun>d,1);
        break
    end
    if max(abs(pre-commun))<converg
        break;
    end
    diagOK=diag(Rr);
    Rr=Rr+diag(commun-pre);
    if any(diag(Rr)>d)
        Heywood=find(commun>d,1);
        Rr=R+diag(diagOK-1);
        [V,e]=svdR(Rr,k);
        axes=V*diag(sqrt(e));
        break
    end
    pre=commun;
end
if Heywood
    Rr=R+diag(diagOK-1);
    [V,e]=svdR(Rr,k);
    axes=V*diag(sqrt(e));
end

