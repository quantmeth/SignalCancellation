function [axes,modelProb,eigRanks,Ev,nfctPA]=NEST(R,nS,graph)
% [axes,modelProb,eigRanks]=NEST(data,graph);min(Ev) max(Ev)
% or [axes,modelProb,eigRanks]=NEST(R,nS,graph);
% First form: data(nS,nVar)
% Second form: R(nVar,nVar), the correlation matrix,
%              nS the number of cases
% graph, if present, indicates to produce the graphic outputs and is the
% common caption for all the graphs
% axes(nVar,nFct) gives the set of loadings for the retained solution
% modelProb(1+nFact) is the empirical probability for models
%           with 0 to nFact factors
% eigRanks(nFact+1) the ranks of data eigenvalues for the final solution
% nfctPA(2) résultats de l'analyse parallèle pour 50e et 95e centiles (6-oct-2024)
% (c) André Achim, July 2016
eigRanks=[];
[N,nVar]=size(R);
if N==nVar     % second input form
    grph=nargin>2;
else           % first input form
    if nargin>1
        grph=1;
        graph=nS;
    else
        grph=0;
    end
    nS=N;
    R=corrcoef(R);
end
nrep=1000;
[~,EigVal]=svd(R);
EigVal=diag(EigVal);
% prob=[];
rPA=ones(1,nVar-1);   % ranks of data eigenvalues among simulations for Parallel Analysis
Ev=zeros(nrep,nVar);
hndl=[];
rg=round([.01 .05 .5 .95 .99]*nrep);
maxFct=min(sum(EigVal>1e-4),ceil(.8*nVar));
if maxFct>=nVar, maxFct=nVar-1; end
for k=0:maxFct
    recipe=prepareRecipe(R,k);  % will be (nVar+k,nVar)
    nr=size(recipe,1);
    for j=1:nrep
        rr=corrcoef(randn(nS,nr)*recipe);
        % if grph
            [~,b]=svd(rr);
            Ev(j,:)=diag(b);
        % else
        %     [~,b]=svds(rr,k+1);
        %     Ev(j,1:k+1)=diag(b);
        % end
    end
    Ev=sort(Ev);
    if k==0
        for j=1:nVar-1
            rPA(j)=1+sum(Ev(:,j)>=EigVal(j));
        end
        modelProb(1)=rPA(1);
        f=find(rPA>rg(3),1);
        if isempty(f), f=1;end
        nfctPA(1)=f(1)-1;
        f=find(rPA>rg(2),1);
        if isempty(f), f=1;end
        nfctPA(2)=f(1)-1;
    else
        modelProb(k+1)=1+sum(Ev(:,k+1)>=EigVal(k+1));
    end
    if grph
        hndl(end+1)=figure;
        col=.6*[1 1 1];
        fill([1:nVar nVar:-1:1],[Ev(nrep,:) Ev(1,end:-1:1)],col);
        hold on
        fill([1:nVar nVar:-1:1],[Ev(rg(2),:) Ev(rg(4),end:-1:1)],.5*col);
        plot(Ev(rg(1:2:5),:)','k');
        if k==0
            plot(EigVal,'r-o');
            ylim([0,max(Ev(end,1),EigVal(1)+.3)]);
            for j=1:nVar-1
                text(j,EigVal(1),num2str(rPA(j)));
            end
            if nfctPA(1)>1, s='s'; else s=[]; end
            ti=['Parallel Analysis (' num2str(nfctPA) ' factor' s ' suggested)'] ;
        else
            plot(EigVal,'r');
            h=max(Ev(end,k+1)+.3,EigVal(k+1)); g=k+.85;
            b=0;d=k+1.15;
            plot([g d d g g],[b b h h b],'b:');
            text(d,h,[' ' num2str(modelProb(k+1)) '/' num2str(nrep+1)]);
            ti=['Next Eigenvalue Sufficiency test (NEST) for the ' num2str(k) ' factor model'];
        end
        title([graph 10 ti]);
        xlim([.9,nVar+.1]);
    end
    modelProb(k+1)=modelProb(k+1)/(nrep+1);
    if modelProb(k+1)>.05 && isempty(eigRanks)
        for j=1:k+1
            eigRanks(j)=1+sum(Ev(:,j)>=EigVal(j));
        end
    end
    if modelProb(k+1)>.5
        Ev(:,k+2:end)=[];
        break;
    end
end
axes=recipe(1:k,:)';
if grph
    for j=numel(hndl)-1:-1:1
        figure(hndl(j));
    end
end
end

function recipe=prepareRecipe(R,k)
nVar=size(R,1);
if k==0
    recipe=eye(nVar);
else
% [~,v]=svds(R,k+1);P.vp=diag(v);P.nf=k;
    [~,axes]=reduceR(R,k);
%     [Rr,axes]=reduitAtanh(R,k);
% 3 prochaines lignes donnent une solution via blanchiment (équivaut à vraisemblance maximale)
%     [Rb,d]=blanchis(Rr);
%     [axes,ampl]=svdR(Rb,k);
%     axes=diag(d)*axes*diag(sqrt(ampl));
    v=sum(axes.^2,2);
    recipe=[axes,diag(sqrt(1-v))]';
end
end

function [Rr,axes]=reduceR(R,k)
[Rr,axes]=reduitR(R,k);   % pour incorporer les modifications apportées re: Heywood
% % % [Rr,axes]=reduceR(R,k);
% % % inspired from Ruscio and Kaczetow (2008) Factor.Analysis procedure
% % % puts communalities for k factors in R's diagonal
% % converg=.00001;
% % Rr=R;
% % pre=diag(Rr);
% % diagOK=diag(R);
% % for j=1:500   % max nb of iterations
% %     [c,eigv]=svdR(Rr);
% %     kk=k; while eigv(kk)<=0, kk=kk-1; end
% %     axes=c(:,1:kk)*diag(sqrt(eigv(1:kk)));
% %     if j==1, ax=axes; end % for rare Heywood cases at first iteration
% %     commun=sum(axes.^2,2);
% %     %     commun=min(1,commun);
% %     if any(commun>1)       % stop before any communality exceeds unity
% % %         fprintf('H');
% %         Rr=Rr+diag(diagOK-diag(Rr));
% %         if j==1
% %             ac=find(commun>1);
% %             ax(ac,:)=ax(ac,:)./sqrt(sum(axes(ac,:).^2));
% %         end
% %         axes=ax;
% %         break;
% %     end
% %     if max(abs(pre-commun))<converg, break; end
% %     diagOK=diag(Rr);
% %     Rr=Rr+diag(commun-pre);
% %     pre=commun;
% %     ax=axes;
% % end
% f=find(diag(Rr)<.1);
% if ~isempty(f)
%     warning(['When k=' sprintf('%d',k) ',probability of orphan variable at rank',sprintf(' %d',f)]);
% end
end

% function [a,b]=svdR(R)   % version of svd that returns the singular
% values as negative is they are negative
% [a,b]=svd(R);
% b=diag(b)';
% Q=R*a./b(ones(size(R,1),1),:);
% b=b.*sign(a(1,:).*Q(1,:));
% [b,o]=sort(b,'descend');
% a=a(:,o);
