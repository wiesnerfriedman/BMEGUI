function maskDataGen
% 
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% 
msg =[];
 existID = exist('maskFileName.txt', 'file');
    if (existID==2)
        maskFilePath = textread('maskFileName.txt', '%s', 'delimiter', '\n','whitespace', '');
        %Read shape file
        [pathstr, name, ext] = fileparts(maskFilePath{1}); 
        if ext =='.shp'
            c=shaperead(maskFilePath{1});                
            % Look for "NaN" values (where POLYGON completes)
            N=[];   % N=No of plygon in read shape file.
            for i=1:length(c.X)
                if abs(c.X(i))>1
                else
                    N=[N,i];
                end
            end
            disp(sprintf('No of Polygons in your shape file = %d',length(N)));

            %----------------------------
            %----------------------------


            % Specifically for this PROJECT%
            %----------------------------



            % if more than one POLYGON; Ask user how many polygon have to be MASKED
              %  if N>1
              %      maskN=input('How many of these polygons you want to MASK   ');
              %  else
              %     maskN=1;
              %  end

            %length(N)=maskN 

            maskN=length(N);
            %----------------------------
            %----------------------------

            % Create coordinate file for each polygon separately
            % pVal{1} - Coordinates corresponding to Polygon # 1
            % pVal{n} - Coordinates corresponding to polygon# n

            for j=1:maskN
                if j>1
                    pVal{j}=[c.X(N(j-1)+1:N(j)-1)',c.Y(N(j-1)+1:N(j)-1)'];    
                else
                    pVal{j}=[c.X(1:N(j)-1)',c.Y(1:N(j)-1)'];
                end
            end


            % Find the area you interested in (Outer most BOUNDRY of Interest) 
            bnd=[];
            for k=1:maskN
                max1 = max(pVal{k}(:,1));
                max2 = max(pVal{k}(:,2));
                min1 = min(pVal{k}(:,1));
                min2 = min(pVal{k}(:,2));
                bnd=[bnd;max1,max2,min1,min2];
            end

            max11=max(bnd(:,1));
            max22=max(bnd(:,2));
            min11=min(bnd(:,3));
            min22=min(bnd(:,4));


            % The boundry margin for area you interested in
            % BoundryMatrix=[minLongitude maxLongitude minLatitude maxLatitude]

            del1 = (max11-min11)/5;
            del2 = (max22-min22)/5;
            ax = [min11-del1,max11+del1,min22-del2,max22+del2];

            %%%%%%plotMask(pVal,ax)
            nArea=1;
            pArea{nArea}=[pVal{1}];

            for i=1:length(pVal)-1
                if pVal{i}(end,1)==pVal{i+1}(1,1) & pVal{i}(end,2)==pVal{i+1}(1,2)
                    pArea{nArea}=[pArea{nArea};pVal{i+1}];
                else
                    nArea=nArea+1;
                    pArea{nArea}=[pVal{i+1}];
                end
            end

            for i=1:nArea
                idxsameval=[find(sum(abs(diff(pArea{i})),2)~=0);size(pArea{i},1)];
                pArea{i}=pArea{i}(idxsameval,:);
                minAreaLo(i)=min(pArea{i}(:,1));
                maxAreaLo(i)=max(pArea{i}(:,1));
                minAreaLa(i)=min(pArea{i}(:,2));
                maxAreaLa(i)=max(pArea{i}(:,2));
                idxstpt(i)=min(find(pArea{i}(:,1)==min(pArea{i}(:,1))));
            end

            puax=ax;

            if ax(1)>min(minAreaLo)
                puax(1)=min(minAreaLo)-(max(maxAreaLo)-min(minAreaLo))/20;
            end

            if ax(2)<max(maxAreaLo)
                puax(2)=max(maxAreaLo)+(max(maxAreaLo)-min(minAreaLo))/20;
            end

            if ax(3)>min(minAreaLa)
                puax(3)=min(minAreaLa)-(max(maxAreaLo)-min(minAreaLo))/20;
            end

            if ax(4)<max(maxAreaLa)
                puax(4)=max(maxAreaLa)+(max(maxAreaLo)-min(minAreaLo))/20;
            end

            [srtLo,idxLo]=sort(minAreaLo);
            maskedge=[puax(1) puax(4);puax(1) pArea{idxLo(1)}(idxstpt(idxLo(1)),2)];

            idxinArea=[];
            for i=1:nArea
                for j=1:nArea
                    if i==j
                        idxinArea(i,j)=0;
                    else
                        if maxAreaLo(idxLo(i))>maxAreaLo(idxLo(j))
                            flgin=sum(inpolygon(pArea{idxLo(j)}(:,1),pArea{idxLo(j)}(:,2),...
                                pArea{idxLo(i)}(:,1),pArea{idxLo(i)}(:,2)));
                            if flgin==0
                                idxinArea(i,j)=0;
                            else
                                idxinArea(i,j)=1;
                            end
                        else
                            idxinArea(i,j)=0;
                        end
                    end
                end
            end
            idxinside=sum(idxinArea);

            idxSep=idxLo(~logical(idxinside));
            idxIns=idxLo(logical(idxinside));

            if length(idxSep)==1
                if idxstpt(idxSep(1))~=1
                    maskedge=[maskedge;pArea{idxSep(1)}(idxstpt(idxSep(1)):end,:);...
                            pArea{idxSep(1)}(2:idxstpt(idxSep(1)),:)];
                else
                    maskedge=[maskedge;pArea{idxSep(1)}(:,:)];
                end
                maskedge=[maskedge;puax(1) pArea{idxSep(1)}(idxstpt(idxSep(1)),2);...
                        puax(1) puax(3);puax(2) puax(3);puax(2) puax(4);puax(1) puax(4)];
            elseif length(idxSep)==2
                len2=[];
                for i=1:length(pArea{idxSep(1)})
                    len1=[];
                    for j=1:length(pArea{idxSep(2)})
                        len1(j)=sqrt((pArea{idxSep(1)}(i,1)-pArea{idxSep(2)}(j,1))^2+...
                                (pArea{idxSep(1)}(i,2)-pArea{idxSep(2)}(j,2))^2);
                    end
                    [minlen1,idxlen1]=min(len1);
                    len2(i,:)=[minlen1 idxlen1];
                end
                [minlen2,idxlen2]=min(len2(:,1));
                pton1=idxlen2;
                pton2=len2(idxlen2,2);

                if idxstpt(idxSep(1))<pton1
                    maskedge=[maskedge;pArea{idxSep(1)}(idxstpt(idxSep(1)):pton1,:)];
                else
                    maskedge=[maskedge;pArea{idxSep(1)}(idxstpt(idxSep(1)):end,:);...
                            pArea{idxSep(1)}(2:pton1,:)];
                end

                if pton2==1
                    maskedge=[maskedge;pArea{idxSep(2)}(pton2:end,:)];
                else
                    maskedge=[maskedge;pArea{idxSep(2)}(pton2:end,:);pArea{idxSep(2)}(2:pton2,:)];
                end

                if idxstpt(idxSep(1))<pton1
                    maskedge=[maskedge;pArea{idxSep(1)}(pton1:end,:);...
                            pArea{idxSep(1)}(2:idxstpt(idxSep(1)),:)];
                else
                    maskedge=[maskedge;pArea{idxSep(1)}(pton1:idxstpt(idxSep(1)),:)];
                end

                maskedge=[maskedge;puax(1) pArea{idxSep(1)}(idxstpt(idxSep(1)),2);...
                        puax(1) puax(3);puax(2) puax(3);puax(2) puax(4);puax(1) puax(4)];
            else
                for i=1:length(idxSep)-1
                    if maxAreaLo(idxSep(i))>minAreaLo(idxSep(i+1))
                        error('This function cannot deal with such a complicated shape')
                    end
                    len2=[];
                    for j=1:length(pArea{idxSep(i)})
                        len1=[];
                        for k=1:length(pArea{idxSep(i+1)})
                            len1(k)=sqrt((pArea{idxSep(i)}(j,1)-pArea{idxSep(i+1)}(k,1))^2+...
                                (pArea{idxSep(i)}(j,2)-pArea{idxSep(i+1)}(k,2))^2);
                        end
                        [minlen1,idxlen1]=min(len1);
                        len2(j,:)=[minlen1 idxlen1];
                    end
                    [minlen2,idxlen2]=min(len2(:,1));
                    len3(i,:)=[minlen2,idxlen2,len2(idxlen2,2)];
                end

                if idxstpt(idxSep(1))<len3(1,2)
                    maskedge=[maskedge;pArea{idxSep(1)}(idxstpt(idxSep(1)):len3(1,2),:)];
                else
                    maskedge=[maskedge;pArea{idxSep(1)}(idxstpt(idxSep(1)):end,:);...
                            pArea{idxSep(1)}(2:len3(1,2),:)];
                end

                for i=2:length(idxSep)-1
                    if len3(i-1,3)<len3(i,2)
                        maskedge=[maskedge;pArea{idxSep(i)}(len3(i-1,3):len3(i,2),:)];
                    else
                        maskedge=[maskedge;pArea{idxSep(i)}(len3(i-1,3):end,:);...
                                pArea{idxSep(i)}(2:len3(i,2),:)];
                    end
                end

                if len3(end,3)==1
                    maskedge=[maskedge;pArea{idxSep(end)}(len3(end,3):end,:)];
                else
                    maskedge=[maskedge;pArea{idxSep(end)}(len3(end,3):end,:);pArea{idxSep(end)}(2:len3(end,3),:)];
                end

                for i=length(idxSep)-1:-1:2
                    if len3(i-1,3)<len3(i,2)
                        maskedge=[maskedge;pArea{idxSep(i)}(len3(i,2):end,:);...
                                pArea{idxSep(i)}(2:len3(i-1,3),:)];
                    else
                        maskedge=[maskedge;pArea{idxSep(i)}(len3(i,2):len3(i-1,3),:)];
                    end
                end

                if idxstpt(idxSep(1))<len3(1,2)
                    maskedge=[maskedge;pArea{idxSep(1)}(len3(1,2):end,:);...
                            pArea{idxSep(1)}(2:idxstpt(idxSep(1)),:)];
                else
                    maskedge=[maskedge;pArea{idxSep(1)}(len3(1,2):idxstpt(idxSep(1)),:)];
                end

                maskedge=[maskedge;puax(1) pArea{idxSep(1)}(idxstpt(idxSep(1)),2);...
                        puax(1) puax(3);puax(2) puax(3);puax(2) puax(4);puax(1) puax(4)];
            end

            % Save X, Y data
            dlmwrite('maskX.txt',maskedge(:,1),'precision','%.4f');
            dlmwrite('maskY.txt',maskedge(:,2),'precision','%.4f');


        elseif ext =='.csv'           
            c =[];
                try
                    c=csvread(maskFilePath{1}); 
                    disp('.csv file has no header line') ;                   
                end
            if isempty(c)
                c=csvread(maskFilePath{1},1); 
                disp('.csv file has header line') ;
                
                if ~isempty(c(:,3))
                    msg='.csv file should have only two columns (X, Y)';
                else
                    % Save X, Y data
                    dlmwrite('maskX.txt',c(:,1),'precision','%.4f');
                    dlmwrite('maskY.txt',c(:,2),'precision','%.4f');
                end
            end
        else
            msg = sprintf('Inappropriate mask file. Only .csv (X,Y vector) and shape (.shp) files are acceptable');         
        end
        
    else 
        msg = ' Mask File Reading Error ';
    end
    if ~isempty(msg)
        h = errordlg(sprintf([char(msg),'\nPlease select Mask file again']),'BMEGUI: Mask File Error');
    end
    end
    
    
