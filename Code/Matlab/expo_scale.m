function [scaleOut, centerOut] = expo_scale(DATA, scale, center)

if (isa(scale,'char')),
    if (strcmp(lower(scale),'ss1')),
        if(isa(center,'logical') && center),
            center= mean(DATA,1);
        elseif(isa(center,'logical') && ~center),
            center = zeros(1,size(DATA,2));
        end
        scale=std(DATA,1).*(sqrt(size(DATA,1)-1));
    elseif (strcmp(lower(scale),'sd')),
        center = zeros(1, size(DATA,2));
        scale = std(DATA,1);
    elseif (strcmp(lower(scale),'rms')),
        if(isa(center,'logical') && ~center),
            center =false;
            scale = true;
        elseif(isa(center,'logical') && center),
            center =true;
            scale = true;
        end
    elseif (strcmp(lower(scale),'z')),
        center = true;
        scale = true;
    else
        center = true;
        scale = true;
        WARNING('Something is wrong with "center" and "scale". "center" and "scale" both set to TRUE.')
    end
end     % the end of : if (isa(scale,'char'))


if (~isa(center,'logical') && ~(isa(center,'numeric') && length(center)==size(DATA,2))),
    center=true;
    WARNING('Something is wrong with "center". "center" set to TRUE.');
end

if (~isa(scale,'logical') && ~(isa(scale,'numeric') && length(scale)==size(DATA,2))),
    scale=true;
    WARNING('Something is wrong with "scale". "scale" set to TRUE.');
end


scaleOut = std(DATA,0,1);
centerOut = mean(DATA,1);

[I J]=size(DATA);
if (isempty(centerOut)),  % make sure this works
    centerOut = zeros(1, J);
end
if (isempty(scaleOut)),  % is it ones ?
    scaleOut = ones(1, J);
end
end