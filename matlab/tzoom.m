function tzoom()

    unit = 1e-3;       % meters per unit m/u aka base unit size
    mgsm = 1e-3;       % meters per grid m/g
    mgsu = mgsm/unit;  % units per grid u/g
    mgsp = 100;        % pixels per grid px/g
    kppu = mgsp/mgsu;  % pixels per unit px/u
    kgpu = 1;          % metric grids per unit g/u
    
    zoombase  = 2;     % doubling
    baseevery = 5;     % How many zooms for doubling (zoombasing)
    kmpm = 5;          % minor grids per major
    
    zoomlevel = (0:20)';
    zoomexp = zoomlevel / baseevery;
    ppu = kppu * zoombase .^ zoomexp; % px/u
    
    % Major spacing is related to zoombase only, so changing kmpm does not
    % affect major spacing.  Minor spacing is then related to major spacing
    % by kmpm.
    
    zoomexpstep = floor(zoomexp);
    majspacingu = 1 ./ (kgpu * zoombase .^ zoomexpstep); % 1/(g/u) = u/g = units per grid
    minspacingu = majspacingu / kmpm;
    majspacingpx = majspacingu .* ppu; % u/g * px/u = px/g
    minspacingpx = (minspacingu .* ppu) * (1:(kmpm-1));
    
    figure(1); clf; hold on;
    plot(ppu);
    plot(majspacingpx);
    plot(minspacingpx);
    ylim([0 1000]);
    

end




