% SSC SDK - MATLAB example
%
% example script to run PVWatts

clear

ssccall('load');

% create a data container to store all the variables
data = ssccall('data_create');

% setup the system parameters
ssccall('data_set_string', data, 'file_name', 'abilene.tm2');
ssccall('data_set_number', data, 'system_size', 4);
ssccall('data_set_number', data, 'derate', 0.77);
ssccall('data_set_number', data, 'track_mode', 0);
ssccall('data_set_number', data, 'tilt', 30);
ssccall('data_set_number', data, 'azimuth', 180);

% create the PVWatts module
module = ssccall('module_create', 'pvwattsv1');

% run the module
ok = ssccall('module_exec', module, data);
if ok,
    % if successful, retrieve the hourly AC generation data and print
    % annual kWh on the screen
    ac = ssccall('data_get_array', data, 'ac');
    disp(sprintf('pvwatts: %.2f kWh',sum(ac)/1000.0));
else
    % if it failed, print all the errors
    disp('pvwattsv1 errors:');
    ii=0;
    while 1,
        err = ssccall('module_log', module, ii);
        if strcmp(err,''),
            break;
        end
        disp( err );
        ii=ii+1;
    end
end

% free the PVWatts module that we created
ssccall('module_free', module);

% release the data container and all of its variables
ssccall('data_free', data);

% unload the library
ssccall('unload');