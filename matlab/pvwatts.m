clear
ssccall('load');

data = ssccall('data_create');

ssccall('data_set_string', data, 'file_name', 'c:/sam/2012.5.11/exelib/climate_files/TX Abilene.tm2');
ssccall('data_set_number', data, 'system_size', 4);
ssccall('data_set_number', data, 'derate', 0.77);
ssccall('data_set_number', data, 'track_mode', 0);
ssccall('data_set_number', data, 'tilt', 30);
ssccall('data_set_number', data, 'azimuth', 180);

module = ssccall('module_create', 'pvwattsv1');

ok = ssccall('module_exec', module, data);
if ok,
    ac = ssccall('data_get_array', data, 'ac');
    disp(sprintf('pvwatts: %.2f kWh',sum(ac)/1000.0));
else
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
ssccall('module_free', module);

ssccall('data_free', data);

ssccall('unload');