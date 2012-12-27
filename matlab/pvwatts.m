clear

data = ssc('data_create');

ssc('data_set_string', data, 'file_name', 'c:/sam/2012.5.11/exelib/climate_files/TX Abilene.tm2');
ssc('data_set_number', data, 'system_size', 4);
ssc('data_set_number', data, 'derate', 0.77);
ssc('data_set_number', data, 'track_mode', 0);
ssc('data_set_number', data, 'tilt', 30);
ssc('data_set_number', data, 'azimuth', 180);

ssc('exec_simple', 'pvwattsv1', data);

ac = ssc('data_get_array', data, 'ac');
disp(sprintf('pvwatts: %.2f kWh',sum(ac)/1000.0));


ssc('data_free', data);