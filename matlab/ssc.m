function [result] = ssc(action, arg0, arg1, arg2 )
% SAM Simulation Core (SSC) MATLAB API
% (c) 2012 National Renewable Energy Laboratory
ssclib = 'ssc32';
if ( strcmp(computer(), 'PCWIN64') )
    ssclib = 'ssc64';
end

if strcmp(action,'load')
    if ~libisloaded(ssclib)
        loadlibrary(ssclib,'sscapi.h');
    end

elseif strcmp(action,'unload')
    if libisloaded(ssclib)
        unloadlibrary(ssclib)    
    end

elseif strcmp(action,'version')
    result = calllib(ssclib,'ssc_version');

elseif strcmp(action,'build_info')
    result = calllib(ssclib, 'ssc_build_info');

elseif strcmp(action,'data_create')
    result = calllib(ssclib, 'ssc_data_create');
 
elseif strcmp(action,'data_free')
    result = calllib(ssclib, 'ssc_data_free', arg0);
   
elseif strcmp(action,'data_unassign')
    result = calllib(ssclib, 'ssc_data_unassign', arg0, arg1);
    
elseif strcmp(action,'data_query')
    result = calllib(ssclib, 'ssc_data_query', arg0, arg1 );
    
elseif strcmp(action,'data_first')
    result = calllib(ssclib, 'ssc_data_first', arg0 );
    
elseif strcmp(action,'data_next')
    result = calllib(ssclib, 'ssc_data_next', arg0 );
    
elseif strcmp(action,'data_set_string')
    result = calllib(ssclib, 'ssc_data_set_string', arg0, arg1, arg2 );
    
elseif strcmp(action,'data_set_number')
    result = calllib(ssclib, 'ssc_data_set_number', arg0, arg1, single(arg2) );
    
elseif strcmp(action,'data_set_array')
    len = length(arg2);
    arr = libpointer( 'singlePtr', arg2 );
    result = calllib(ssclib,'ssc_data_set_array',arg0,arg1,arr,len);
    
elseif strcmp(action,'data_set_matrix')
    [nr nc] = size(arg2);
    mat = zeros(nr*nc, 1);
    ii = 1;
    for r=1:nr,
        for c=1:nc,
            mat(ii) = arg2(r,c);
            ii=ii+1;
        end
    end
	arr = libpointer( 'singlePtr', mat );
    result = calllib(ssclib,'ssc_data_set_matrix',arg0,arg1,arr,nr,nc);
            
elseif strcmp(action,'data_set_table')
    result = calllib(ssclib,'ssc_data_set_table',arg0,arg1,arg2);
    
elseif strcmp(action,'data_get_string')
    result = calllib(ssclib,'ssc_data_get_string',arg0,arg1);
    
elseif strcmp(action,'data_get_number')
     p = libpointer('singlePtr',0);
     calllib(ssclib,'ssc_data_get_number', arg0,arg1,p);
     result = get(p,'Value');
    
elseif strcmp(action,'data_get_array')
    p_count = libpointer('int32Ptr',0);   
    [xobj] = calllib(ssclib,'ssc_data_get_array',arg0,arg1,p_count);
    setdatatype(xobj,'int32Ptr',p_count.Value,1);
    len = p_count.Value;
    result = zeros( len, 1 );
    for i=1:len,
        pidx = xobj+(i-1);
        setdatatype(pidx,'singlePtr',1,1);
        result(i) = pidx.Value;
    end
    
elseif strcmp(action,'data_get_matrix')
    p_rows = libpointer('int32Ptr',0);
    p_cols = libpointer('int32Ptr',0);
    [xobj] = calllib(ssclib,'ssc_data_get_matrix',arg0,arg1,p_rows,p_cols);
    setdatatype(xobj,'int32Ptr',p_rows.Value,1);
    setdatatype(xobj,'int32Ptr',p_cols.Value,1);
    nrows = p_rows.Value;
    ncols = p_cols.Value;
    if ( nrows*ncols > 0 )
        result = zeros( nrows, ncols );
        ii=1;
        for r=1:nrows,
            for c=1:ncols,
                pidx = xobj+(ii-1);
                setdatatype(pidx,'singlePtr',1,1);
                result(r,c) = pidx.Value;
                ii=ii+1;
            end
        end
    end
    
elseif strcmp(action,'data_get_table')
    result = calllib(ssclib,'ssc_data_get_table',arg0,arg1);
    
elseif strcmp(action,'module_entry')
    result = calllib(ssclib,'ssc_module_entry',arg0);
    
elseif strcmp(action,'entry_name')
    result = calllib(ssclib,'ssc_entry_name',arg0);
    
elseif strcmp(action,'entry_description')
    result = calllib(ssclib,'ssc_entry_description',arg0);
    
elseif strcmp(action,'entry_version')
    result = calllib(ssclib,'ssc_entry_version',arg0);
    
elseif strcmp(action,'exec_simple')
    result = calllib(ssclib,'ssc_module_exec_simple',arg0,arg1);
    
elseif strcmp(action,'module_log')
    p_type = libpointer('int32Ptr',1);
    p_time = libpointer('singlePtr',1);
    result = calllib(ssclib,'ssc_module_log', arg0, p_type, p_time);
   
% elseif strcmp(action,'model_count')
%     result = calllib(ssclib,'ssc_get_model_count');
% 
% elseif strcmp(action,'model_name')
%     result = calllib(ssclib,'ssc_get_model_name',arg0);
% 
% elseif strcmp(action,'create_context')
%     result = calllib(ssclib,'ssc_create_context',arg0);
% 
% elseif strcmp(action,'free_context')
%     result = calllib(ssclib,'ssc_free_context',arg0);
% 
% elseif strcmp(action,'switch_context')
%     result = calllib(ssclib,'ssc_switch_context',arg0,arg1);
% 
% elseif strcmp(action,'get_input_count')
%     result = calllib(ssclib,'ssc_get_input_count',arg0);
% 
% elseif strcmp(action,'get_output_count')
%     result = calllib(ssclib,'ssc_get_output_count',arg0);
% 
% elseif strcmp(action,'get_input_name')
%     p_type = libpointer('int32Ptr',-1);
%     result = calllib(ssclib,'ssc_get_input',arg0,arg1,p_type);
%     
% elseif strcmp(action,'get_input_type')
%     p_type = libpointer('int32Ptr',-1);
%     calllib(ssclib,'ssc_get_input',arg0,arg1,p_type);
%     result = get(p_type,'Value');
%     
% elseif strcmp(action,'get_input_desc')
%     result = calllib(ssclib,'ssc_get_input_desc',arg0,arg1);
%     
% elseif strcmp(action,'get_output_name')
%     p_type = libpointer('int32Ptr',-1);
%     result = calllib(ssclib,'ssc_get_output',arg0,arg1,p_type);
%     
% elseif strcmp(action,'get_output_type')
%     p_type = libpointer('int32Ptr',-1);
%     calllib(ssclib,'ssc_get_output',arg0,arg1,p_type);
%     result = get(p_type,'Value');
%     
% elseif strcmp(action,'get_output_desc')
%     result = calllib(ssclib,'ssc_get_output_desc',arg0,arg1);
%     
% elseif strcmp(action,'set_i')
%     result = calllib(ssclib,'ssc_set_i',arg0,arg1,int32(arg2));
%     
% elseif strcmp(action,'set_ia')
%     ia = zeros(length(arg2),1);
%     for i = 1:length(arg2)
%         ia(i) = int32(arg2(i));
%     end
%     result = calllib(ssclib,'ssc_set_ia',arg0,arg1,ia);
% elseif strcmp(action,'set_d')
%     result = calllib(ssclib,'ssc_set_d',arg0,arg1,double(arg2));
%     
% elseif strcmp(action,'set_da')
%     da = zeros(length(arg2),1);
%     for i=1:length(arg2)
%         da(i) = double(arg2(i));
%     end
%     result = calllib(ssclib,'ssc_set_da',arg0,arg1,da);
%     
% elseif strcmp(action,'set_s')
%     result = calllib(ssclib,'ssc_set_s',arg0,arg1,arg2);
%     
% elseif strcmp(action,'get_i')
%     p_i = libpointer('int32Ptr',0);
%     calllib(ssclib,'ssc_get_i', arg0,arg1,p_i);
%     result = get(p_i,'Value');
%     
% elseif strcmp(action,'get_ia')
%     p_count = libpointer('int32Ptr',0);   
%     [xobj, xval] = calllib(ssclib,'ssc_get_ia',arg0,arg1,p_count);
%     setdatatype(xobj,'int32Ptr',p_count.Value,1);
%     result = zeros( p_count.Value, 1);
%     for i=1:p_count.Value
%         pidx = xobj+(i-1);
%         setdatatype(pidx,'int32Ptr',1,1);
%         result(i) = pidx.Value;
%     end
%     
% elseif strcmp(action,'get_d')
%     p_d = libpointer('doublePtr',0);
%     calllib(ssclib,'ssc_get_d', arg0,arg1,p_d);
%     result = p_d.Value;
%     
% elseif strcmp(action,'get_da')
%     p_count = libpointer('int32Ptr',0);   
%     [xobj, xval] = calllib(ssclib,'ssc_get_da',arg0,arg1,p_count);
%     setdatatype(xobj,'doublePtr',p_count.Value,1);
%     result = zeros( p_count.Value, 1);
%     for i=1:p_count.Value
%         pidx = xobj+(i-1);
%         setdatatype(pidx,'doublePtr',1,1);
%         result(i) = pidx.Value;
%     end
%     
% elseif strcmp(action,'get_s')   
%     result = calllib(ssclib,'ssc_get_s',arg0,arg1);
%     
% elseif strcmp(action,'precheck')
%     result = calllib(ssclib,'ssc_precheck',arg0);
%     
% elseif strcmp(action,'run')
%     result = calllib(ssclib,'ssc_run', arg0);
%     
% elseif strcmp(action,'message_count')
%     result = calllib(ssclib,'ssc_message_count', arg0);
%     
% elseif strcmp(action,'get_message')
%     result = calllib(ssclib,'ssc_get_message',arg0,arg1);
    
else
    disp('ssc: bad action')
    result = 0
end

