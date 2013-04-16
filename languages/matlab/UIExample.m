function varargout = UIExample(varargin)
% UIEXAMPLE MATLAB code for UIExample.fig
%      UIEXAMPLE, by itself, creates a new UIEXAMPLE or raises the existing
%      singleton*.
%
%      H = UIEXAMPLE returns the handle to a new UIEXAMPLE or the handle to
%      the existing singleton*.
%
%      UIEXAMPLE('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in UIEXAMPLE.M with the given input arguments.
%
%      UIEXAMPLE('Property','Value',...) creates a new UIEXAMPLE or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before UIExample_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to UIExample_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES

% Edit the above text to modify the response to help UIExample

% Last Modified by GUIDE v2.5 16-Apr-2013 04:37:20

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
                   'gui_Singleton',  gui_Singleton, ...
                   'gui_OpeningFcn', @UIExample_OpeningFcn, ...
                   'gui_OutputFcn',  @UIExample_OutputFcn, ...
                   'gui_LayoutFcn',  [] , ...
                   'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before UIExample is made visible.
function UIExample_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to UIExample (see VARARGIN)

% Choose default command line output for UIExample
handles.output = hObject;

% Update handles structure
guidata(hObject, handles);

% UIWAIT makes UIExample wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = UIExample_OutputFcn(hObject, eventdata, handles) 
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;



function txtData_Callback(hObject, eventdata, handles)
% hObject    handle to txtData (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hints: get(hObject,'String') returns contents of txtData as text
%        str2double(get(hObject,'String')) returns contents of txtData as a double


% --- Executes during object creation, after setting all properties.
function txtData_CreateFcn(hObject, eventdata, handles)
% hObject    handle to txtData (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called

% Hint: edit controls usually have a white background on Windows.
%       See ISPC and COMPUTER.
if ispc && isequal(get(hObject,'BackgroundColor'), get(0,'defaultUicontrolBackgroundColor'))
    set(hObject,'BackgroundColor','white');
end


% --- Executes on button press in btnVersion.
function btnVersion_Callback(hObject, eventdata, handles)
% hObject    handle to btnVersion (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
sscAPI = SSC.API();
set(handles.txtData,'String',{sprintf('Version = %d',sscAPI.Version);sscAPI.BuildInfo});


% --- Executes on button press in btnModuleList.
function btnModuleList_Callback(hObject, eventdata, handles)
% hObject    handle to btnModuleList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
sscEntry = SSC.Entry();
names = {};
while (sscEntry.Get())
    module_name = sscEntry.Name();
    description = sscEntry.Description();
    version = sscEntry.Version();
    names{end+1} = sprintf('Module: %s, version: %d', module_name, version );
    names{end+1} = description ;
end        
set(handles.txtData,'String',names);


% --- Executes on button press in btnModuleAndVariables.
function btnModuleAndVariables_Callback(hObject, eventdata, handles)
% hObject    handle to btnModuleAndVariables (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
sscEntry = SSC.Entry();
names = {};
while (sscEntry.Get())
    moduleName = sscEntry.Name();
    description = sscEntry.Description();
    version = sscEntry.Version();
    names{end+1} = sprintf('Module: %s, version: %d', moduleName, version );
    names{end+1} = description ;

    sscModule = SSC.Module(moduleName);
    sscInfo = SSC.Info(sscModule);

    while (sscInfo.Get())
        names{end+1} = sprintf('\t%s: "%s" ["%s"] %s (%s)',sscInfo.VariableType(), sscInfo.Name(), sscInfo.DataType(), sscInfo.Label(), sscInfo.Units());
    end
end
set(handles.txtData,'String',names);


% --- Executes on button press in btnTestArrays.
function btnTestArrays_Callback(hObject, eventdata, handles)
% hObject    handle to btnTestArrays (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
names = {};
sscData = SSC.Data();
arr = [];
for i = 1:10
    arr(i) = i / 10.0;
end
sscData.SetArray('TestArray', arr);

retArray = sscData.GetArray('TestArray');

names{end+1} = 'Testing SetArray and GetArray';
for i = 1:10
    names{end+1} = sprintf('\treturned array element: %d = %g',i, retArray(i));
end
set(handles.txtData,'String',names);


% --- Executes on button press in btnTestMatrices.
function btnTestMatrices_Callback(hObject, eventdata, handles)
% hObject    handle to btnTestMatrices (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
names = {};
sscData = SSC.Data();
matrix = [ 1 2 ; 3 4 ; 5 6 ; 7 8; 9 10];
sscData.SetMatrix('TestMatrix', matrix);

retMatrix = sscData.GetMatrix('TestMatrix');
[nrows ncols] = size(retMatrix);
names{end+1} = sprintf('Testing SetMatrix and GetMatrix size %d x %d', nrows,ncols);
for i = 1: nrows
    for j = 1: ncols
        names{end+1} = sprintf('\treturned matrix element: (%d,%d) = %g', i,j, retMatrix(i,j));
    end
end
set(handles.txtData,'String',names);


% --- Executes on button press in btnPVWatts.
function btnPVWatts_Callback(hObject, eventdata, handles)
% hObject    handle to btnPVWatts (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
names = {};
sscData = SSC.Data();
sscData.SetString('file_name', '../../examples/abilene.tm2');
sscData.SetNumber('system_size', 4.0);
sscData.SetNumber('derate', 0.77);
sscData.SetNumber('track_mode', 0);
sscData.SetNumber('tilt', 20);
sscData.SetNumber('azimuth', 180);

mod = SSC.Module('pvwattsv1');
if (mod.Exec(sscData)),
    tot = sscData.GetNumber('ac_annual');
    ac = sscData.GetArray('ac_monthly');
    for i = 1:size(ac)
        names{end+1} = sprintf('[%d]: %g kWh', i,ac(i));
    end
    names{end+1} = sprintf('AC total: %g kWh', tot);
    names{end+1} = 'PVWatts test OK';
else
    idx = 0;
    [result, msg, type, time] = mod.Log(idx);
    while (result)
         names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
         idx = idx + 1;
        [result, msg, type, time] = mod.Log(idx);
    end
    names{end+1} = 'PVWatts example failed';
end
set(handles.txtData,'String',names);

% --- Executes on button press in bntPVWattsFunc.
function bntPVWattsFunc_Callback(hObject, eventdata, handles)
% hObject    handle to bntPVWattsFunc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
names = {};
sscData = SSC.Data();
sscModule = SSC.Module('pvwattsfunc');
sscData.SetNumber('year', 1970); % general year (tiny effect in sun position)
sscData.SetNumber('month', 1); % 1-12
sscData.SetNumber('day', 1); %1-number of days in month
sscData.SetNumber('hour', 9); % 0-23
sscData.SetNumber('minute', 30); % minute of the hour (typically 30 min for midpoint calculation)
sscData.SetNumber('lat', 33.4); % latitude, degrees
sscData.SetNumber('lon', -112); % longitude, degrees
sscData.SetNumber('tz', -7); % timezone from gmt, hours
sscData.SetNumber('time_step', 1); % time step, hours

% solar and weather data
sscData.SetNumber('beam', 824); % beam (DNI) irradiance, W/m2
sscData.SetNumber('diffuse', 29); % diffuse (DHI) horizontal irradiance, W/m2
sscData.SetNumber('tamb', 9.4); % ambient temp, degree C
sscData.SetNumber('wspd', 2.1); % wind speed, m/s
sscData.SetNumber('snow', 0); % snow depth, cm (0 is default - when there is snow, ground reflectance is increased.  assumes panels have been cleaned off)

% system specifications
sscData.SetNumber('system_size', 4); % system DC nameplate rating (kW)
sscData.SetNumber('derate', 0.77); % derate factor
sscData.SetNumber('track_mode', 0); % tracking mode 0=fixed, 1=1axis, 2=2axis
sscData.SetNumber('azimuth', 180); % azimuth angle 0=north, 90=east, 180=south, 270=west
sscData.SetNumber('tilt', 20); % tilt angle from horizontal 0=flat, 90=vertical


% previous timestep values of cell temperature and POA
sscData.SetNumber('tcell', 6.94); % calculated cell temperature from previous timestep, degree C, (can default to ambient for morning or if you don't know)
sscData.SetNumber('poa', 84.5); % plane of array irradiance (W/m2) from previous time step

if (sscModule.Exec(sscData))
    poa = sscData.GetNumber('poa');
    tcell = sscData.GetNumber('tcell');
    dc = sscData.GetNumber('dc');
    ac = sscData.GetNumber('ac');
    names{end+1} = sprintf('poa: %g W/m2', poa);
    names{end+1} = sprintf('tcell: %g C', tcell);
    names{end+1} = sprintf('dc: %g W', dc);
    names{end+1} = sprintf('ac: %g W', ac);
end
set(handles.txtData,'String',names);


% --- Executes on button press in btnPVSamV1.
function btnPVSamV1_Callback(hObject, eventdata, handles)
% hObject    handle to btnPVSamV1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

names={};
data = SSC.Data();

% pvsamv1 input variables
data.SetString( 'weather_file', '../../examples/AZ Phoenix.tm2' );
data.SetNumber( 'use_wf_albedo', 1 );
data.SetNumber( 'albedo', 0.2 );
data.SetNumber( 'irrad_mode', 0 );
data.SetNumber( 'sky_model', 2 );
data.SetNumber( 'ac_derate', 0.99 );
data.SetNumber( 'modules_per_string', 9 );
data.SetNumber( 'strings_in_parallel', 2 );
data.SetNumber( 'inverter_count', 1 );
data.SetNumber( 'enable_mismatch_vmax_calc', 0 );
data.SetNumber( 'subarray1_tilt', 20 );
data.SetNumber( 'subarray1_tilt_eq_lat', 0 );
data.SetNumber( 'subarray1_azimuth', 180 );
data.SetNumber( 'subarray1_track_mode', 0 );
data.SetNumber( 'subarray1_rotlim', 45 );
data.SetNumber( 'subarray1_enable_backtracking', 0 );
data.SetNumber( 'subarray1_btwidth', 2 );
data.SetNumber( 'subarray1_btspacing', 1 );
data.SetMatrix( 'subarray1_shading_mxh', [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ; 
 0.2, 0.2, 0.2, 0.2, 0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5 ] );
data.SetArray( 'subarray1_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray1_derate', 0.955598 );
data.SetNumber( 'subarray2_enable', 0 );
data.SetNumber( 'subarray2_nstrings', 0 );
data.SetNumber( 'subarray2_tilt', 20 );
data.SetNumber( 'subarray2_tilt_eq_lat', 0 );
data.SetNumber( 'subarray2_azimuth', 180 );
data.SetNumber( 'subarray2_track_mode', 0 );
data.SetNumber( 'subarray2_rotlim', 45 );
data.SetNumber( 'subarray2_enable_backtracking', 0 );
data.SetNumber( 'subarray2_btwidth', 2 );
data.SetNumber( 'subarray2_btspacing', 1 );
data.SetArray( 'subarray2_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray2_derate', 0.955598 );
data.SetNumber( 'subarray3_enable', 0 );
data.SetNumber( 'subarray3_nstrings', 0 );
data.SetNumber( 'subarray3_tilt', 20 );
data.SetNumber( 'subarray3_tilt_eq_lat', 0 );
data.SetNumber( 'subarray3_azimuth', 180 );
data.SetNumber( 'subarray3_track_mode', 0 );
data.SetNumber( 'subarray3_rotlim', 45 );
data.SetNumber( 'subarray3_enable_backtracking', 0 );
data.SetNumber( 'subarray3_btwidth', 2 );
data.SetNumber( 'subarray3_btspacing', 1 );
data.SetArray( 'subarray3_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray3_derate', 0.955598 );
data.SetNumber( 'subarray4_enable', 0 );
data.SetNumber( 'subarray4_nstrings', 0 );
data.SetNumber( 'subarray4_tilt', 20 );
data.SetNumber( 'subarray4_tilt_eq_lat', 0 );
data.SetNumber( 'subarray4_azimuth', 180 );
data.SetNumber( 'subarray4_track_mode', 0 );
data.SetNumber( 'subarray4_rotlim', 45 );
data.SetNumber( 'subarray4_enable_backtracking', 0 );
data.SetNumber( 'subarray4_btwidth', 2 );
data.SetNumber( 'subarray4_btspacing', 1 );
data.SetArray( 'subarray4_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray4_derate', 0.955598 );
data.SetNumber( 'module_model', 1 );
data.SetNumber( 'spe_area', 0.74074 );
data.SetNumber( 'spe_rad0', 200 );
data.SetNumber( 'spe_rad1', 400 );
data.SetNumber( 'spe_rad2', 600 );
data.SetNumber( 'spe_rad3', 800 );
data.SetNumber( 'spe_rad4', 1000 );
data.SetNumber( 'spe_eff0', 13.5 );
data.SetNumber( 'spe_eff1', 13.5 );
data.SetNumber( 'spe_eff2', 13.5 );
data.SetNumber( 'spe_eff3', 13.5 );
data.SetNumber( 'spe_eff4', 13.5 );
data.SetNumber( 'spe_reference', 4 );
data.SetNumber( 'spe_module_structure', 0 );
data.SetNumber( 'spe_a', -3.56 );
data.SetNumber( 'spe_b', -0.075 );
data.SetNumber( 'spe_dT', 3 );
data.SetNumber( 'spe_temp_coeff', -0.5 );
data.SetNumber( 'spe_fd', 1 );
data.SetNumber( 'cec_area', 1.244 );
data.SetNumber( 'cec_a_ref', 1.9816 );
data.SetNumber( 'cec_adjust', 20.8 );
data.SetNumber( 'cec_alpha_sc', 0.002651 );
data.SetNumber( 'cec_beta_oc', -0.14234 );
data.SetNumber( 'cec_gamma_r', -0.407 );
data.SetNumber( 'cec_i_l_ref', 5.754 );
data.SetNumber( 'cec_i_mp_ref', 5.25 );
data.SetNumber( 'cec_i_o_ref', 1.919e-010 );
data.SetNumber( 'cec_i_sc_ref', 5.75 );
data.SetNumber( 'cec_n_s', 72 );
data.SetNumber( 'cec_r_s', 0.105 );
data.SetNumber( 'cec_r_sh_ref', 160.48 );
data.SetNumber( 'cec_t_noct', 49.2 );
data.SetNumber( 'cec_v_mp_ref', 41 );
data.SetNumber( 'cec_v_oc_ref', 47.7 );
data.SetNumber( 'cec_temp_corr_mode', 0 );
data.SetNumber( 'cec_standoff', 6 );
data.SetNumber( 'cec_height', 0 );
data.SetNumber( 'cec_mounting_config', 0 );
data.SetNumber( 'cec_heat_transfer', 0 );
data.SetNumber( 'cec_mounting_orientation', 0 );
data.SetNumber( 'cec_gap_spacing', 0.05 );
data.SetNumber( 'cec_module_width', 1 );
data.SetNumber( 'cec_module_length', 1.244 );
data.SetNumber( 'cec_array_rows', 1 );
data.SetNumber( 'cec_array_cols', 10 );
data.SetNumber( 'cec_backside_temp', 20 );
data.SetNumber( '6par_celltech', 1 );
data.SetNumber( '6par_vmp', 30 );
data.SetNumber( '6par_imp', 6 );
data.SetNumber( '6par_voc', 37 );
data.SetNumber( '6par_isc', 7 );
data.SetNumber( '6par_bvoc', -0.11 );
data.SetNumber( '6par_aisc', 0.004 );
data.SetNumber( '6par_gpmp', -0.41 );
data.SetNumber( '6par_nser', 60 );
data.SetNumber( '6par_area', 1.3 );
data.SetNumber( '6par_tnoct', 46 );
data.SetNumber( '6par_standoff', 6 );
data.SetNumber( '6par_mounting', 0 );
data.SetNumber( 'snl_module_structure', 0 );
data.SetNumber( 'snl_a', -3.62 );
data.SetNumber( 'snl_b', -0.075 );
data.SetNumber( 'snl_dtc', 3 );
data.SetNumber( 'snl_ref_a', -3.62 );
data.SetNumber( 'snl_ref_b', -0.075 );
data.SetNumber( 'snl_ref_dT', 3 );
data.SetNumber( 'snl_fd', 1 );
data.SetNumber( 'snl_a0', 0.94045 );
data.SetNumber( 'snl_a1', 0.052641 );
data.SetNumber( 'snl_a2', -0.0093897 );
data.SetNumber( 'snl_a3', 0.00072623 );
data.SetNumber( 'snl_a4', -1.9938e-005 );
data.SetNumber( 'snl_aimp', -0.00038 );
data.SetNumber( 'snl_aisc', 0.00061 );
data.SetNumber( 'snl_area', 1.244 );
data.SetNumber( 'snl_b0', 1 );
data.SetNumber( 'snl_b1', -0.002438 );
data.SetNumber( 'snl_b2', 0.0003103 );
data.SetNumber( 'snl_b3', -1.246e-005 );
data.SetNumber( 'snl_b4', 2.112e-007 );
data.SetNumber( 'snl_b5', -1.359e-009 );
data.SetNumber( 'snl_bvmpo', -0.139 );
data.SetNumber( 'snl_bvoco', -0.136 );
data.SetNumber( 'snl_c0', 1.0039 );
data.SetNumber( 'snl_c1', -0.0039 );
data.SetNumber( 'snl_c2', 0.291066 );
data.SetNumber( 'snl_c3', -4.73546 );
data.SetNumber( 'snl_c4', 0.9942 );
data.SetNumber( 'snl_c5', 0.0058 );
data.SetNumber( 'snl_c6', 1.0723 );
data.SetNumber( 'snl_c7', -0.0723 );
data.SetNumber( 'snl_impo', 5.25 );
data.SetNumber( 'snl_isco', 5.75 );
data.SetNumber( 'snl_ixo', 5.65 );
data.SetNumber( 'snl_ixxo', 3.85 );
data.SetNumber( 'snl_mbvmp', 0 );
data.SetNumber( 'snl_mbvoc', 0 );
data.SetNumber( 'snl_n', 1.221 );
data.SetNumber( 'snl_series_cells', 72 );
data.SetNumber( 'snl_vmpo', 40 );
data.SetNumber( 'snl_voco', 47.7 );
data.SetNumber( 'inverter_model', 1 );
data.SetNumber( 'inv_spe_efficiency', 95 );
data.SetNumber( 'inv_spe_power_ac', 4000 );
data.SetNumber( 'inv_snl_c0', -6.57929e-006 );
data.SetNumber( 'inv_snl_c1', 4.72925e-005 );
data.SetNumber( 'inv_snl_c2', 0.00202195 );
data.SetNumber( 'inv_snl_c3', 0.000285321 );
data.SetNumber( 'inv_snl_paco', 4000 );
data.SetNumber( 'inv_snl_pdco', 4186 );
data.SetNumber( 'inv_snl_pnt', 0.17 );
data.SetNumber( 'inv_snl_pso', 19.7391 );
data.SetNumber( 'inv_snl_vdco', 310.67 );
data.SetNumber( 'inv_snl_vdcmax', 0 );
data.SetNumber( 'self_shading_enabled', 0 );
data.SetNumber( 'self_shading_length', 1.84844 );
data.SetNumber( 'self_shading_width', 0.673 );
data.SetNumber( 'self_shading_mod_orient', 1 );
data.SetNumber( 'self_shading_str_orient', 0 );
data.SetNumber( 'self_shading_ncellx', 6 );
data.SetNumber( 'self_shading_ncelly', 12 );
data.SetNumber( 'self_shading_ndiode', 3 );
data.SetNumber( 'self_shading_nmodx', 2 );
data.SetNumber( 'self_shading_nstrx', 1 );
data.SetNumber( 'self_shading_nmody', 3 );
data.SetNumber( 'self_shading_nrows', 3 );
data.SetNumber( 'self_shading_rowspace', 5 );

module = SSC.Module('pvsamv1');
if (module.Exec(data))
    ac_hourly = data.GetArray('hourly_ac_net');
    ac_monthly = data.GetArray('monthly_ac_net');
    ac_annual = data.GetNumber('annual_ac_net');
    for i = 1:size(ac_monthly)
        names{end+1} = sprintf('[%d]: %g kWh', i,ac_monthly(i));
    end
    names{end+1} = sprintf('AC total: %g kWh', ac_annual);
    names{end+1} = 'PVSamV1 test OK';
else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'pvsamv1 example failed';
end
set(handles.txtData,'String',names);


% --- Executes on button press in btnPVRes.
function btnPVRes_Callback(hObject, eventdata, handles)
% hObject    handle to btnPVRes (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Flat Plate PV in Commercial market SAM defaults

names={};
data = SSC.Data();

% pvsamv1 input variables
data.SetString( 'weather_file', '../../examples/AZ Phoenix.tm2' );
data.SetNumber( 'use_wf_albedo', 1 );
data.SetNumber( 'albedo', 0.2 );
data.SetNumber( 'irrad_mode', 0 );
data.SetNumber( 'sky_model', 2 );
data.SetNumber( 'ac_derate', 0.99 );
data.SetNumber( 'modules_per_string', 9 );
data.SetNumber( 'strings_in_parallel', 2 );
data.SetNumber( 'inverter_count', 1 );
data.SetNumber( 'enable_mismatch_vmax_calc', 0 );
data.SetNumber( 'subarray1_tilt', 20 );
data.SetNumber( 'subarray1_tilt_eq_lat', 0 );
data.SetNumber( 'subarray1_azimuth', 180 );
data.SetNumber( 'subarray1_track_mode', 0 );
data.SetNumber( 'subarray1_rotlim', 45 );
data.SetNumber( 'subarray1_enable_backtracking', 0 );
data.SetNumber( 'subarray1_btwidth', 2 );
data.SetNumber( 'subarray1_btspacing', 1 );
data.SetArray( 'subarray1_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray1_derate', 0.955598 );
data.SetNumber( 'subarray2_enable', 0 );
data.SetNumber( 'subarray2_nstrings', 0 );
data.SetNumber( 'subarray2_tilt', 20 );
data.SetNumber( 'subarray2_tilt_eq_lat', 0 );
data.SetNumber( 'subarray2_azimuth', 180 );
data.SetNumber( 'subarray2_track_mode', 0 );
data.SetNumber( 'subarray2_rotlim', 45 );
data.SetNumber( 'subarray2_enable_backtracking', 0 );
data.SetNumber( 'subarray2_btwidth', 2 );
data.SetNumber( 'subarray2_btspacing', 1 );
data.SetArray( 'subarray2_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray2_derate', 0.955598 );
data.SetNumber( 'subarray3_enable', 0 );
data.SetNumber( 'subarray3_nstrings', 0 );
data.SetNumber( 'subarray3_tilt', 20 );
data.SetNumber( 'subarray3_tilt_eq_lat', 0 );
data.SetNumber( 'subarray3_azimuth', 180 );
data.SetNumber( 'subarray3_track_mode', 0 );
data.SetNumber( 'subarray3_rotlim', 45 );
data.SetNumber( 'subarray3_enable_backtracking', 0 );
data.SetNumber( 'subarray3_btwidth', 2 );
data.SetNumber( 'subarray3_btspacing', 1 );
data.SetArray( 'subarray3_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray3_derate', 0.955598 );
data.SetNumber( 'subarray4_enable', 0 );
data.SetNumber( 'subarray4_nstrings', 0 );
data.SetNumber( 'subarray4_tilt', 20 );
data.SetNumber( 'subarray4_tilt_eq_lat', 0 );
data.SetNumber( 'subarray4_azimuth', 180 );
data.SetNumber( 'subarray4_track_mode', 0 );
data.SetNumber( 'subarray4_rotlim', 45 );
data.SetNumber( 'subarray4_enable_backtracking', 0 );
data.SetNumber( 'subarray4_btwidth', 2 );
data.SetNumber( 'subarray4_btspacing', 1 );
data.SetArray( 'subarray4_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray4_derate', 0.955598 );
data.SetNumber( 'module_model', 1 );
data.SetNumber( 'spe_area', 0.74074 );
data.SetNumber( 'spe_rad0', 200 );
data.SetNumber( 'spe_rad1', 400 );
data.SetNumber( 'spe_rad2', 600 );
data.SetNumber( 'spe_rad3', 800 );
data.SetNumber( 'spe_rad4', 1000 );
data.SetNumber( 'spe_eff0', 13.5 );
data.SetNumber( 'spe_eff1', 13.5 );
data.SetNumber( 'spe_eff2', 13.5 );
data.SetNumber( 'spe_eff3', 13.5 );
data.SetNumber( 'spe_eff4', 13.5 );
data.SetNumber( 'spe_reference', 4 );
data.SetNumber( 'spe_module_structure', 0 );
data.SetNumber( 'spe_a', -3.56 );
data.SetNumber( 'spe_b', -0.075 );
data.SetNumber( 'spe_dT', 3 );
data.SetNumber( 'spe_temp_coeff', -0.5 );
data.SetNumber( 'spe_fd', 1 );
data.SetNumber( 'cec_area', 1.244 );
data.SetNumber( 'cec_a_ref', 1.9816 );
data.SetNumber( 'cec_adjust', 20.8 );
data.SetNumber( 'cec_alpha_sc', 0.002651 );
data.SetNumber( 'cec_beta_oc', -0.14234 );
data.SetNumber( 'cec_gamma_r', -0.407 );
data.SetNumber( 'cec_i_l_ref', 5.754 );
data.SetNumber( 'cec_i_mp_ref', 5.25 );
data.SetNumber( 'cec_i_o_ref', 1.919e-010 );
data.SetNumber( 'cec_i_sc_ref', 5.75 );
data.SetNumber( 'cec_n_s', 72 );
data.SetNumber( 'cec_r_s', 0.105 );
data.SetNumber( 'cec_r_sh_ref', 160.48 );
data.SetNumber( 'cec_t_noct', 49.2 );
data.SetNumber( 'cec_v_mp_ref', 41 );
data.SetNumber( 'cec_v_oc_ref', 47.7 );
data.SetNumber( 'cec_temp_corr_mode', 0 );
data.SetNumber( 'cec_standoff', 6 );
data.SetNumber( 'cec_height', 0 );
data.SetNumber( 'cec_mounting_config', 0 );
data.SetNumber( 'cec_heat_transfer', 0 );
data.SetNumber( 'cec_mounting_orientation', 0 );
data.SetNumber( 'cec_gap_spacing', 0.05 );
data.SetNumber( 'cec_module_width', 1 );
data.SetNumber( 'cec_module_length', 1.244 );
data.SetNumber( 'cec_array_rows', 1 );
data.SetNumber( 'cec_array_cols', 10 );
data.SetNumber( 'cec_backside_temp', 20 );
data.SetNumber( '6par_celltech', 1 );
data.SetNumber( '6par_vmp', 30 );
data.SetNumber( '6par_imp', 6 );
data.SetNumber( '6par_voc', 37 );
data.SetNumber( '6par_isc', 7 );
data.SetNumber( '6par_bvoc', -0.11 );
data.SetNumber( '6par_aisc', 0.004 );
data.SetNumber( '6par_gpmp', -0.41 );
data.SetNumber( '6par_nser', 60 );
data.SetNumber( '6par_area', 1.3 );
data.SetNumber( '6par_tnoct', 46 );
data.SetNumber( '6par_standoff', 6 );
data.SetNumber( '6par_mounting', 0 );
data.SetNumber( 'snl_module_structure', 0 );
data.SetNumber( 'snl_a', -3.62 );
data.SetNumber( 'snl_b', -0.075 );
data.SetNumber( 'snl_dtc', 3 );
data.SetNumber( 'snl_ref_a', -3.62 );
data.SetNumber( 'snl_ref_b', -0.075 );
data.SetNumber( 'snl_ref_dT', 3 );
data.SetNumber( 'snl_fd', 1 );
data.SetNumber( 'snl_a0', 0.94045 );
data.SetNumber( 'snl_a1', 0.052641 );
data.SetNumber( 'snl_a2', -0.0093897 );
data.SetNumber( 'snl_a3', 0.00072623 );
data.SetNumber( 'snl_a4', -1.9938e-005 );
data.SetNumber( 'snl_aimp', -0.00038 );
data.SetNumber( 'snl_aisc', 0.00061 );
data.SetNumber( 'snl_area', 1.244 );
data.SetNumber( 'snl_b0', 1 );
data.SetNumber( 'snl_b1', -0.002438 );
data.SetNumber( 'snl_b2', 0.0003103 );
data.SetNumber( 'snl_b3', -1.246e-005 );
data.SetNumber( 'snl_b4', 2.112e-007 );
data.SetNumber( 'snl_b5', -1.359e-009 );
data.SetNumber( 'snl_bvmpo', -0.139 );
data.SetNumber( 'snl_bvoco', -0.136 );
data.SetNumber( 'snl_c0', 1.0039 );
data.SetNumber( 'snl_c1', -0.0039 );
data.SetNumber( 'snl_c2', 0.291066 );
data.SetNumber( 'snl_c3', -4.73546 );
data.SetNumber( 'snl_c4', 0.9942 );
data.SetNumber( 'snl_c5', 0.0058 );
data.SetNumber( 'snl_c6', 1.0723 );
data.SetNumber( 'snl_c7', -0.0723 );
data.SetNumber( 'snl_impo', 5.25 );
data.SetNumber( 'snl_isco', 5.75 );
data.SetNumber( 'snl_ixo', 5.65 );
data.SetNumber( 'snl_ixxo', 3.85 );
data.SetNumber( 'snl_mbvmp', 0 );
data.SetNumber( 'snl_mbvoc', 0 );
data.SetNumber( 'snl_n', 1.221 );
data.SetNumber( 'snl_series_cells', 72 );
data.SetNumber( 'snl_vmpo', 40 );
data.SetNumber( 'snl_voco', 47.7 );
data.SetNumber( 'inverter_model', 1 );
data.SetNumber( 'inv_spe_efficiency', 95 );
data.SetNumber( 'inv_spe_power_ac', 4000 );
data.SetNumber( 'inv_snl_c0', -6.57929e-006 );
data.SetNumber( 'inv_snl_c1', 4.72925e-005 );
data.SetNumber( 'inv_snl_c2', 0.00202195 );
data.SetNumber( 'inv_snl_c3', 0.000285321 );
data.SetNumber( 'inv_snl_paco', 4000 );
data.SetNumber( 'inv_snl_pdco', 4186 );
data.SetNumber( 'inv_snl_pnt', 0.17 );
data.SetNumber( 'inv_snl_pso', 19.7391 );
data.SetNumber( 'inv_snl_vdco', 310.67 );
data.SetNumber( 'inv_snl_vdcmax', 0 );
data.SetNumber( 'self_shading_enabled', 0 );
data.SetNumber( 'self_shading_length', 1.84844 );
data.SetNumber( 'self_shading_width', 0.673 );
data.SetNumber( 'self_shading_mod_orient', 1 );
data.SetNumber( 'self_shading_str_orient', 0 );
data.SetNumber( 'self_shading_ncellx', 6 );
data.SetNumber( 'self_shading_ncelly', 12 );
data.SetNumber( 'self_shading_ndiode', 3 );
data.SetNumber( 'self_shading_nmodx', 2 );
data.SetNumber( 'self_shading_nstrx', 1 );
data.SetNumber( 'self_shading_nmody', 3 );
data.SetNumber( 'self_shading_nrows', 3 );
data.SetNumber( 'self_shading_rowspace', 5 );

module = SSC.Module('pvsamv1');
if (module.Exec(data))
    ac_hourly = data.GetArray('hourly_ac_net');
    ac_monthly = data.GetArray('monthly_ac_net');
    ac_annual = data.GetNumber('annual_ac_net');
    for i = 1:size(ac_monthly)
        names{end+1} = sprintf('[%d]: %g kWh', i,ac_monthly(i));
    end
    names{end+1} = sprintf('AC total: %g kWh', ac_annual);
    names{end+1} = 'PVSamV1 test OK';
else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'pvsamv1 example failed';
end

% annualoutput input variables
data.SetNumber( 'analysis_years', 25 );
data.SetArray( 'energy_availability', [ 100 ] );
data.SetArray( 'energy_degradation', [ 0.5 ] );
data.SetMatrix( 'energy_curtailment', [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] );
data.SetNumber( 'system_use_lifetime_output', 0 );
data.SetArray( 'energy_net_hourly', ac_hourly);

module = SSC.Module('annualoutput');
if (module.Exec(data))

 	net_hourly = data.GetArray('hourly_e_net_delivered');

else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'annualoutput example failed';
end

% utilityrate input variables
data.SetNumber( 'analysis_years', 25 );
data.SetArray( 'e_with_system', net_hourly);
data.SetArray( 'system_availability', [ 100 ] );
data.SetArray( 'system_degradation', [ 0.5 ] );
data.SetArray( 'load_escalation', [ 2.5 ] );
data.SetArray( 'rate_escalation', [ 2.5 ] );
data.SetNumber( 'ur_sell_eq_buy', 1 );
data.SetNumber( 'ur_monthly_fixed_charge', 0 );
data.SetNumber( 'ur_flat_buy_rate', 0.12 );
data.SetNumber( 'ur_flat_sell_rate', 0 );
data.SetNumber( 'ur_tou_enable', 0 );
data.SetNumber( 'ur_tou_p1_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p1_sell_rate', 0 );
data.SetNumber( 'ur_tou_p2_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p2_sell_rate', 0 );
data.SetNumber( 'ur_tou_p3_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p3_sell_rate', 0 );
data.SetNumber( 'ur_tou_p4_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p4_sell_rate', 0 );
data.SetNumber( 'ur_tou_p5_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p5_sell_rate', 0 );
data.SetNumber( 'ur_tou_p6_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p6_sell_rate', 0 );
data.SetNumber( 'ur_tou_p7_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p7_sell_rate', 0 );
data.SetNumber( 'ur_tou_p8_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p8_sell_rate', 0 );
data.SetNumber( 'ur_tou_p9_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p9_sell_rate', 0 );
data.SetString( 'ur_tou_sched_weekday', '111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111' );
data.SetString( 'ur_tou_sched_weekend', '111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111' );
data.SetNumber( 'ur_dc_enable', 0 );
data.SetNumber( 'ur_dc_fixed_m1', 0 );
data.SetNumber( 'ur_dc_fixed_m2', 0 );
data.SetNumber( 'ur_dc_fixed_m3', 0 );
data.SetNumber( 'ur_dc_fixed_m4', 0 );
data.SetNumber( 'ur_dc_fixed_m5', 0 );
data.SetNumber( 'ur_dc_fixed_m6', 0 );
data.SetNumber( 'ur_dc_fixed_m7', 0 );
data.SetNumber( 'ur_dc_fixed_m8', 0 );
data.SetNumber( 'ur_dc_fixed_m9', 0 );
data.SetNumber( 'ur_dc_fixed_m10', 0 );
data.SetNumber( 'ur_dc_fixed_m11', 0 );
data.SetNumber( 'ur_dc_fixed_m12', 0 );
data.SetNumber( 'ur_dc_p1', 0 );
data.SetNumber( 'ur_dc_p2', 0 );
data.SetNumber( 'ur_dc_p3', 0 );
data.SetNumber( 'ur_dc_p4', 0 );
data.SetNumber( 'ur_dc_p5', 0 );
data.SetNumber( 'ur_dc_p6', 0 );
data.SetNumber( 'ur_dc_p7', 0 );
data.SetNumber( 'ur_dc_p8', 0 );
data.SetNumber( 'ur_dc_p9', 0 );
data.SetString( 'ur_dc_sched_weekday', '444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444' );
data.SetString( 'ur_dc_sched_weekend', '444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444' );
data.SetNumber( 'ur_tr_enable', 0 );
data.SetNumber( 'ur_tr_sell_mode', 1 );
data.SetNumber( 'ur_tr_sell_rate', 0 );
data.SetNumber( 'ur_tr_s1_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s1_rate1', 0 );
data.SetNumber( 'ur_tr_s1_rate2', 0 );
data.SetNumber( 'ur_tr_s1_rate3', 0 );
data.SetNumber( 'ur_tr_s1_rate4', 0 );
data.SetNumber( 'ur_tr_s1_rate5', 0 );
data.SetNumber( 'ur_tr_s1_rate6', 0 );
data.SetNumber( 'ur_tr_s2_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s2_rate1', 0 );
data.SetNumber( 'ur_tr_s2_rate2', 0 );
data.SetNumber( 'ur_tr_s2_rate3', 0 );
data.SetNumber( 'ur_tr_s2_rate4', 0 );
data.SetNumber( 'ur_tr_s2_rate5', 0 );
data.SetNumber( 'ur_tr_s2_rate6', 0 );
data.SetNumber( 'ur_tr_s3_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s3_rate1', 0 );
data.SetNumber( 'ur_tr_s3_rate2', 0 );
data.SetNumber( 'ur_tr_s3_rate3', 0 );
data.SetNumber( 'ur_tr_s3_rate4', 0 );
data.SetNumber( 'ur_tr_s3_rate5', 0 );
data.SetNumber( 'ur_tr_s3_rate6', 0 );
data.SetNumber( 'ur_tr_s4_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s4_rate1', 0 );
data.SetNumber( 'ur_tr_s4_rate2', 0 );
data.SetNumber( 'ur_tr_s4_rate3', 0 );
data.SetNumber( 'ur_tr_s4_rate4', 0 );
data.SetNumber( 'ur_tr_s4_rate5', 0 );
data.SetNumber( 'ur_tr_s4_rate6', 0 );
data.SetNumber( 'ur_tr_s5_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s5_rate1', 0 );
data.SetNumber( 'ur_tr_s5_rate2', 0 );
data.SetNumber( 'ur_tr_s5_rate3', 0 );
data.SetNumber( 'ur_tr_s5_rate4', 0 );
data.SetNumber( 'ur_tr_s5_rate5', 0 );
data.SetNumber( 'ur_tr_s5_rate6', 0 );
data.SetNumber( 'ur_tr_s6_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s6_rate1', 0 );
data.SetNumber( 'ur_tr_s6_rate2', 0 );
data.SetNumber( 'ur_tr_s6_rate3', 0 );
data.SetNumber( 'ur_tr_s6_rate4', 0 );
data.SetNumber( 'ur_tr_s6_rate5', 0 );
data.SetNumber( 'ur_tr_s6_rate6', 0 );
data.SetNumber( 'ur_tr_sched_m1', 0 );
data.SetNumber( 'ur_tr_sched_m2', 0 );
data.SetNumber( 'ur_tr_sched_m3', 0 );
data.SetNumber( 'ur_tr_sched_m4', 0 );
data.SetNumber( 'ur_tr_sched_m5', 0 );
data.SetNumber( 'ur_tr_sched_m6', 0 );
data.SetNumber( 'ur_tr_sched_m7', 0 );
data.SetNumber( 'ur_tr_sched_m8', 0 );
data.SetNumber( 'ur_tr_sched_m9', 0 );
data.SetNumber( 'ur_tr_sched_m10', 0 );
data.SetNumber( 'ur_tr_sched_m11', 0 );
data.SetNumber( 'ur_tr_sched_m12', 0 );

module = SSC.Module('utilityrate');
if (module.Exec(data))

 % TODO: place output variables here

else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'utilityrate example failed';
end


% cashloan input variables
data.SetNumber( 'analysis_years', 25 );
data.SetNumber( 'federal_tax_rate', 28 );
data.SetNumber( 'state_tax_rate', 7 );
data.SetNumber( 'property_tax_rate', 0 );
data.SetNumber( 'prop_tax_cost_assessed_percent', 100 );
data.SetNumber( 'prop_tax_assessed_decline', 0 );
data.SetNumber( 'sales_tax_rate', 5 );
data.SetNumber( 'real_discount_rate', 8 );
data.SetNumber( 'inflation_rate', 2.5 );
data.SetNumber( 'insurance_rate', 0 );
data.SetNumber( 'system_capacity', 3.8745 );
data.SetNumber( 'system_heat_rate', 0 );
data.SetNumber( 'loan_term', 25 );
data.SetNumber( 'loan_rate', 7.5 );
data.SetNumber( 'loan_debt', 100 );
data.SetArray( 'om_fixed', [ 0 ] );
data.SetNumber( 'om_fixed_escal', 0 );
data.SetArray( 'om_production', [ 0 ] );
data.SetNumber( 'om_production_escal', 0 );
data.SetArray( 'om_capacity', [ 20 ] );
data.SetNumber( 'om_capacity_escal', 0 );
data.SetArray( 'om_fuel_cost', [ 0 ] );
data.SetNumber( 'om_fuel_cost_escal', 0 );
data.SetNumber( 'annual_fuel_usage', 0 );
data.SetNumber( 'itc_fed_amount', 0 );
data.SetNumber( 'itc_fed_amount_deprbas_fed', 0 );
data.SetNumber( 'itc_fed_amount_deprbas_sta', 0 );
data.SetNumber( 'itc_sta_amount', 0 );
data.SetNumber( 'itc_sta_amount_deprbas_fed', 0 );
data.SetNumber( 'itc_sta_amount_deprbas_sta', 0 );
data.SetNumber( 'itc_fed_percent', 30 );
data.SetNumber( 'itc_fed_percent_maxvalue', 1e+038 );
data.SetNumber( 'itc_fed_percent_deprbas_fed', 0 );
data.SetNumber( 'itc_fed_percent_deprbas_sta', 0 );
data.SetNumber( 'itc_sta_percent', 0 );
data.SetNumber( 'itc_sta_percent_maxvalue', 1e+038 );
data.SetNumber( 'itc_sta_percent_deprbas_fed', 0 );
data.SetNumber( 'itc_sta_percent_deprbas_sta', 0 );
data.SetArray( 'ptc_fed_amount', [ 0 ] );
data.SetNumber( 'ptc_fed_term', 10 );
data.SetNumber( 'ptc_fed_escal', 2.5 );
data.SetArray( 'ptc_sta_amount', [ 0 ] );
data.SetNumber( 'ptc_sta_term', 10 );
data.SetNumber( 'ptc_sta_escal', 2.5 );
data.SetNumber( 'ibi_fed_amount', 0 );
data.SetNumber( 'ibi_fed_amount_tax_fed', 1 );
data.SetNumber( 'ibi_fed_amount_tax_sta', 1 );
data.SetNumber( 'ibi_fed_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_fed_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_sta_amount', 0 );
data.SetNumber( 'ibi_sta_amount_tax_fed', 1 );
data.SetNumber( 'ibi_sta_amount_tax_sta', 1 );
data.SetNumber( 'ibi_sta_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_sta_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_uti_amount', 0 );
data.SetNumber( 'ibi_uti_amount_tax_fed', 1 );
data.SetNumber( 'ibi_uti_amount_tax_sta', 1 );
data.SetNumber( 'ibi_uti_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_uti_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_oth_amount', 0 );
data.SetNumber( 'ibi_oth_amount_tax_fed', 1 );
data.SetNumber( 'ibi_oth_amount_tax_sta', 1 );
data.SetNumber( 'ibi_oth_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_oth_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_fed_percent', 0 );
data.SetNumber( 'ibi_fed_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_fed_percent_tax_fed', 1 );
data.SetNumber( 'ibi_fed_percent_tax_sta', 1 );
data.SetNumber( 'ibi_fed_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_fed_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_sta_percent', 0 );
data.SetNumber( 'ibi_sta_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_sta_percent_tax_fed', 1 );
data.SetNumber( 'ibi_sta_percent_tax_sta', 1 );
data.SetNumber( 'ibi_sta_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_sta_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_uti_percent', 0 );
data.SetNumber( 'ibi_uti_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_uti_percent_tax_fed', 1 );
data.SetNumber( 'ibi_uti_percent_tax_sta', 1 );
data.SetNumber( 'ibi_uti_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_uti_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_oth_percent', 0 );
data.SetNumber( 'ibi_oth_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_oth_percent_tax_fed', 1 );
data.SetNumber( 'ibi_oth_percent_tax_sta', 1 );
data.SetNumber( 'ibi_oth_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_oth_percent_deprbas_sta', 0 );
data.SetNumber( 'cbi_fed_amount', 0 );
data.SetNumber( 'cbi_fed_maxvalue', 1e+038 );
data.SetNumber( 'cbi_fed_tax_fed', 1 );
data.SetNumber( 'cbi_fed_tax_sta', 1 );
data.SetNumber( 'cbi_fed_deprbas_fed', 0 );
data.SetNumber( 'cbi_fed_deprbas_sta', 0 );
data.SetNumber( 'cbi_sta_amount', 0 );
data.SetNumber( 'cbi_sta_maxvalue', 1e+038 );
data.SetNumber( 'cbi_sta_tax_fed', 1 );
data.SetNumber( 'cbi_sta_tax_sta', 1 );
data.SetNumber( 'cbi_sta_deprbas_fed', 0 );
data.SetNumber( 'cbi_sta_deprbas_sta', 0 );
data.SetNumber( 'cbi_uti_amount', 0 );
data.SetNumber( 'cbi_uti_maxvalue', 1e+038 );
data.SetNumber( 'cbi_uti_tax_fed', 1 );
data.SetNumber( 'cbi_uti_tax_sta', 1 );
data.SetNumber( 'cbi_uti_deprbas_fed', 0 );
data.SetNumber( 'cbi_uti_deprbas_sta', 0 );
data.SetNumber( 'cbi_oth_amount', 0 );
data.SetNumber( 'cbi_oth_maxvalue', 1e+038 );
data.SetNumber( 'cbi_oth_tax_fed', 1 );
data.SetNumber( 'cbi_oth_tax_sta', 1 );
data.SetNumber( 'cbi_oth_deprbas_fed', 0 );
data.SetNumber( 'cbi_oth_deprbas_sta', 0 );
data.SetArray( 'pbi_fed_amount', [ 0 ] );
data.SetNumber( 'pbi_fed_term', 0 );
data.SetNumber( 'pbi_fed_escal', 0 );
data.SetNumber( 'pbi_fed_tax_fed', 1 );
data.SetNumber( 'pbi_fed_tax_sta', 1 );
data.SetArray( 'pbi_sta_amount', [ 0 ] );
data.SetNumber( 'pbi_sta_term', 0 );
data.SetNumber( 'pbi_sta_escal', 0 );
data.SetNumber( 'pbi_sta_tax_fed', 1 );
data.SetNumber( 'pbi_sta_tax_sta', 1 );
data.SetArray( 'pbi_uti_amount', [ 0 ] );
data.SetNumber( 'pbi_uti_term', 0 );
data.SetNumber( 'pbi_uti_escal', 0 );
data.SetNumber( 'pbi_uti_tax_fed', 1 );
data.SetNumber( 'pbi_uti_tax_sta', 1 );
data.SetArray( 'pbi_oth_amount', [ 0 ] );
data.SetNumber( 'pbi_oth_term', 0 );
data.SetNumber( 'pbi_oth_escal', 0 );
data.SetNumber( 'pbi_oth_tax_fed', 1 );
data.SetNumber( 'pbi_oth_tax_sta', 1 );
data.SetNumber( 'market', 0 );
data.SetNumber( 'mortgage', 0 );
data.SetNumber( 'total_installed_cost', 22194.2 );
data.SetNumber( 'salvage_percentage', 0 );

module = SSC.Module('cashloan');
if (module.Exec(data))

    lcoe_real = data.GetNumber('lcoe_real');
    lcoe_nom = data.GetNumber('lcoe_nom');
    npv = data.GetNumber('npv');
    names{end+1} = sprintf('LCOE real (cents/kWh) = %g', lcoe_real);
    names{end+1} = sprintf('LCOE nominal (cents/kWh) = %g', lcoe_nom);
    names{end+1} = sprintf('NPV = %g', npv);
    names{end+1} = 'PV Residential Test OK';
else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'cashloan example failed';
end


set(handles.txtData,'String',names);


% --- Executes on button press in btnPVCom.
function btnPVCom_Callback(hObject, eventdata, handles)
% hObject    handle to btnPVCom (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Flat Plate PV in Commercial market SAM defaults
names={};
data = SSC.Data();

% pvsamv1 input variables
data.SetString( 'weather_file', '../../examples/AZ Phoenix.tm2' );
data.SetNumber( 'use_wf_albedo', 1 );
data.SetNumber( 'albedo', 0.2 );
data.SetNumber( 'irrad_mode', 0 );
data.SetNumber( 'sky_model', 2 );
data.SetNumber( 'ac_derate', 0.99 );
data.SetNumber( 'modules_per_string', 8 );
data.SetNumber( 'strings_in_parallel', 116 );
data.SetNumber( 'inverter_count', 6 );
data.SetNumber( 'enable_mismatch_vmax_calc', 0 );
data.SetNumber( 'subarray1_tilt', 33 );
data.SetNumber( 'subarray1_tilt_eq_lat', 0 );
data.SetNumber( 'subarray1_azimuth', 180 );
data.SetNumber( 'subarray1_track_mode', 0 );
data.SetNumber( 'subarray1_rotlim', 45 );
data.SetNumber( 'subarray1_enable_backtracking', 0 );
data.SetNumber( 'subarray1_btwidth', 2 );
data.SetNumber( 'subarray1_btspacing', 1 );
data.SetArray( 'subarray1_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray1_derate', 0.955598 );
data.SetNumber( 'subarray2_enable', 0 );
data.SetNumber( 'subarray2_nstrings', 0 );
data.SetNumber( 'subarray2_tilt', 20 );
data.SetNumber( 'subarray2_tilt_eq_lat', 0 );
data.SetNumber( 'subarray2_azimuth', 180 );
data.SetNumber( 'subarray2_track_mode', 0 );
data.SetNumber( 'subarray2_rotlim', 45 );
data.SetNumber( 'subarray2_enable_backtracking', 0 );
data.SetNumber( 'subarray2_btwidth', 2 );
data.SetNumber( 'subarray2_btspacing', 1 );
data.SetArray( 'subarray2_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray2_derate', 0.955598 );
data.SetNumber( 'subarray3_enable', 0 );
data.SetNumber( 'subarray3_nstrings', 0 );
data.SetNumber( 'subarray3_tilt', 20 );
data.SetNumber( 'subarray3_tilt_eq_lat', 0 );
data.SetNumber( 'subarray3_azimuth', 180 );
data.SetNumber( 'subarray3_track_mode', 0 );
data.SetNumber( 'subarray3_rotlim', 45 );
data.SetNumber( 'subarray3_enable_backtracking', 0 );
data.SetNumber( 'subarray3_btwidth', 2 );
data.SetNumber( 'subarray3_btspacing', 1 );
data.SetArray( 'subarray3_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray3_derate', 0.955598 );
data.SetNumber( 'subarray4_enable', 0 );
data.SetNumber( 'subarray4_nstrings', 0 );
data.SetNumber( 'subarray4_tilt', 20 );
data.SetNumber( 'subarray4_tilt_eq_lat', 0 );
data.SetNumber( 'subarray4_azimuth', 180 );
data.SetNumber( 'subarray4_track_mode', 0 );
data.SetNumber( 'subarray4_rotlim', 45 );
data.SetNumber( 'subarray4_enable_backtracking', 0 );
data.SetNumber( 'subarray4_btwidth', 2 );
data.SetNumber( 'subarray4_btspacing', 1 );
data.SetArray( 'subarray4_soiling', [ 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95 ] );
data.SetNumber( 'subarray4_derate', 0.955598 );
data.SetNumber( 'module_model', 1 );
data.SetNumber( 'spe_area', 0.74074 );
data.SetNumber( 'spe_rad0', 200 );
data.SetNumber( 'spe_rad1', 400 );
data.SetNumber( 'spe_rad2', 600 );
data.SetNumber( 'spe_rad3', 800 );
data.SetNumber( 'spe_rad4', 1000 );
data.SetNumber( 'spe_eff0', 13.5 );
data.SetNumber( 'spe_eff1', 13.5 );
data.SetNumber( 'spe_eff2', 13.5 );
data.SetNumber( 'spe_eff3', 13.5 );
data.SetNumber( 'spe_eff4', 13.5 );
data.SetNumber( 'spe_reference', 4 );
data.SetNumber( 'spe_module_structure', 0 );
data.SetNumber( 'spe_a', -3.56 );
data.SetNumber( 'spe_b', -0.075 );
data.SetNumber( 'spe_dT', 3 );
data.SetNumber( 'spe_temp_coeff', -0.5 );
data.SetNumber( 'spe_fd', 1 );
data.SetNumber( 'cec_area', 1.244 );
data.SetNumber( 'cec_a_ref', 1.9816 );
data.SetNumber( 'cec_adjust', 20.8 );
data.SetNumber( 'cec_alpha_sc', 0.002651 );
data.SetNumber( 'cec_beta_oc', -0.14234 );
data.SetNumber( 'cec_gamma_r', -0.407 );
data.SetNumber( 'cec_i_l_ref', 5.754 );
data.SetNumber( 'cec_i_mp_ref', 5.25 );
data.SetNumber( 'cec_i_o_ref', 1.919e-010 );
data.SetNumber( 'cec_i_sc_ref', 5.75 );
data.SetNumber( 'cec_n_s', 72 );
data.SetNumber( 'cec_r_s', 0.105 );
data.SetNumber( 'cec_r_sh_ref', 160.48 );
data.SetNumber( 'cec_t_noct', 49.2 );
data.SetNumber( 'cec_v_mp_ref', 41 );
data.SetNumber( 'cec_v_oc_ref', 47.7 );
data.SetNumber( 'cec_temp_corr_mode', 0 );
data.SetNumber( 'cec_standoff', 6 );
data.SetNumber( 'cec_height', 0 );
data.SetNumber( 'cec_mounting_config', 0 );
data.SetNumber( 'cec_heat_transfer', 0 );
data.SetNumber( 'cec_mounting_orientation', 0 );
data.SetNumber( 'cec_gap_spacing', 0.05 );
data.SetNumber( 'cec_module_width', 1 );
data.SetNumber( 'cec_module_length', 1.244 );
data.SetNumber( 'cec_array_rows', 1 );
data.SetNumber( 'cec_array_cols', 10 );
data.SetNumber( 'cec_backside_temp', 20 );
data.SetNumber( '6par_celltech', 1 );
data.SetNumber( '6par_vmp', 30 );
data.SetNumber( '6par_imp', 6 );
data.SetNumber( '6par_voc', 37 );
data.SetNumber( '6par_isc', 7 );
data.SetNumber( '6par_bvoc', -0.11 );
data.SetNumber( '6par_aisc', 0.004 );
data.SetNumber( '6par_gpmp', -0.41 );
data.SetNumber( '6par_nser', 60 );
data.SetNumber( '6par_area', 1.3 );
data.SetNumber( '6par_tnoct', 46 );
data.SetNumber( '6par_standoff', 6 );
data.SetNumber( '6par_mounting', 0 );
data.SetNumber( 'snl_module_structure', 0 );
data.SetNumber( 'snl_a', -3.62 );
data.SetNumber( 'snl_b', -0.075 );
data.SetNumber( 'snl_dtc', 3 );
data.SetNumber( 'snl_ref_a', -3.62 );
data.SetNumber( 'snl_ref_b', -0.075 );
data.SetNumber( 'snl_ref_dT', 3 );
data.SetNumber( 'snl_fd', 1 );
data.SetNumber( 'snl_a0', 0.94045 );
data.SetNumber( 'snl_a1', 0.052641 );
data.SetNumber( 'snl_a2', -0.0093897 );
data.SetNumber( 'snl_a3', 0.00072623 );
data.SetNumber( 'snl_a4', -1.9938e-005 );
data.SetNumber( 'snl_aimp', -0.00038 );
data.SetNumber( 'snl_aisc', 0.00061 );
data.SetNumber( 'snl_area', 1.244 );
data.SetNumber( 'snl_b0', 1 );
data.SetNumber( 'snl_b1', -0.002438 );
data.SetNumber( 'snl_b2', 0.0003103 );
data.SetNumber( 'snl_b3', -1.246e-005 );
data.SetNumber( 'snl_b4', 2.112e-007 );
data.SetNumber( 'snl_b5', -1.359e-009 );
data.SetNumber( 'snl_bvmpo', -0.139 );
data.SetNumber( 'snl_bvoco', -0.136 );
data.SetNumber( 'snl_c0', 1.0039 );
data.SetNumber( 'snl_c1', -0.0039 );
data.SetNumber( 'snl_c2', 0.291066 );
data.SetNumber( 'snl_c3', -4.73546 );
data.SetNumber( 'snl_c4', 0.9942 );
data.SetNumber( 'snl_c5', 0.0058 );
data.SetNumber( 'snl_c6', 1.0723 );
data.SetNumber( 'snl_c7', -0.0723 );
data.SetNumber( 'snl_impo', 5.25 );
data.SetNumber( 'snl_isco', 5.75 );
data.SetNumber( 'snl_ixo', 5.65 );
data.SetNumber( 'snl_ixxo', 3.85 );
data.SetNumber( 'snl_mbvmp', 0 );
data.SetNumber( 'snl_mbvoc', 0 );
data.SetNumber( 'snl_n', 1.221 );
data.SetNumber( 'snl_series_cells', 72 );
data.SetNumber( 'snl_vmpo', 40 );
data.SetNumber( 'snl_voco', 47.7 );
data.SetNumber( 'inverter_model', 1 );
data.SetNumber( 'inv_spe_efficiency', 95 );
data.SetNumber( 'inv_spe_power_ac', 4000 );
data.SetNumber( 'inv_snl_c0', -3.55551e-008 );
data.SetNumber( 'inv_snl_c1', 8.6927e-005 );
data.SetNumber( 'inv_snl_c2', 0.000564748 );
data.SetNumber( 'inv_snl_c3', -0.00369635 );
data.SetNumber( 'inv_snl_paco', 36000 );
data.SetNumber( 'inv_snl_pdco', 37453.9 );
data.SetNumber( 'inv_snl_pnt', 0.6 );
data.SetNumber( 'inv_snl_pso', 194.963 );
data.SetNumber( 'inv_snl_vdco', 309.917 );
data.SetNumber( 'inv_snl_vdcmax', 600 );
data.SetNumber( 'self_shading_enabled', 0 );
data.SetNumber( 'self_shading_length', 1.84844 );
data.SetNumber( 'self_shading_width', 0.673 );
data.SetNumber( 'self_shading_mod_orient', 1 );
data.SetNumber( 'self_shading_str_orient', 0 );
data.SetNumber( 'self_shading_ncellx', 6 );
data.SetNumber( 'self_shading_ncelly', 12 );
data.SetNumber( 'self_shading_ndiode', 3 );
data.SetNumber( 'self_shading_nmodx', 116 );
data.SetNumber( 'self_shading_nstrx', 1 );
data.SetNumber( 'self_shading_nmody', 3 );
data.SetNumber( 'self_shading_nrows', 2 );
data.SetNumber( 'self_shading_rowspace', 5 );

module = SSC.Module('pvsamv1');
if (module.Exec(data))
    ac_hourly = data.GetArray('hourly_ac_net');
    ac_monthly = data.GetArray('monthly_ac_net');
    ac_annual = data.GetNumber('annual_ac_net');
    for i = 1:size(ac_monthly)
        names{end+1} = sprintf('[%d]: %g kWh', i,ac_monthly(i));
    end
    names{end+1} = sprintf('AC total: %g kWh', ac_annual);
    names{end+1} = 'PVSamV1 test OK';
else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'pvsamv1 example failed';
end

% annualoutput input variables
data.SetNumber( 'analysis_years', 25 );
data.SetArray( 'energy_availability', [ 100 ] );
data.SetArray( 'energy_degradation', [ 0.5 ] );
data.SetMatrix( 'energy_curtailment', [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ; 
 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ] );
data.SetNumber( 'system_use_lifetime_output', 0 );
data.SetArray( 'energy_net_hourly', ac_hourly);

module = SSC.Module('annualoutput');
if (module.Exec(data))

 	net_hourly = data.GetArray('hourly_e_net_delivered');

else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'annualoutput example failed';
end

% utilityrate input variables
data.SetNumber( 'analysis_years', 25 );
data.SetArray( 'e_with_system', net_hourly);
data.SetArray( 'system_availability', [ 100 ] );
data.SetArray( 'system_degradation', [ 0.5 ] );
data.SetArray( 'load_escalation', [ 2.5 ] );
data.SetArray( 'rate_escalation', [ 2.5 ] );
data.SetNumber( 'ur_sell_eq_buy', 1 );
data.SetNumber( 'ur_monthly_fixed_charge', 0 );
data.SetNumber( 'ur_flat_buy_rate', 0.12 );
data.SetNumber( 'ur_flat_sell_rate', 0 );
data.SetNumber( 'ur_tou_enable', 0 );
data.SetNumber( 'ur_tou_p1_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p1_sell_rate', 0 );
data.SetNumber( 'ur_tou_p2_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p2_sell_rate', 0 );
data.SetNumber( 'ur_tou_p3_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p3_sell_rate', 0 );
data.SetNumber( 'ur_tou_p4_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p4_sell_rate', 0 );
data.SetNumber( 'ur_tou_p5_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p5_sell_rate', 0 );
data.SetNumber( 'ur_tou_p6_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p6_sell_rate', 0 );
data.SetNumber( 'ur_tou_p7_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p7_sell_rate', 0 );
data.SetNumber( 'ur_tou_p8_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p8_sell_rate', 0 );
data.SetNumber( 'ur_tou_p9_buy_rate', 0.12 );
data.SetNumber( 'ur_tou_p9_sell_rate', 0 );
data.SetString( 'ur_tou_sched_weekday', '111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111' );
data.SetString( 'ur_tou_sched_weekend', '111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111' );
data.SetNumber( 'ur_dc_enable', 0 );
data.SetNumber( 'ur_dc_fixed_m1', 0 );
data.SetNumber( 'ur_dc_fixed_m2', 0 );
data.SetNumber( 'ur_dc_fixed_m3', 0 );
data.SetNumber( 'ur_dc_fixed_m4', 0 );
data.SetNumber( 'ur_dc_fixed_m5', 0 );
data.SetNumber( 'ur_dc_fixed_m6', 0 );
data.SetNumber( 'ur_dc_fixed_m7', 0 );
data.SetNumber( 'ur_dc_fixed_m8', 0 );
data.SetNumber( 'ur_dc_fixed_m9', 0 );
data.SetNumber( 'ur_dc_fixed_m10', 0 );
data.SetNumber( 'ur_dc_fixed_m11', 0 );
data.SetNumber( 'ur_dc_fixed_m12', 0 );
data.SetNumber( 'ur_dc_p1', 0 );
data.SetNumber( 'ur_dc_p2', 0 );
data.SetNumber( 'ur_dc_p3', 0 );
data.SetNumber( 'ur_dc_p4', 0 );
data.SetNumber( 'ur_dc_p5', 0 );
data.SetNumber( 'ur_dc_p6', 0 );
data.SetNumber( 'ur_dc_p7', 0 );
data.SetNumber( 'ur_dc_p8', 0 );
data.SetNumber( 'ur_dc_p9', 0 );
data.SetString( 'ur_dc_sched_weekday', '444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444' );
data.SetString( 'ur_dc_sched_weekend', '444444443333333333334444444444443333333333334444444444443333333333334444444444443333333333334444222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222222222221111111111112222444444443333333333334444444444443333333333334444' );
data.SetNumber( 'ur_tr_enable', 0 );
data.SetNumber( 'ur_tr_sell_mode', 1 );
data.SetNumber( 'ur_tr_sell_rate', 0 );
data.SetNumber( 'ur_tr_s1_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s1_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s1_rate1', 0 );
data.SetNumber( 'ur_tr_s1_rate2', 0 );
data.SetNumber( 'ur_tr_s1_rate3', 0 );
data.SetNumber( 'ur_tr_s1_rate4', 0 );
data.SetNumber( 'ur_tr_s1_rate5', 0 );
data.SetNumber( 'ur_tr_s1_rate6', 0 );
data.SetNumber( 'ur_tr_s2_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s2_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s2_rate1', 0 );
data.SetNumber( 'ur_tr_s2_rate2', 0 );
data.SetNumber( 'ur_tr_s2_rate3', 0 );
data.SetNumber( 'ur_tr_s2_rate4', 0 );
data.SetNumber( 'ur_tr_s2_rate5', 0 );
data.SetNumber( 'ur_tr_s2_rate6', 0 );
data.SetNumber( 'ur_tr_s3_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s3_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s3_rate1', 0 );
data.SetNumber( 'ur_tr_s3_rate2', 0 );
data.SetNumber( 'ur_tr_s3_rate3', 0 );
data.SetNumber( 'ur_tr_s3_rate4', 0 );
data.SetNumber( 'ur_tr_s3_rate5', 0 );
data.SetNumber( 'ur_tr_s3_rate6', 0 );
data.SetNumber( 'ur_tr_s4_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s4_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s4_rate1', 0 );
data.SetNumber( 'ur_tr_s4_rate2', 0 );
data.SetNumber( 'ur_tr_s4_rate3', 0 );
data.SetNumber( 'ur_tr_s4_rate4', 0 );
data.SetNumber( 'ur_tr_s4_rate5', 0 );
data.SetNumber( 'ur_tr_s4_rate6', 0 );
data.SetNumber( 'ur_tr_s5_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s5_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s5_rate1', 0 );
data.SetNumber( 'ur_tr_s5_rate2', 0 );
data.SetNumber( 'ur_tr_s5_rate3', 0 );
data.SetNumber( 'ur_tr_s5_rate4', 0 );
data.SetNumber( 'ur_tr_s5_rate5', 0 );
data.SetNumber( 'ur_tr_s5_rate6', 0 );
data.SetNumber( 'ur_tr_s6_energy_ub1', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub2', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub3', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub4', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub5', 1e+038 );
data.SetNumber( 'ur_tr_s6_energy_ub6', 1e+038 );
data.SetNumber( 'ur_tr_s6_rate1', 0 );
data.SetNumber( 'ur_tr_s6_rate2', 0 );
data.SetNumber( 'ur_tr_s6_rate3', 0 );
data.SetNumber( 'ur_tr_s6_rate4', 0 );
data.SetNumber( 'ur_tr_s6_rate5', 0 );
data.SetNumber( 'ur_tr_s6_rate6', 0 );
data.SetNumber( 'ur_tr_sched_m1', 0 );
data.SetNumber( 'ur_tr_sched_m2', 0 );
data.SetNumber( 'ur_tr_sched_m3', 0 );
data.SetNumber( 'ur_tr_sched_m4', 0 );
data.SetNumber( 'ur_tr_sched_m5', 0 );
data.SetNumber( 'ur_tr_sched_m6', 0 );
data.SetNumber( 'ur_tr_sched_m7', 0 );
data.SetNumber( 'ur_tr_sched_m8', 0 );
data.SetNumber( 'ur_tr_sched_m9', 0 );
data.SetNumber( 'ur_tr_sched_m10', 0 );
data.SetNumber( 'ur_tr_sched_m11', 0 );
data.SetNumber( 'ur_tr_sched_m12', 0 );

module = SSC.Module('utilityrate');
if (module.Exec(data))

 % TODO: place output variables here

else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'utilityrate example failed';
end


% cashloan input variables
data.SetNumber( 'analysis_years', 25 );
data.SetNumber( 'federal_tax_rate', 28 );
data.SetNumber( 'state_tax_rate', 7 );
data.SetNumber( 'property_tax_rate', 2 );
data.SetNumber( 'prop_tax_cost_assessed_percent', 100 );
data.SetNumber( 'prop_tax_assessed_decline', 0 );
data.SetNumber( 'sales_tax_rate', 5 );
data.SetNumber( 'real_discount_rate', 5.2 );
data.SetNumber( 'inflation_rate', 2.5 );
data.SetNumber( 'insurance_rate', 0.5 );
data.SetNumber( 'system_capacity', 199.752 );
data.SetNumber( 'system_heat_rate', 0 );
data.SetNumber( 'loan_term', 25 );
data.SetNumber( 'loan_rate', 7.5 );
data.SetNumber( 'loan_debt', 100 );
data.SetArray( 'om_fixed', [ 0 ] );
data.SetNumber( 'om_fixed_escal', 0 );
data.SetArray( 'om_production', [ 0 ] );
data.SetNumber( 'om_production_escal', 0 );
data.SetArray( 'om_capacity', [ 20 ] );
data.SetNumber( 'om_capacity_escal', 0 );
data.SetArray( 'om_fuel_cost', [ 0 ] );
data.SetNumber( 'om_fuel_cost_escal', 0 );
data.SetNumber( 'annual_fuel_usage', 0 );
data.SetNumber( 'depr_fed_type', 1 );
data.SetNumber( 'depr_fed_sl_years', 7 );
data.SetArray( 'depr_fed_custom', [ 9.91839e-039 ] );
data.SetNumber( 'depr_sta_type', 1 );
data.SetNumber( 'depr_sta_sl_years', 7 );
data.SetArray( 'depr_sta_custom', [ 7.84727e-044 ] );
data.SetNumber( 'itc_fed_amount', 0 );
data.SetNumber( 'itc_fed_amount_deprbas_fed', 1 );
data.SetNumber( 'itc_fed_amount_deprbas_sta', 1 );
data.SetNumber( 'itc_sta_amount', 0 );
data.SetNumber( 'itc_sta_amount_deprbas_fed', 0 );
data.SetNumber( 'itc_sta_amount_deprbas_sta', 0 );
data.SetNumber( 'itc_fed_percent', 30 );
data.SetNumber( 'itc_fed_percent_maxvalue', 1e+038 );
data.SetNumber( 'itc_fed_percent_deprbas_fed', 1 );
data.SetNumber( 'itc_fed_percent_deprbas_sta', 1 );
data.SetNumber( 'itc_sta_percent', 0 );
data.SetNumber( 'itc_sta_percent_maxvalue', 1e+038 );
data.SetNumber( 'itc_sta_percent_deprbas_fed', 0 );
data.SetNumber( 'itc_sta_percent_deprbas_sta', 0 );
data.SetArray( 'ptc_fed_amount', [ 0 ] );
data.SetNumber( 'ptc_fed_term', 10 );
data.SetNumber( 'ptc_fed_escal', 2.5 );
data.SetArray( 'ptc_sta_amount', [ 0 ] );
data.SetNumber( 'ptc_sta_term', 10 );
data.SetNumber( 'ptc_sta_escal', 2.5 );
data.SetNumber( 'ibi_fed_amount', 0 );
data.SetNumber( 'ibi_fed_amount_tax_fed', 1 );
data.SetNumber( 'ibi_fed_amount_tax_sta', 1 );
data.SetNumber( 'ibi_fed_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_fed_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_sta_amount', 0 );
data.SetNumber( 'ibi_sta_amount_tax_fed', 1 );
data.SetNumber( 'ibi_sta_amount_tax_sta', 1 );
data.SetNumber( 'ibi_sta_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_sta_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_uti_amount', 0 );
data.SetNumber( 'ibi_uti_amount_tax_fed', 1 );
data.SetNumber( 'ibi_uti_amount_tax_sta', 1 );
data.SetNumber( 'ibi_uti_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_uti_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_oth_amount', 0 );
data.SetNumber( 'ibi_oth_amount_tax_fed', 1 );
data.SetNumber( 'ibi_oth_amount_tax_sta', 1 );
data.SetNumber( 'ibi_oth_amount_deprbas_fed', 0 );
data.SetNumber( 'ibi_oth_amount_deprbas_sta', 0 );
data.SetNumber( 'ibi_fed_percent', 0 );
data.SetNumber( 'ibi_fed_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_fed_percent_tax_fed', 1 );
data.SetNumber( 'ibi_fed_percent_tax_sta', 1 );
data.SetNumber( 'ibi_fed_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_fed_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_sta_percent', 0 );
data.SetNumber( 'ibi_sta_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_sta_percent_tax_fed', 1 );
data.SetNumber( 'ibi_sta_percent_tax_sta', 1 );
data.SetNumber( 'ibi_sta_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_sta_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_uti_percent', 0 );
data.SetNumber( 'ibi_uti_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_uti_percent_tax_fed', 1 );
data.SetNumber( 'ibi_uti_percent_tax_sta', 1 );
data.SetNumber( 'ibi_uti_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_uti_percent_deprbas_sta', 0 );
data.SetNumber( 'ibi_oth_percent', 0 );
data.SetNumber( 'ibi_oth_percent_maxvalue', 1e+038 );
data.SetNumber( 'ibi_oth_percent_tax_fed', 1 );
data.SetNumber( 'ibi_oth_percent_tax_sta', 1 );
data.SetNumber( 'ibi_oth_percent_deprbas_fed', 0 );
data.SetNumber( 'ibi_oth_percent_deprbas_sta', 0 );
data.SetNumber( 'cbi_fed_amount', 0 );
data.SetNumber( 'cbi_fed_maxvalue', 1e+038 );
data.SetNumber( 'cbi_fed_tax_fed', 1 );
data.SetNumber( 'cbi_fed_tax_sta', 1 );
data.SetNumber( 'cbi_fed_deprbas_fed', 0 );
data.SetNumber( 'cbi_fed_deprbas_sta', 0 );
data.SetNumber( 'cbi_sta_amount', 0 );
data.SetNumber( 'cbi_sta_maxvalue', 1e+038 );
data.SetNumber( 'cbi_sta_tax_fed', 1 );
data.SetNumber( 'cbi_sta_tax_sta', 1 );
data.SetNumber( 'cbi_sta_deprbas_fed', 0 );
data.SetNumber( 'cbi_sta_deprbas_sta', 0 );
data.SetNumber( 'cbi_uti_amount', 0 );
data.SetNumber( 'cbi_uti_maxvalue', 1e+038 );
data.SetNumber( 'cbi_uti_tax_fed', 1 );
data.SetNumber( 'cbi_uti_tax_sta', 1 );
data.SetNumber( 'cbi_uti_deprbas_fed', 0 );
data.SetNumber( 'cbi_uti_deprbas_sta', 0 );
data.SetNumber( 'cbi_oth_amount', 0 );
data.SetNumber( 'cbi_oth_maxvalue', 1e+038 );
data.SetNumber( 'cbi_oth_tax_fed', 1 );
data.SetNumber( 'cbi_oth_tax_sta', 1 );
data.SetNumber( 'cbi_oth_deprbas_fed', 0 );
data.SetNumber( 'cbi_oth_deprbas_sta', 0 );
data.SetArray( 'pbi_fed_amount', [ 0 ] );
data.SetNumber( 'pbi_fed_term', 10 );
data.SetNumber( 'pbi_fed_escal', 0 );
data.SetNumber( 'pbi_fed_tax_fed', 1 );
data.SetNumber( 'pbi_fed_tax_sta', 1 );
data.SetArray( 'pbi_sta_amount', [ 0 ] );
data.SetNumber( 'pbi_sta_term', 10 );
data.SetNumber( 'pbi_sta_escal', 0 );
data.SetNumber( 'pbi_sta_tax_fed', 1 );
data.SetNumber( 'pbi_sta_tax_sta', 1 );
data.SetArray( 'pbi_uti_amount', [ 0 ] );
data.SetNumber( 'pbi_uti_term', 10 );
data.SetNumber( 'pbi_uti_escal', 0 );
data.SetNumber( 'pbi_uti_tax_fed', 1 );
data.SetNumber( 'pbi_uti_tax_sta', 1 );
data.SetArray( 'pbi_oth_amount', [ 0 ] );
data.SetNumber( 'pbi_oth_term', 10 );
data.SetNumber( 'pbi_oth_escal', 0 );
data.SetNumber( 'pbi_oth_tax_fed', 1 );
data.SetNumber( 'pbi_oth_tax_sta', 1 );
data.SetNumber( 'market', 1 );
data.SetNumber( 'total_installed_cost', 922575 );
data.SetNumber( 'salvage_percentage', 0 );

module = SSC.Module('cashloan');
if (module.Exec(data))

    lcoe_real = data.GetNumber('lcoe_real');
    lcoe_nom = data.GetNumber('lcoe_nom');
    npv = data.GetNumber('npv');
    names{end+1} = sprintf('LCOE real (cents/kWh) = %g', lcoe_real);
    names{end+1} = sprintf('LCOE nominal (cents/kWh) = %g', lcoe_nom);
    names{end+1} = sprintf('NPV = %g', npv);
    names{end+1} = 'PV Commercial Test OK';
else

	idx = 0;
	[result, msg, type, time] = module.Log(idx);
	while (result)
		names{end+1} = sprintf('[%s at time:%g ]: %s', type, time, msg);
		idx = idx + 1;
		[result, msg, type, time] = module.Log(idx);
	end
	names{end+1} = 'cashloan example failed';
end

set(handles.txtData,'String',names);
