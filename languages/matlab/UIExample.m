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

% Last Modified by GUIDE v2.5 11-Mar-2013 04:41:10

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
        names{end+1} = sprintf('\t%s: "%s" ["%s"] %s (%s)\n',sscInfo.VarType(), sscInfo.Name(), sscInfo.DataType(), sscInfo.Label(), sscInfo.Units());
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
    names{end+1} = sprintf('\treturned array element: %d = %g\n',i, retArray(i));
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
        names{end+1} = sprintf('\treturned matrix element: (%d,%d) = %g\n', i,j, retMatrix(i,j));
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
sscData.SetString('file_name', '../../../abilene.tm2');
sscData.SetNumber('system_size', 4.0);
sscData.SetNumber('derate', 0.77);
sscData.SetNumber('track_mode', 0);
sscData.SetNumber('tilt', 20);
sscData.SetNumber('azimuth', 180);

mod = SSC.Module('pvwattsv1');
if (mod.Exec(sscData)),
    tot = data.GetNumber('ac_annual');
    ac = data.GetArray('ac_monthly');
    for i = 1:size(ac)
        names{end+1} = sprintf('[%d]: %g kWh\n', i,ac(i));
    end
    names{end+1} = sprintf('AC total: %g\n', tot);
    names{end+1} = 'PVWatts test OK\n';
%else
%     int idx = 0;
%     String msg;
%     int type;
%     float time;
%     while (mod.Log(idx, msg, type, time))
%        String stype = 'NOTICE';
%         if (type == SSC.API.WARNING),
%             stype = 'WARNING';
%         else
%             if (type == SSC.API.ERROR) stype = 'ERROR';
%         txtData.AppendText('[ ' + stype + ' at time:' + time + ' ]: ' + msg + '\n');
%         idx++;
%     txtData.AppendText('PVWatts example failed\n');
%             end 
%         end
%     end
end
set(handles.txtData,'String',names);

% --- Executes on button press in bntPVWattsFunc.
function bntPVWattsFunc_Callback(hObject, eventdata, handles)
% hObject    handle to bntPVWattsFunc (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
