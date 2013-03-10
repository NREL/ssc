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

% Last Modified by GUIDE v2.5 10-Mar-2013 04:53:39

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
sscAPI = API();
set(handles.txtData,'String',{sprintf('Version = %d',sscAPI.Version);sscAPI.BuildInfo});


% --- Executes on button press in btnModuleList.
function btnModuleList_Callback(hObject, eventdata, handles)
% hObject    handle to btnModuleList (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
sscEntry = Entry();
names = {};
while (sscEntry.Get())
    module_name = sscEntry.Name();
    description = sscEntry.Description();
    version = sscEntry.Version();
    names{end+1} = sprintf('Module: %s, version: %d', module_name, version );
    names{end+1} = description ;
end        
set(handles.txtData,'String',names);
