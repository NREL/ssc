classdef API < handle
    %API Meta data from SSC
    %   Version information and Build information from ssc dll
    
    properties 
       Version
       BuildInfo
    end
    
    methods
        function obj = API()
            ssccall('load');
            obj.Version =  ssccall('version');
            obj.BuildInfo = ssccall('build_info');
        end
        function delete(obj)
            ssccall('unload');
        end
    end
    
end

