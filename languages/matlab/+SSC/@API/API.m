classdef API < handle
    %API Meta data from SSC
    %   Version information and Build information from ssc dll
    
    properties 
       Version
       BuildInfo
    end
    
    methods
        function obj = API()
            SSC.ssccall('load');
            obj.Version =  SSC.ssccall('version');
            obj.BuildInfo = SSC.ssccall('build_info');
        end
        function delete(obj)
            SSC.ssccall('unload');
        end
    end
    
end

