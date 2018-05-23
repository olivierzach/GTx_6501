classdef ex_class
    %EX_CLASS Summary of this class goes here
    %   Detailed explanation goes here
    
    properties
    end
    
    methods (Static) %static means that the file can be accessed from outside
        function y = ex_function(theta,a,b)
            y = (1./(1+theta.^2)).*((a+(b-1).*theta).^2+b^2);
            %notice that only theta is a vector, a and b are constants
        end
        
        function y = square_function(x)
            y = x.^2;
        end
    end
    
end

