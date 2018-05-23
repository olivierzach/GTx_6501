function y = ex_function(theta,a,b)
y = (1./(1+theta.^2)).*((a+(b-1).*theta).^2+b^2);
%notice that only theta is a vector, a and b are constants
end

