
% MSA Tutoring Sessions - Week 02 - Matlab
clear all; close all; clc;
% help function
% to terminate (stop execution) press Command + . (iOS) or Ctrl + C (Windows).
% Command (or Ctrl) + i (to organize the script)
% Shift + Enter = Rename shortcut
%% 
prompt = 'Do you want to learn a lot today? Answer = Y/N: ';
answer = input(prompt, 's');
% If the input is empty, this code assigns a default value, 'N', to str.
answer = input(prompt,'s');
if isempty(answer)
    answer = 'N';
end
while answer == 'N'  % a better option would be to use while answer ~= 'Y'... why?
    msgbox('Wrong answer, but let''s try again...','Warning');
    prompt = ' Do you want to learn a lot today. Answer = Y/N ';
    answer = input(prompt,'s');
end
msgbox('Well Done! Let''s get started','Success');
disp('You can also use "disp" to display messages to the user!!!')
clear answer % to clear the variable


%% The Multiple Linear Regression Example from last class
disp ('A student was asked to find the weights of 2 balls A and B using a scale with random measurement errors. He found the following weights')
disp ('Weight of Ball A (measured alone) = 2lbs')
disp ('Weight of Ball B (measured alone) = 3lbs')
disp ('Weight of Balls A + B (measured together) = 4lbs')   

% 2 ways: manually (algebra) and regression function
%% 
clear all;
disp ('First Method: by Algebra')
Y = [2 3 4]';  %or equivalently Y = [2;3;4] or Y = transpose([2 3 4])
X = [ 1 0 ; 0 1 ; 1 1];
inv_matrix = inv(X'*X);
beta_lse = inv_matrix * X' * Y
% notice that the symbol * does the matrix multiplication.
% for element-by-element multiplication, you have to use .*  

% Now calculate the MSE (error variance estimate) and Standard Deviation
mse_est = ((Y(1)-beta_lse(1))^2 + (Y(2)-beta_lse(2))^2 + (Y(3)-beta_lse(1)-beta_lse(2))^2)/(3-2)
std_dev = sqrt(mse_est)

% Compute a 90% Confidence Interval for the weights of Ball A 
help tstat
t_stat = tinv(0.95, 1)
beta_a_upper = beta_lse(1) + t_stat * std_dev * sqrt(inv_matrix(1,1))
beta_a_lower = beta_lse(1) - t_stat * std_dev * sqrt(inv_matrix(1,1))

% .... Alternatively, you can find CI's for both Balls (A and B) using matrix
% calculus directly
upper_ci = beta_lse + t_stat * std_dev * sqrt(diag(inv_matrix))
lower_ci = beta_lse - t_stat * std_dev * sqrt(diag(inv_matrix))
%% 
clear all;
disp ('Second Method: by Stats Function')
Y = [2 3 4]';   %or equivalently Y = [2;3;4] or Y = transpose([2 3 4])
X = [ 1 0 ; 0 1 ; 1 1];
[b, bint, r, rint, stats] = regress(Y, X, 0.1) 
help regress
help rcoplot
rcoplot(r, rint)
%% 
% Let's now check a Famous numeric Method called Bisection
% Implement bisection method to find the t corresponding to 0.95 quantile 
% of a t dist with n=5 df's. Start with the interval [1.291,2.582] and stop when the
% length of the interval is less than 10^(-4).
clear all;
close all;
clc;
% Step 1 - ask user to initialize problem
prompt = 'Please type initial interval [a,b]. a =  ';
a = input(prompt);
prompt = 'b =  ';
b = input(prompt);
stop = 10e-4; % criteria to stop the algorithm
a_quantile= tcdf(a,5, 'upper'); % tcdf returns upper quantile for t-dist with 5 df's
b_quantile= tcdf(b,5, 'upper');
while (b-a)>stop
    c=(a+b)/2;
    c_quantile = tcdf(c,5, 'upper');
    if c_quantile>0.05
        a=c;
        a_quantile=c_quantile;
    elseif c_quantile<0.05
        b=c;
        b_quantile=c_quantile;
    else
        break
    end
end
true_value = tinv(0.95,5);
fprintf('The approx. value is %d and the true value is %d .\n',c,true_value)
%% 
% Random Number Generation and Sorting Algorithm
% you could simply use the function sort(), but let's see an alternative
% way
clear all;
close all;
clc;
vector_ordered = zeros (5000,1);
vector_u = zeros(5000,1);
for ii = 1:5000
     vector_u (ii) = random ('uniform',0,1,1,1);
end
%algorithm for sorting
vector_ordered(1)=min(vector_u);
for jj = 2:5000
    aux = inf;
    for ii = 1:5000
         if vector_u(ii) < aux & vector_u(ii) > vector_ordered(jj-1) 
             aux = vector_u(ii);
         end
         vector_ordered(jj)=aux;
    end
end
histogram (vector_ordered)
%% Linear Optimization with linprog example
clear all;
close all;
clc;
%Draw the feasible set of the LO program, find the optimal solution and
%optimal value to the problem.
%max x2
%s.t.
%   x1 - 2*x2 <= 0
% 2*x1 - 3*x2 <= 2
%   x1 -   x2 <= 3
%  -x1 + 2*x2 <= 2
%-2*x1 +   x2 <= 0

%help linprog

% First: convert to a minimization problem
% x = linprog(f,A,b,Aeq,beq,lb,ub)
% f is the vector with coefficients of the objetive function 
% A is the matrix of coeff. of constraints (LHS) which are <=
% b is the matrix of the values on the RHS for the A constraints
% Aeq is the matrix of coeff. of contrainsts (LHS) which are =
% beq is the matrix of the values on the RHS for the Aeq constraints
% default domain for any variable in Matlab = [-inf, +inf]
% lb = lower bound for the domain of variables
% ub = upper bound for the domain of variables
% [X, Z] = output for coeff. is X and for optimal sol. is Z
f = [0 -1];
A = [1 -2; 2 -3; 1 -1; -1 2; -2 1];
b = [0 2 3 2 0];
Aeq = []; % no equality constraints, empty matrix
beq = [];
lb = [0 0];
ub = [];
[X, Z] = linprog(f,A,b,Aeq,beq,lb,ub)
% we converted from max problem to min problem, so we multiply Z by (-1)
fprintf('The optimal solution is x1 = %.0f  and x2 = %.0f and the opt value is %.0f .\n',X(1), X(2), -Z)


% Now let's plot the feasible region
% However, we're gonna learn how plotting works in Matlab first!!!! 
% For example, if I have y = x^2, I cannot plot like this... why?
% x is not a symbolic variable, it's a vector!!! So when I square I square
% each element in the vector... what I need to do is: y = x.^2 (element-wise)
clc; clear all; close all;
disp('Remember: Matlab just plot data, NOT symbolic elements (use . before operations)')
x= linspace(-2,2); %creates a vector with 100 points between -2 and 2
y = x.^2
plot(x,y)
hold on
z = sin(x)
plot(x,z)
hold off
figure
y = x.^3
plot(x,y)

disp('Let''s continue our LO example and plot the inequality constraints contours')
clc; clear all; close all;
x1=-10:1:10;
x2=-10:1:10;
% x1 and x2 are vectors filled with numbers starting at -10 and ending at 10
% with values at intervals of 0.1
[X1 X2] = meshgrid(x1,x2); % generates matrices X1 and X2 corresponding vectors x1 and x2
%or simply [X1 X2]=meshgrid(-10:1:10,-10:1:10); 
f1 = X2; % the objecive function is evaluated over the entire mesh
ineq1 = X1 - 2*X2; % the inequality g1 is evaluated over the mesh
ineq2 = 2*X1 - 3*X2;
ineq3 = X1 - X2;
ineq4 = -X1 + 2*X2;
ineq5 = -2*X1 + X2;
[C1,h1] = contour(x1,x2,ineq1,[0,0],'r-','LineWidth',2);
clabel(C1,h1,'FontSize',15,'Color','red');
hold on
[C2,h2] = contour(x1,x2,ineq2,[2,2],'m-','LineWidth',2);
clabel(C2,h2,'FontSize',15,'Color','red');
% NOTE: You may define ineq2 as:
%          ineq2 = 2*X1 - 3*X2 - 2
%       and plot the zero-valued contour as:
%          [C2,h2] = contour(x1,x2,ineq2,[0,0],'m-');
%          clabel(C2,h2);
[C3,h3] = contour(x1,x2,ineq3,[3,3],'b-','LineWidth',2);
clabel(C3,h3,'FontSize',15,'Color','red');
[C4,h4] = contour(x1,x2,ineq4,[2,2],'k-','LineWidth',2);
clabel(C4,h4,'FontSize',15,'Color','red');
[C5,h5] = contour(x1,x2,ineq5,[0,0],'y-','LineWidth',2);
clabel(C5,h5,'FontSize',15,'Color','red');
[C,h] = contour(x1,x2,f1,'g-','LineWidth',2);
clabel(C,h,'FontSize',15,'Color','red'); % obj fct contours are plotted GREEN
xlabel(' x1 values','FontName','times','FontSize',12,'FontWeight','bold');
% label for x-axes
ylabel(' x2 values','FontName','times','FontSize',12,'FontWeight','bold');
% label for y-axes
grid
hold off
%ineq1 = x1 - 2*x2 <= 0; % Get True where condition aplies, false where not.
% Get boundaries of the condition
%bound1 = 2*x2(1,:);

%% Function Example

%a very simple anonymous (inline) function example
clear all; close all; clc;
f = @(x) x.^2 %@ indicates that the function is anonymous
f(1)
f(2)
f(3)
x = linspace(-10,10,100);
plot(x,f(x))

%let's do a more complex example putting f in a separate file function
clear all; close all; clc;
theta=-30:0.1:30;
n=1;
a1 = 1/(n+1);
b1 = n/(n+1);
a2 = 1/(1+n^0.5);
b2 = n^0.5/(1+n^0.5);
yn1 = ex_function(theta,0,1);
yn2 = ex_function(theta,a1,b1);
yn3 = ex_function(theta,a2,b2);
yn4 = ex_function(theta,1,0);
hold on
plot(theta,yn1)
plot(theta,yn2)
plot(theta,yn3)
plot(theta,yn4)
ylim([-0.2 2])
xlabel('theta')
ylabel('risk')
legend('procedure1','procedure2','procedure3','procedure4')
hold off

% create a class, which allows to put functions in a same file and access
% them from a script

%access the functions by starting with the class' name + .
%after dot, press tab and Matlab will return all functions available to you

ex_class2.square_function(2)
ex_class2.ex_function(theta,1,0)

%% Solving Linear Equations with MATLAB
clear all;
close all;
clc;
% Example 1: You want to solve for x1 and x2 and you have 2 equations as follows:
% 3*x1 - 9*x2 = -42
% 2*x1 + 4*x2 =   2
A = [3 -9; 2 4]
b = [-42; 2]
% to solve you will need X = inv(A)*b 
X = inv(A)*b

% Example 2: You want to solve for x1, x2 and x3 and you have 3 equations as follows:
%   x1 - 2*x2 -  x3 =  6
% 2*x1 + 2*x2 =  x3 +  1
% 2*x3 -    1 =  x1 + x2
clc;
A = [1 -2 -1; 2 2 -1; -1 -1 2]
b = [6; 1; 1]
% to solve you will need X = inv(A)*b 
X = inv(A)*b