clc
close all
clear all
disp('PCB in Yolks of Pelican Eggs, Book exercise page 882') 
% PCB in Yolks of Pelican Eggs.
% A well known data set for testing agreement with normal distribution comes from
% Risebrough (1972) who explored concentrations of polychlorinated biphenyl (PCB)
% in yolk lipids of pelican eggs. For  n=65   pelicans from Anacapa Island 
% (north-west of LA), the concentrations of PCB were
%   
dataset=[452   184   115   315   139   177   214   356   166   246   177   289   175 ... 
   296   205   324   260   188   208   109   204    89    320   256   138   198  ...
   191   193   316   122   305   203   396   250   230   214    46   256   204 ...
   150   218   261   143   229   173   132   175   236   220   212   119   144 ...
   147   171   216   232   216   164   185   216   199   236  237   206  87 ];

%% Pearson's Chi-Square Test
n = length(dataset);
classes = 2*round(n^(2/5));  % classes = 10
hist(dataset,classes)
% frequencies table
sort_data = sort(dataset);
ftable = zeros(classes,1); 
percentiles = prctile(dataset, [0 10 20 30 40 50 60 70 80 90 100]);
for i = 1:classes
    ftable(i) = sum((sort_data > percentiles(i)) & (sort_data <= percentiles(i+1))) ; % frequencies count
end
ftable(1)=ftable(1)+sum(sort_data==percentiles(1)); % correction for 1st obs 
sum(ftable,1) %must be = n
% Chi-Square
expected = n * diff(normcdf(percentiles,mean(dataset), std(dataset)));
chisqs = (ftable' - expected).^2./expected;
chi2 = sum(chisqs)
r = length(percentiles)-1;
pval=1 - chi2cdf(chi2, r-1-2) % 2 df's lost to estimate mean and var
% 0.2312
% p-value =  0.2312, cannot reject H0

%% Kolmogorov's Test
% Suppose that the null distribution is fully specified as normal N(200,70^2).
i=1:n;
distances = [i./n - normcdf(sort_data, 200,70); normcdf(sort_data, 200,70) - (i-1)./n ];
Dn = max(max(distances)) %0.0932
pval = 1 - kscdf(sqrt(n)*Dn) %0.6251 %download kscdf from http://statbook.gatech.edu/Statbook/Ch17.Goodness/Goodnessmat/

%% Cramer-von Mises' Test
% Suppose that the null distribution is fully specified as normal N(200,70^2).
i1n = ( 2*(1:n) - 1 )/(2*n);
alpha=0.05
w2 = 1/(12 * n) + sum( (normcdf(sort_data, 200,70) - i1n).^2 ) %0.1192
a1 = (48*n^2 - 137*n + 87)/(30*(32*n^2 - 61*n + 30)) 
b1 = (32*n^2 - 61*n + 30)/(84*n*(4*n - 3))
k1 = (98*n*(4*n - 3)^3)/(5*(32*n^2 - 61*n + 30)^2) % 1.2551
w2crit = a1 + b1* chi2inv(1-alpha, k1) % 0.4650
chi2 = (w2 - a1)/b1 % 0.7482 The statistic chi2 has approx a chi2-dist with k1 df's
pval1 = 1 - chi2cdf(chi2, k1) %0.4760

