##### Optimization
#### Popular Optimization Solvers : CPLEX, Gurobi, GLPK, CLP, CBC, 
#### IPOPT (part of COIN-OR), LINDO, etc.
##### Python's Interface for opt: PuLP (Linear P.), OpenOpt (linear and non-linear)
#### PuLP is an LP modeler written in python. PuLP can generate MPS
#### or LP files and call GLPK, COIN CLP/CBC, CPLEX, and GUROBI to solve linear problems.


### we are going to install pip which is able to install any Python's libraries, including PuLP
# On iOS, go to Terminal (~/Applications/Utilities/Terminal)
## type curl https://bootstrap.pypa.io/get-pip.py > get-pip.py
### type sudo python get-pip.py (runs the script to get pip)
### enter your admin password
### sudo pip install PuLP
### Now PuLP should be installed in your computer
### default solver is CBC, which comes packaged with PuLP upon installation

### alternatively
### download PuLP at https://pypi.python.org/pypi/PuLP
#### Open terminal and go to the folder containing the downloaded file
### type sudo python setup.py install
### import pulp
### pulp.pulpTestAll()

#### We're gonna work with a simple LP Example
# The Optimization Problem
# max z  = 4x + 3y
# s.t.
# x≥0
# y≥2
# 2y≤25–x
# 4y≥2x–8
# y≤2x−5


### The solution is Z is 73.75, when x is 14.5 and y is 5.25. Let's see this below:
#Before we start PuLP, let's graph this problem

import numpy as np
import matplotlib.pyplot as plt  #need to install pyplot first (template: sudo pip install pyplot)

# Construct lines
# x > 0
x = np.linspace(0, 20, 2000) # returns 2000 evenly space numbers over [0,20]
# y >= 2
y1 = (x*0) + 2 #blue
# 2y <= 25 - x
y2 = (25-x)/2.0 #orange
# 4y >= 2x - 8 
y3 = (2*x-8)/4.0 #green
# y <= 2x - 5 
y4 = 2 * x -5 #red

# Make plot
plt.plot(x, y1, label=r'$y\geq2$')
plt.plot(x, y2, label=r'$2y\leq25-x$')
plt.plot(x, y3, label=r'$4y\geq 2x - 8$')
plt.plot(x, y4, label=r'$y\leq 2x-5$')
plt.xlim((0, 16))
plt.ylim((0, 11))
plt.xlabel(r'$x$')
plt.ylabel(r'$y$')

# Fill feasible region and insert legend
y5 = np.minimum(y2, y4)
y6 = np.maximum(y1, y3)
plt.fill_between(x, y5, y6, where=y5>y6, color='grey', alpha=0.5)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)


### Now introducing PuLP
import pulp 
#help(pulp.LpProblem)
my_lp_problem = pulp.LpProblem("My LP Problem", pulp.LpMaximize)
x = pulp.LpVariable('x', lowBound=0, cat='Continuous')
y = pulp.LpVariable('y', lowBound=2, cat='Continuous')

# Objective function
my_lp_problem += 4 * x + 3 * y, "Z"

# Constraints
my_lp_problem += 2 * y <= 25 - x
my_lp_problem += 4 * y >= 2 * x - 8
my_lp_problem += y <= 2 * x - 5
my_lp_problem
my_lp_problem.solve()
pulp.LpStatus[my_lp_problem.status]

# 5 possible status
# Not Solved: Status prior to solving the problem.
#Optimal: An optimal solution has been found.
#Infeasible: There are no feasible solutions (e.g. if you set the constraints x <= 1 and x >=2).
#Unbounded: The constraints are not bounded, maximising the solution will tend towards infinity (e.g. if the only constraint was x >= 3).
#Undefined: The optimal solution may exist but may not have been found.

for variable in my_lp_problem.variables():
    print ("{} = {}".format(variable.name, variable.varValue))
    
    
print (pulp.value(my_lp_problem.objective))

    