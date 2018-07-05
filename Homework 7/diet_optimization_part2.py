"""
ISYE6501 - Advanced Analytics Modeling

Question 15.2

In the videos, we saw the “diet problem”. 
(The diet problem is one of the first large-scale optimization problems to be 
studied in practice. Back in the 1930’s and 40’s, the Army wanted to meet the 
nutritional requirements of its soldiers while minimizing the cost.) 
In this homework you get to solve a diet problem with real data. 
The data is given in the file diet.xls.


1. Formulate an optimization model (a linear program) to find the 
cheapest diet that satisfies the maximum and minimum daily nutrition 
constraints, and solve it using PuLP. Turn in your code and the solution. 
(The optimal solution should be a diet of air-popped popcorn, poached eggs, 
oranges, raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)

2. Please add to your model the following constraints 
(which might require adding more variables) and solve the new model:
a. If a food is selected, then a minimum of 1/10 serving must be chosen. 
(Hint: now you will need two variables for each food i: 
whether it is chosen, and how much is part of the diet.
You’ll also need to write a constraint to link them.)
    
b. Many people dislike celery and frozen broccoli. 
So at most one, but not both, can be selected.

c. To get day-to-day variety in protein, 
at least 3 kinds of meat/poultry/fish/eggs must be selected. 
[If something is ambiguous (e.g., should bean-and-bacon soup be considered

Created on Mon Jul  2 19:29:56 2018

@author: zacholivier


Part 2


"""

# load the libraries needed
# !pip install pulp

from pulp import *
import pandas as pd


# load the diet data
df = pd.read_excel(
        open(
            '/Users/zacholivier/Desktop/GTX/Homework 7/dietSummer2018.xls',
            'rb'
                ),
                sheet_name='Sheet1'
                )


# look at the data 
df.head()

# clean data - take first 64 rows not including bottom data
data = df[0:64]

# convert to list "list within a list"
data = data.values.tolist()


# create master foods dictionary
foods = [x[0] for x in data]
calories = dict([(x[0], float(x[3])) for x in data])
cholesterol = dict([(x[0], float(x[4])) for x in data])
totalFat = dict([(x[0], float(x[5])) for x in data])
sodium = dict([(x[0], float(x[6])) for x in data])
carbs = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protien = dict([(x[0], float(x[9])) for x in data])
vitaminA = dict([(x[0], float(x[10])) for x in data])
vitaminC = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])



# create list for mins and maxes (all foods)
amin = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
amax = [2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]


# append collection of contraints for each column 
B = []
for j in range(0,11):
    B.append(dict([(x[0], float(x[j+3])) for x in data]))


# define the cost dictionary
cost = dict([(x[0], float(x[1])) for x in data])



# create the optimization problem framework - minimization problem
problem2 = LpProblem('PuLPTutorial', LpMinimize)




# define the variables - continous
foodVars = LpVariable.dicts("foods", foods,0)


# define the variables - binary
chosenVars = LpVariable.dicts("Chosen",foods,0,1,"Binary")


# dictionary of lp variables 
x = LpVariable.dicts("x", foods, 0)


# define the objective function
problem2 += lpSum([cost[f] * foodVars[f] for f in foods])


# add contraints amount greater than .1 or less than large amount - if chosen
for f in foods:
    problem2 += foodVars[f] <= 10000 * chosenVars[f]
    problem2 += foodVars[f] >= .1 * chosenVars[f]



# add constraints for all foods
for i in range(0,11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition1 = amin[i] <= + dot_B_x
    problem2 += condition1
    
for i in range(0,11):
    dot_B_x = pulp.lpSum([B[i][j] * foodVars[j] for j in foods])
    condition2 = amax[i] >= + dot_B_x
    problem2 += condition2
    
    
# add contraints to eat at most one of a group of foods    
problem2 += chosenVars['Frozen Broccoli'] + \
chosenVars['Celery, Raw'] <= 1, 'At most one Broccoli / Celery'


# add contraints that says we require to eat as least 1 from group of food
problem2 += chosenVars['Roasted Chicken'] + chosenVars['Poached Eggs'] + \
  chosenVars['Scrambled Eggs'] + chosenVars['Frankfurter, Beef'] + \
  chosenVars['Kielbasa,Prk'] + chosenVars['Hamburger W/Toppings'] + \
  chosenVars['Hotdog, Plain'] + chosenVars['Pork'] + \
  chosenVars['Bologna,Turkey'] + chosenVars['Ham,Sliced,Extralean'] + \
  chosenVars['White Tuna in Water'] \
  >= 3, 'At least three proteins'





# solve the optimization problem!
problem2.solve()


# print the foods of the optimal diet
print('Optimization Solution:')
for var in problem2.variables():
    if var.varValue > 0:
        if str(var).find('Chosen'):
            print(str(var.varValue) + " units of " + str(var))
            
# print the costs of the optimal diet             
print("Total cost of food = $%.2f" % value(problem2.objective))