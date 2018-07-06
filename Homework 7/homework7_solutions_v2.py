####### Homework 11 Question 2 using PuLP ######

# ---------- Import modules -----------

#import PuLP and pandas modules

from pulp import *
import pandas as pd


# ------------ Read data ---------------

data = pd.read_excel("diet.xls", header = 0) # read all data

dataTable = data[0:64] # rows 0:64 (Excel calls them 1-65) is the food data table
dataTable = dataTable.values.tolist() # Convert dataframe to list

nutrientNames = list(data.columns.values) # column headers (nutrient names are in columns 3-13; Excel calls them D-N)

minVal = data[65:66].values.tolist() # minimum nutrient values
maxVal = data[66:67].values.tolist() # maximum nutrient values
    

# ------------ Extract individual vectors of data ------------ 
#
# We could do this just by reading vectors, but here it's shown using the python "dict" structure
# which is more efficient

foods = [j[0] for j in dataTable] #list of food names

cost = dict([(j[0], float(j[1])) for j in dataTable]) # cost for each food

nutrients = []
for i in range(0,11): # for loop running through each nutrient: 11 times starting with 0
    nutrients.append(dict([(j[0], float(j[i+3])) for j in dataTable])) # amount of nutrient i in food j


# ------------ Create a new LP Problem ------------ 
#
# This problem is a minimization problem (find the *lowest* cost), so "LpMinimize" is the second parameter.

prob = LpProblem('Food optimization', LpMinimize) # 2 parameters: "name" and "sense"


# ------------ Define the variables ---------------
#
# One variable (we chose the name "foodVars") for each food.
# Lower limit of each variable is 0, since we can't eat negative amounts of anything.

foodVars = LpVariable.dicts("Foods", foods, 0)
foodVars_selected = LpVariable.dicts("food_select",foods,0,1,LpBinary) #Create binary integer variables for whether a food is eaten


# ------------ Create objective function ------------ 
#
# Note that the first function we add is taken to be the objective function

prob += lpSum([cost[f] * foodVars[f] for f in foods]), 'Total Cost'


# ------------ Add constraints for each nutrient ------------ 

for i in range(0,11): # for loop running through each nutrient: 11 times starting with 0
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) >= minVal[0][i+3], 'min nutrient ' + nutrientNames[i]
    prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) <= maxVal[0][i+3], 'max nutrient ' + nutrientNames[i]


# ------------ Adding additional constraints ------------   

# CONSTRAINT A

# If a food is eaten, must eat at least 0.1 serving

for food in foods:
    prob += foodVars[food] >= 0.1 * foodVars_selected[food]

# If any of a food is eaten, its binary variable must be 1

for food in foods:
    prob += foodVars_selected[food] >= foodVars[food]*0.0000001 

# CONSTRAINT B

# Include at most 1 of celery and frozen brocolli

prob += foodVars_selected['Frozen Broccoli'] + foodVars_selected['Celery, Raw'] <= 1 

# CONSTRAINT C

# At least 3 kinds of meat/poultry/fish/eggs

prob += foodVars_selected['Roasted Chicken'] + foodVars_selected['Poached Eggs'] \
        + foodVars_selected['Scrambled Eggs'] + foodVars_selected['Bologna,Turkey'] \
        + foodVars_selected['Frankfurter, Beef'] + foodVars_selected['Ham,Sliced,Extralean'] \
        + foodVars_selected['Kielbasa,Prk'] + foodVars_selected['Pizza W/Pepperoni'] \
        + foodVars_selected['Hamburger W/Toppings'] \
        + foodVars_selected['Hotdog, Plain'] + foodVars_selected['Pork'] \
        + foodVars_selected['Sardines in Oil'] + foodVars_selected['White Tuna in Water'] \
        + foodVars_selected['Chicknoodl Soup'] + foodVars_selected['Splt Pea&Hamsoup'] \
        + foodVars_selected['Vegetbeef Soup'] + foodVars_selected['Neweng Clamchwd'] \
        + foodVars_selected['New E Clamchwd,W/Mlk'] + foodVars_selected['Beanbacn Soup,W/Watr'] >= 3


# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output in a readable format ----------- 

print()
print("---------The solution to the diet problem is----------")
for var in prob.variables():
    if var.varValue > 0 and "food_select" not in var.name: # Print non binary variables
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','') )
print()
print("Total cost of food = $%.2f" % value(prob.objective))        