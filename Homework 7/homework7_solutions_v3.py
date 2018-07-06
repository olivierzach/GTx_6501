####### Homework 11 Question 2 using PuLP ######
#
# Solving the large diet problem with 7147 foods and 29 foods (plus cholesterol to minimize)

# ---------- Import modules -----------

#import PuLP and pandas modules

from pulp import *
import pandas as pd
import numpy as np

# ------------ Read data ---------------

data = pd.read_excel("diet_large.xls", skiprows = 1, header = 0) # read all data

dataTable = data[0:7146] # rows 0:7146 (Excel calls them 2-7148; remember we skipped the blank first row in the read call) is the food data table
dataTable = dataTable.values.tolist() # Convert dataframe to list

nutrientNames = list(data.columns.values) # column headers (nutrient names are in columns 3-13; Excel calls them D-N)
numNutrients = len(nutrientNames) - 1 # don't count the food-name column

# blank elements are read as 'nan', so need to replace them with zero

for i in range(0,7146):
    for j in range(1,numNutrients):
        if np.isnan(dataTable[i][j]):
            dataTable[i][j] = 0

minVal = data[7147:7148].values.tolist() # minimum nutrient values
maxVal = data[7149:7151].values.tolist() # maximum nutrient values


# ------------ Extract individual vectors of data ------------ 

foods = [j[0] for j in dataTable] #list of food names

cost = dict([(j[0], float(j[nutrientNames.index('Cholesterol')])) for j in dataTable]) # cost for each food

nutrients = []
for i in range(0,numNutrients): # for loop running through each nutrient
    nutrients.append(dict([(j[0], float(j[i+1])) for j in dataTable])) # amount of nutrient i in food j


# ------------ Create a new LP Problem ------------ 
#
# This problem is a minimization problem (find the *lowest* cost), so "LpMinimize" is the second parameter.

prob = LpProblem('Food optimization', LpMinimize) # 2 parameters: "name" and "sense"


# ------------ Define the variables ---------------
#
# One variable (we chose the name "foodVars") for each food.
# Lower limit of each variable is 0, since we can't eat negative amounts of anything.

foodVars = LpVariable.dicts("Foods", foods, 0)


# ------------ Create objective function ------------ 
#
# Note that the first function we add is taken to be the objective function

prob += lpSum([cost[f] * foodVars[f] for f in foods]), 'Total Cost'


# ------------ Add constraints for each nutrient ------------ 

for i in range(0,numNutrients): # for loop running through each nutrient
    if (not np.isnan(minVal[0][i+1])) and (not np.isnan(maxVal[0][i+1])): # only write a constraint if upper and lower bounds exist
        print("adding constraint for " + nutrientNames[i+1])
        prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) >= minVal[0][i+1], 'min nutrient ' + nutrientNames[i+1]
        prob += lpSum([nutrients[i][j] * foodVars[j] for j in foods]) <= maxVal[0][i+1], 'max nutrient ' + nutrientNames[i+1]
            

# ------------ Solve the optimization problem ------------ 

prob.solve()


# ------------ Print the output in a readable format ----------- 

print()
print("---------The solution to the diet problem is----------")
for var in prob.variables():
    if var.varValue > 0:
        print(str(var.varValue)+" units of "+str(var).replace('Foods_','') )
print()
print("Total cholesterol = %f" % value(prob.objective))