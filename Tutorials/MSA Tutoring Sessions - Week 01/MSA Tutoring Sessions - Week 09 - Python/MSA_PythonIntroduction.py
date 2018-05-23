#### MSA Introductory Course in Python
### To access help press F1 or go to the Help Menu
### whenever you find brackets inside a function, that means that command is optional
### e.g. input([prompt]): prompt is optional, therefore can be omitted
### Ctrl+c stops program from running
### MUST respect indentation in Python
### reset on the console to clear variables
### clear on the console to clear console
### access help(function_name)
# Part 1 - #%% creates cells

1+1 # will not be printed in the shell
print(2+2)
variable = 1+1
print(variable)
type(variable)
type(2)  #integer
type(0.5)  #float
name = 'Mariana' #or 
surname = " de Almeida Costa"
print(surname.split()) #get the words in a list
fullname = name + surname # (string concatenation is done through + )
print(fullname)
print("_" * 80) #see how cool that is! You can also multiply strings
print( 'What if we want to display the "quote marks" ? Then you must use singles quotes ')
print( """To insert veeeery long texts that use more than one line you can
start and end with 3 double quotes " or 3 single quotes '. Python will read
everything like it's a huge text! """)
# Part 2
a = "Problem"
b = 1
print("{} - {}".format(a,b))#pairs of curly brackets are place holders for some data
#format you automatically convert a and b to string and place them inside the brackets
# some string functions
"Mariana".count("a")
# or simply
name.count("a")
print(name.upper())  #upper case all letters
print(name.lower())   #lower case all letters
name.islower()
print("hello world".capitalize())  #capitalize the first letter only
print("hello world".title()) #capitalize the first letter of each word
"hello world".index("world") #index starts from 0 and counts  blank space as well
"hello world".find("world") #find does the same, but returns -1 when error
"hello world".find("mariana")
"000000hello world000000".strip("0") #strip away to get rid of things
"000000hello world000000".lstrip("0")
name = input("What's your name? ").strip() # will remove all blank spaces to the left or right of whatever the user enters
print(len(name)) #length
print(name[3])
print(name[-3]) #starts to count from the end
print(name[0:10]) #to slice use variable[start:end:step] with default for step = 1
print(name[10:]) #slice from index 10 and show everything up to the last element
print(name[10::2]) # same as above but with step 2
print(name[::-1]) #reverse words easily - cool thing about Python! 
"hello world".isalpha() #checks whether the text contains only letters or not (space here is NOT a letter)
"123".isdigit()
"hello123".isalnum() #alpha numeric: only letters or numbers
#Python arithmetic operators: '+' '-' '/' '*'
# We also have the mod operator which is '%'
# power operator is '**' or use the function pow(base, exponent) in math library
# ** returns an integer number and pow returns a float number (decimal)
print(10%5)
print(10/5) #whenever you divide in Python, you get a float number (decimal)
int(10/5) # to force it to turn into integer

# Part 3
# Boolean operators '==' '!=' '>' '<' '>=' '<='
print(3 == 2)
print(3 != 2)
print(type (3 != 2))

# Logical Operators not, and, or
print(not 3 != 2)
print(4>3 and 4>3)
print(2>3 or 4>3)


#Random number generation
import random  #this is how you import a library
print(random.randint(0,100))
# or alternatively
from random import randint
print(randint(0,100))


#Math mode
print(round(1.26667, 3))
import math
print(math.ceil(1.26667))
print(math.floor(1.26667))
print(math.pi)
print(math.e)
#library also has trigonometric functions (unit is radians)
print(math.sin(math.pi)) #not zero because python use an approximation for pi
print(math.floor(math.sin(math.pi))) 
print(math.asin(0)) #returns the inverse of sin function
print(math.acos(0)) #returns the inverse of sin function
print(math.exp(2)) # e power
print(math.log(math.e)) #natural log
# you can also use math.log10 or math.log2 for other basis

#or alternatively do
from math import log  # you can do this for any function
print(log(10))

# Part 4
#vectors, lists and matrices
vector = [1,2,3,4,5]
print(vector)
vector[4]
my_list = [1,2,3,"a", "b","c", True, False]
print(my_list)
print(type(my_list))
print(my_list[0:2])
my_list = [1,2,3,["a", "b","c"], True, False] #list inside list
print(my_list[3])
print(my_list[3][0])
print("a" in my_list) #returns boolean True if the list contains a, False if not
my_list.append("word") # add "word" to the end of the list
my_list.remove("word")  #if you don't know the position, use .remove, but be careful, remove just removes the first observation that matches
my_list.insert(0,"word") #insert word at index 0
del my_list[0]  #if you know the position, use del
# be careful when using append or insert: you cannot assign it to an object like 'variable=my_list.append()' because lists are mutable objects
other_list = ["add", "me", "please"]
print(other_list)
full_list=my_list + other_list #can only combine lists with lists
full_list = full_list + ["this_is_a_list"] + list("ABC")+list(str(456))+ [7,8,9,10]
print(full_list)
A,B,C="123"
print(A)
print(B)
print(C)
#lists are mutable, that means they can be changed at anytime. For imutable "lists" we use Tuples in Python
my_tuple= 1,2,3,"a", "b","c"
print(my_tuple)
type(my_tuple)
print(my_tuple[0:2]) #you basically slice the same way as done with lists, except that you cannot change it
# for example, cannot do my_tuple[2] = 4 (you could to it with lists)
my_new_tuple = tuple(full_list) #converts full_list into tuple

# Part 5
#dictionaries start and end with { } 
students={
    "Alice":["ID001",26, "Michigan"],
    "Bob":["ID001",27, "Atlanta"],
    "John":["ID001",24, "New York"],
    "Mary": ["ID001",23, "San Francisco"],
    "Dan":["ID001",28, "Houston"]
    }
print(students["Alice"])
print(students["Alice"][0])
print(students["Alice"][1:])
#dictionaries are very useful, and you may create dictionaries inside dictionaries too

# Example 1
# Loopings!!!!! Let's work with some examples because at this point you're probably familiar with these structures
# Let's create a code to run a cinema
films = {
    "Finding Dory":{"seats":3,"age":0,"ticket":8},
    "Mission Impossible":{"seats":1,"age":12,"ticket":10},
    "Titanic":{"seats":0,"age":18,"ticket":10},
    "Avatar":{"seats":1,"age":12,"ticket":8},
    "The Simpsons":{"seats":1,"age":14,"ticket":8}
    }

while True: 
    
    choice = input("Which movie would you like to watch?").strip().title()
    if choice in films:
        age = int(input("How old are you? ").strip())

        if age >= films[choice]["age"] and films[choice]["seats"] > 0:
             print("Enjoy the Movie!")
             films[choice]["seats"]= films[choice]["seats"] - 1
        elif age >= films[choice]["age"] and \
        films[choice]["seats"] == 0:
             print("Sorry, we don't have anymore tickets available!")
        else:
             print("Sorry, you are too young to see this movie!")
    else:
        print("We don't have this film")
        break #could also use continue - they work for loops For and While

# Example 2
# Code to print all even numbers from 0 to 1000 
for number in range(0,1000,2):
    print(number)
# Example 3    
#or you could just write
even_numbers =[x for x in range(0,1001) if x % 2 == 0]  #remember % is the mod function
print(even_numbers)

# Example 4
#another example with lists
weird_list =["I", "am", "not", "sure"]
not_so_weird_list = [[l.upper(),l.lower(), len(l)] for l in weird_list]
print(not_so_weird_list)

# Example 5
# Code to count vowels and consonants in a sentence
vowels=0
consonants=0

for letter in "I love Python":
    if letter.lower() in "aeiou":
        vowels = vowels+1
    elif letter == "":
        pass
    else:
        consonants = consonants+1

print("There are {} vowels and {} consonants.".format(vowels, consonants))

# Functions
    
#Functions in Python
a = 25 #this a is a global variable
def add(x,y,b=4): #def tells Python that we are DEFining a function, add is the name of the function and x,y are its parameters
    # b is the default parameter, if the user does not insert it, the function assumes it is 4
    #default parameters must ALWAYS be the last argument(s) of the function
     a = 10 #a is a local scope variable... it's not gonna be available out of the function, because functions always create local scope inside them
     return x + y+ b+ a #return is NOT the same as print, return will store the result if assigned to a variable, print will just print
    #use global a to use the global variable a instead of the local
num1 = add(5,10) #call the function
print(num1)
num2 = add(5,10,1)
print(num2)
print(a)  #global variable a =25 will be printed, not a inside the function :). After functions run, the local variables are erased from memory.

numbers = [1,2,3,4,5,6,7,8,9,10]
print(numbers)
print(*numbers)  #packing or unpacking  * args arguments (args) into a function
def add2(*numbers): #very useful for large lists
    total = 0
    for i in numbers:
        total = total + i
    return(total)
    print(total)
    
print(add2(*even_numbers))

def func(**arguments): #packing or unpacking ** kwards arguments from a dictionary for the function
    for key, value in arguments.items():
        print("{}:{}".format(key,value))

func(film="Titanic", ticket=0, age=12)
#returns a dictionary





