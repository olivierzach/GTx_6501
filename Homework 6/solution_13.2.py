####### Homework 9 Question 4 using Simpy ######

# ---------- Import modules -----------

#import SimPy module

import simpy

# Import random module

import random

# ------------ Set constants ---------------

numCheckers = 35 # number of boarding-pass checkers
numScanners = 35 # number of scanners

arrRate = 50 # arrival rate (passengers per minute)
checkRate = 0.75 # boarding-pass check rate (minutes per passenger)
minScan = 0.5 # scanner minimum time for uniform distribution
maxScan = 1.0 # scanner maximum time for uniform distribution
runTime = 720 # run time (minutes) per simulation
replications = 100 # number of replications

# ------------ Initialize global variables ----------

avgCheckTime = [] # average boarding-pass check time (for each replication)
avgScanTime = [] # average scan time (for each replication)
avgWaitTime = [] # average total wait time (for each replication)
avgSystemTime = [] # average total time in system (for each replication)

# ------------ Create model ------------------

# System class

class System(object):
    def __init__(self,env):
        self.env = env
        self.checker = simpy.Resource(env,numCheckers) # define number of boarding-pass checkers
        self.scanner = [] # define a set of scanners with 1 each; needed because each has its own queue
        for i in range(numScanners):
            self.scanner.append(simpy.Resource(env,1))

    # define boarding-pass check time (exponential)
    def check(self,passenger):
        # For some reason in python, expovariate actually uses 1 over the mean, like Poisson
        yield self.env.timeout(random.expovariate(1.0/checkRate))

    # define scan time (uniform)
    def scan(self,passenger):
        yield self.env.timeout(random.uniform(minScan,maxScan))

# Passenger process through system

def passenger(env,name,s):

    # access global variables to be able to modify them
    global checkWait
    global scanWait
    global sysTime
    global totThrough

    timeArrive = env.now # note arrival time of passenger


    # print('%s arrives at time %.2f' % (name,timeArrive))

    # Go through boarding-pass check queue
    with s.checker.request() as request:
        # print('check queue length = %d' % len(s.checker.queue))
        yield request # request a checker
        tIn = env.now # note when passenger starts being checked
        yield env.process(s.check(name)) # call check process
        tOut = env.now # note when passenger ends being checked
        checkTime.append(tOut - tIn) # calculate total time for passenger to be checked

    # Find the shortest scanner queue (note: scanners are numbered 0 through numScanners-1)
    minq = 0
    for i in range(1,numScanners):
        if (len(s.scanner[i].queue) < len(s.scanner[minq].queue)):
            minq = i

    # print('scanner queue %d lengths = %d' % (minq,len(s.scanner[minq].queue)))

    # Go through scanner queue
    with s.scanner[minq].request() as request: # use scanner number minq (the shortest, from above)
          yield request # request the scanner
          tIn = env.now # note when passenger starts being scanned
          yield env.process(s.scan(name)) # call scan process
          tOut = env.now # note when passenger ends being scanned
          scanTime.append(tOut - tIn) # calculate total time for passenger to be scanned
          
    timeLeave = env.now # note time passenger finishes
    sysTime.append(timeLeave - timeArrive) # calculate total time in system for passenger
    totThrough += 1 # count another passenger who got through the system


# Passenger arrival process

def setup(env):
    i = 0
    s = System(env)
    while True: # keep doing it (until simulation ends)
        yield env.timeout(random.expovariate(arrRate)) # find tieme until next passenger is created
        i += 1 # count one more passenger

        # send the passenger through its process
        env.process(passenger(env,'Passenger %d' % i,s)) # name the passenger "Passenger i"
        

# ------------------ Run the model --------------------

# for each replication
for i in range(replications):

    # choose random seed
    random.seed(i)

    # create environment
    env = simpy.Environment()

    # initialize global variables
    totThrough = 0
    checkTime = []
    scanTime = []
    sysTime = []

    # run the simulation
    env.process(setup(env)) # start passenger arrival process
    env.run(until=runTime) # run for runTime simulated minutes

    # Calculate average times for this replication
    
    avgSystemTime.append(sum(sysTime[1:totThrough]) / totThrough)
    avgCheckTime.append(sum(checkTime[1:totThrough]) / totThrough)
    avgScanTime.append(sum(scanTime[1:totThrough]) / totThrough)
    avgWaitTime.append(avgSystemTime[i] - avgCheckTime[i] - avgScanTime[i])

    print('%d : Replication %d times %.2f %.2f %.2f %.2f' % (totThrough,i+1,avgSystemTime[i],avgCheckTime[i],avgScanTime[i],avgWaitTime[i]))

# Calculate overall averages across all replications

print('-----')    
print('Average system time = %.2f' % (sum(avgSystemTime)/replications))
print('Average check time = %.2f' % (sum(avgCheckTime)/replications))
print('Average scan time = %.2f' % (sum(avgScanTime)/replications))
print('Average wait time = %.2f' % (sum(avgWaitTime)/replications))
