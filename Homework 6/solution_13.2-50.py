####### Homework 9 Question 4 using Simpy ######

# ---------- Import modules -----------

#import SimPy module

import simpy

# Import random module

import random

# ------------ Set constants ---------------

numCheckers = 35 # smallest number of boarding-pass checkers
numScanners = 35 # smallest number of scanners

arrRate = 50 # arrival rate (passengers per minute)
checkRate = 0.75 # boarding-pass check rate (minutes per passenger)
minScan = 0.5 # scanner minimum time for uniform distribution
maxScan = 1.0 # scanner maximum time for uniform distribution
runTime = 720 # run time (minutes) per simulation
replications = 100 # number of replications

# ------------ Initialize global variables ----------

avgCheckTime = [[0.0 for i in range(6)] for j in range(6)] # average boarding-pass check time (for each replication)
avgScanTime = [[0.0 for i in range(6)] for j in range(6)] # average scan time (for each replication)
avgWaitTime = [[0.0 for i in range(6)] for j in range(6)] # average total wait time (for each replication)
avgSystemTime = [[0.0 for i in range(6)] for j in range(6)] # average total time in system (for each replication)

# ------------ Create model ------------------

# System class

class System(object):
    def __init__(self,env,i,j):
        self.env = env
        self.checker = simpy.Resource(env,i+numCheckers) # define number of boarding-pass checkers
        self.scanner = [] # define a set of scanners with 1 each; needed because each has its own queue
        for i in range(j+numScanners):
            self.scanner.append(simpy.Resource(env,1))

    # define boarding-pass check time (exponential)
    def check(self,passenger):
        # For some reason in python, expovariate actually uses 1 over the mean, like Poisson
        yield self.env.timeout(random.expovariate(1.0/checkRate))

    # define scan time (uniform)
    def scan(self,passenger):
        yield self.env.timeout(random.uniform(minScan,maxScan))

# Passenger process through system

def passenger(env,name,s,i,j,pnum):

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
        checkTime[pnum] = (tOut - tIn) # calculate total time for passenger to be checked

    # Find the shortest scanner queue (note: scanners are numbered 0 through numScanners-1)
    minq = 0
    for k in range(1,j+numScanners):
        if (len(s.scanner[k].queue) < len(s.scanner[minq].queue)):
            minq = k

    # print('scanner queue %d lengths = %d' % (minq,len(s.scanner[minq].queue)))

    # Go through scanner queue
    with s.scanner[minq].request() as request: # use scanner number minq (the shortest, from above)
          yield request # request the scanner
          tIn = env.now # note when passenger starts being scanned
          yield env.process(s.scan(name)) # call scan process
          tOut = env.now # note when passenger ends being scanned
          scanTime[pnum] = (tOut - tIn) # calculate total time for passenger to be scanned
          
    timeLeave = env.now # note time passenger finishes
    sysTime[pnum] = (timeLeave - timeArrive) # calculate total time in system for passenger
    totThrough += 1 # count another passenger who got through the system


# Passenger arrival process

def setup(env,i,j):
    k = 0
    s = System(env,i,j)
    while True: # keep doing it (until simulation ends)
        yield env.timeout(random.expovariate(arrRate)) # find tieme until next passenger is created
        k += 1 # count one more passenger

        # send the passenger through its process
        env.process(passenger(env,'Passenger %d' % k,s,i,j,k)) # name the passenger "Passenger i"
        

# ------------------ Run the model --------------------

for i in range(6): # number of boarding-pass checkers
    for j in range(6): # number of scanners
        
        # for each replication
        for k in range(replications):

            # choose random seed
            random.seed(k)

            # create environment
            env = simpy.Environment()

            # initialize global variables
            totThrough = 0
            checkTime = [0.0] * int(arrRate*runTime*1.5)
            scanTime = [0.0] * int(arrRate*runTime*1.5)
            sysTime = [0.0] * int(arrRate*runTime*1.5)

            # run the simulation
            env.process(setup(env,i,j)) # start passenger arrival process
            env.run(until=runTime) # run for runTime simulated minutes

            # Calculate average times for this replication

            # print('%d : Replication %d times %.2f %.2f %.2f' % (totThrough,k+1,sum(sysTime[1:totThrough]) / totThrough,sum(checkTime[1:totThrough]) / totThrough,sum(scanTime[1:totThrough]) / totThrough))

            avgSystemTime[i][j] += (sum(sysTime[1:totThrough]) / totThrough)
            avgCheckTime[i][j] += (sum(checkTime[1:totThrough]) / totThrough)
            avgScanTime[i][j] += (sum(scanTime[1:totThrough]) / totThrough)

        avgWaitTime[i][j] = (avgSystemTime[i][j] - avgCheckTime[i][j] - avgScanTime[i][j])


        # Calculate overall averages across all replications

        avgSystemTime[i][j] /= replications
        avgCheckTime[i][j] /= replications
        avgScanTime[i][j] /= replications
        avgWaitTime[i][j] /= replications
        
        print('----- %d -- %d -----' % (i+35,j+35))    
        print('Average system time = %.2f' % avgSystemTime[i][j])
        print('Average check time = %.2f' % avgCheckTime[i][j])
        print('Average scan time = %.2f' % avgScanTime[i][j])
        print('Average wait time = %.2f' % avgWaitTime[i][j])
