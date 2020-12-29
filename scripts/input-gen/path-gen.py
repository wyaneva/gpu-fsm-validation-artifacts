# Dijkstra's Algorithm

import sys
import itertools
import os
from collections import deque

fsmfilename = sys.argv[1]
outputdir = sys.argv[2]

# read the transitions
with open(fsmfilename, 'r') as fsmfile: # the fsm file
    fsmdata=fsmfile.readlines()
fsmfile.close()

# a dictionary of neighbours
# the key is each state 
# the value is a list of tuples: (next state, transition)
neighbours = {}
unvisited = []
start = -1
for i in range(0, len(fsmdata)):
    ele = fsmdata[i].strip().split()

    if(len(ele)==4):
        inputele = ele[0]
        currstate = int(ele[1])
        nextstate = int(ele[2])

        if start == -1:
            start = currstate

        if currstate not in neighbours:
            neighbours[currstate] = []

        if currstate != nextstate:
            neighbours[currstate].append((nextstate, inputele))

        if currstate not in unvisited:
            unvisited.append(currstate)

# a dictionary, where the key is each state and the value is the current shortest path
paths = {}
current = start
while(len(unvisited) != 0):

    if current not in paths:
        paths[current] = []

    currentpath = paths[current]
    for (nb, trans) in neighbours[current]:
        dist = len(currentpath) + 1

        if nb not in paths or dist < len(paths[nb]):
            paths[nb] = currentpath.copy()
            paths[nb].append(trans)

    # current state is now visited
    unvisited.remove(current)

    # find unvisited node with shortest path to explore next
    minlen = -1
    oldcurrent = current
    for state in unvisited:

        if state not in paths:
            continue

        pathlen = len(paths[state])
        if minlen == -1 or minlen > pathlen:
            minlen = pathlen
            current = state

    # we have not found a new unvisited set with shortest path
    # thus, there is no connection between the initial state
    # and the rest of the unvisited states, so stop
    if oldcurrent == current:
        break;

# new file with all the paths
namefile=fsmfilename.rsplit('/')[-1]
filewrite = open(os.path.join(outputdir+"/paths",namefile),"w")
for (state, path) in paths.items():
    strpath = ""
    for trans in path:
        strpath += trans

    filewrite.write(str(state) + " " + strpath +"\n")
filewrite.close()
