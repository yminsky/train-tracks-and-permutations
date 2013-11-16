#!/usr/bin/env python2.7

# First half of this is a bunch of permutation-handling routines, 
# and then an experiment: 
# Given a cycle c (i.e. (1,2,3,...,n) in cycle notation), 
# and a randomly chosen involution of the form
# T = (i1,j1)(i2,j2)...(i_{n/2},j_{n/2}), how often is Tc also a cycle?  
# Then if c1 and c2 are two cycles, how often are Tc1 and Tc2
# simultaneously cycles?
#
# Second half is a refined version where we use a train track with random 
# weights to build an interval exchange transformation, and from this two 
# cycles c1 and c2, # and now we want to be able to choose involutions T 
# such that Tc1 and Tc2 are simultaneously cycles. (I have to explain 
# this to you better...)

# permutations represented as a list [pi(1),...,pi(N)], 
# NOT in cycle notation.

import random
import pickle

def id(N):
    return range(N)

def rot1(N):
    r1 = range(1,N)
    r1.append(0)
    return r1

def randperm(N):
    rp=id(N)
    random.shuffle(rp)
    return rp

def compose(p,q):
    pq = []
#     assuming p, q same length!
    for i in range(len(q)):
        pq.append(p[q[i]])
    return pq

def inverse(p):
    N = len(p)
    inverse = id(N)
    for i in range(N):
        inverse[p[i]] = i
    return inverse

def iscycle(p):
    N=len(p)
    cyc_len = 1
    i=p[0]
    while i != 0:
        i = p[i]
        cyc_len = cyc_len+1
    if cyc_len == N: 
        return True
    else:
        return False


# build random involution that exchanges 1..N/2 with N/2+1..N
def involution(N):
    half = randperm(N/2)
    left = [x+N/2 for x in half]
    right = inverse(half)
    return left+right

################################################################################
# Try a lot of random involutions and see how often composition with cyc is a cycle.
def count1(N,M):
    scramble = randperm(N)
# conjugate rot1(N) to get a random cycle:
    cyc = compose(scramble,compose(rot1(N),inverse(scramble)))

    count = 0
    for i in range(M):
        if iscycle(compose(involution(N),cyc)): count = count+1
    return(count)

# Count how often composition with TWO different cycles is a cycle.
def count2(N,M):
    scramble = randperm(N)
    cyc = compose(scramble,compose(rot1(N),inverse(scramble)))
    scramble2 = randperm(N)
    cyc2 = compose(scramble2,compose(rot1(N),inverse(scramble2)))

    count = 0
    for i in range(M):
        inv = involution(N)
        if iscycle(compose(inv,cyc)) and iscycle(compose(inv,cyc2)):
            count = count+1
    return count


# version where we just start with a random perm, not cycle.
def count1r(N,M):
    scramble = randperm(N)
    count = 0
    for i in range(M):
        if iscycle(compose(involution(N),scramble)): count = count+1
    return(count)

#############################################################################


# Now the experiment
# N = 24
M = 50000

def exp1(M):
    print "Cycle counting experiment, M=",M
    for N in range(100,120,4):
        c1 = float(count1(N,M))
        c2 = float(count2(N,M))
        print "N=",N," C1:",c1/M, "C2: ",c2/M, "C1^2: ", (c1/M)*(c1/M)

exp1(M)

# for N in range(2,40,2):
#     c1 = count1r(N,M)
#     print "N=",N," C1r:",c1, "proportion:", float(c1)/M

###############################################################################
### stuff for getting IETs to work.
# permutation representation of an IET: k branches, connecting + and - sides of the interval.
# start with description reading left to right along interval
# branches[0] lists the incoming edges on top, left to right. 
# branches[1] lists them on bottom.

# This example is genus 2.
# branches = [ [0,1,2,0],[2,3,1,3]]
# number of strands in each branch. (need to satisfy switch condition!)
#width = [7,13,19,7]   

# Torus:
# branches = [ [0,1],[1,0] ]
# width = [ 21,37 ]

# Another genus 2, 5 branches:
branches = [ [0,1,2,0],[3,2,4,3,1,4]]
width = [6,2,3,4,2]



# Now build a description of the transformation from this. 
# It should map pairs (s,j) to pairs, where now j defines position of the j-th strand.
# s=0 for top side, s=1 for bottom.

def build_iet():
    global branches,width,breakpt,ends,affmap
    # for each branch want list of the position-pairs it ends in:
    # ends[b] = [ (s1,j1), (s2,j2) ] iff branches[s1][j1] = b and branches[s2][j2] = b:
    ends=[]
    for b in range(len(width)):
        ends.append([])
        for s in range(2):
            for j in range(len(branches[s])):
                if branches[s][j] == b:
                    ends[b].append((s,j))

#    print "ends:",ends

# find breakpoints of the interval:
# breakpt[s][i] is the index of the first (0th) strand of the i-th interval on the s side.
# we also get the "strand after the last", which is just the total width.

    breakpt = [[0],[0]]
    for s in range(2):
        for i in range(len(branches[s])):
            breakpt[s].append( width[branches[s][i]] + breakpt[s][i])

#    print "breakpt:",breakpt

# For each interval in top and bottom, record the transformation:
# need: for each (s,i): the branch branch[s][i] meets another pair (t,j).
# The affine map taking the intervals to each other is: 
#  when s != t:     x  ->  x + breakpt[t][j]-breakpt[s][i]
#  when s == t:     x  ->  -x + breakpt[t][j+1] + breakpt[s][i] - 1
#  data structure for this: affmap[s][i] is a pair (d,pm) 
#      encoding the map x -> pm*x + d

    affmap = []
    for s in range(2):
        affmap.append([])
        for i in range(len(branches[s])):
            b = branches[s][i]
            (t,j) = ends[b][0] if ends[b][0] != (s,i) else ends[b][1]
            if s != t:
                affmap[s].append( (breakpt[t][j]-breakpt[s][i], 1))
            else:
                affmap[s].append( (breakpt[t][j+1] + breakpt[s][i] - 1, -1) )


#    print "affmap:",affmap


def iet((s,x)):
    global breakpt,affmap
    i = 0
    while x >= breakpt[s][i+1]:
        i = i+1
    (d,pm) = affmap[s][i]
    y = pm * x + d
    t = s if pm == 1 else 1-s
    return((t,y))


# compute the permutation induced by a train track with weights. 
# Compute the signs of the intersection points. 
# If it is a cycle, return True.
# the permutation is stored in cycle, and the list of signs is sign.

def buildcycle():
    global branches, width, breakpt, cycle,sign
    xorbit = [0]
    signorbit = [0]
    N = 1
    (s,x) = iet((0,0))
    while (s,x) != (0,0):
        xorbit.append(x)
        signorbit.append(s)
        N = N+1
        (s,x) = iet((s,x))
    if N < breakpt[0][-1]:
        return False
    else: 
        cycle = [0 for x in range(N)]
        sign = [0 for x in range(N)]
        for i in range(N-1):
            cycle[xorbit[i]] = xorbit[i+1]
            sign[xorbit[i]] = 1 if signorbit[i]==0 else -1
        cycle[xorbit[N-1]] = xorbit[0]
        sign[xorbit[N-1]] = 1 if signorbit[N-1]==0 else -1
        return True


def choosetrack(num,W):
    if num == 0:
        br =  [ [0,1,2,0],[3,2,4,3,1,4]]
        a1 = random.randint(2,W)
        a2 = random.randint(2,W)
        a3 = random.randint(2,W)
        a4 = random.randint(2,W)
        a0 = a3+a4
        wid = [a0,a1,a2,a3,a4]
    elif num == 1:
        br = [[0,1,3,0,2,3],[4,5,2,5,4,1]]
        wid = [0,0,0,0,0,0]
        for i in range(4):
            wid[i] = random.randint(2,W)
        wid[4] = random.randint(2,wid[0]+wid[3]-2)
        wid[5] = wid[0]+wid[3]-wid[4]

    return (br,wid)

## def euler(br):
## Maybe write this? to compute Euler char of a surface from the train track.    

def findcycles(tracknum,W,M,P):
    global branches,width,zlist, onelist,bothlist
    zlist = []
    onelist = []
    bothlist = []
    for i in range(M):
        branches,width = choosetrack(tracknum,W)
        build_iet()
        if buildcycle() and sum(sign) == 0 and len(sign)%4 == 0:
            left = []
            right = []
            for i in range(len(sign)):
                if sign[i] == 1:
                    left.append(i)
                else:
                    right.append(i)
            signperm = left+right
            
            N = len(cycle)
            r1 = rot1(N)
            print "Zero-sign cycle of length",N
#            print cycle
            zlist.append(width)
            count1 = 0
            count2 = 0
            countboth = 0
            for i in range(P):
                inv = compose(signperm,compose(involution(N),inverse(signperm)))
                if iscycle(compose(inv,r1)):
                    count1 = count1+1
                    onelist.append([branches,width,inv,cycle])
                if iscycle(compose(inv,cycle)):
                    count2 = count2+1
                if iscycle(compose(inv,r1)) and iscycle(compose(inv,cycle)):
                    countboth = countboth+1
                    store([branches,width,inv,cycle])
            print P,"tries. cycle for one:",count1,"for two:",count2,"for both:",countboth
    



def store(list):
    f = open('examples','a')
    pickle.dump(list,f,1)
    f.close()

def retrieve():
    f = open('examples','r')
    u = []
    while True:
        try: 
            u.append(pickle.load(f))
        except EOFError:
            break
    f.close()
    return u


def unicorn(cycle):
    y = cycle[0]
    u = []
    while y != 0:
        u.append(y)
        while y >= u[-1]:
            y = cycle[y]
    return u

    
    


# findcycles(1,100,500,50000)

# findcycles(1,200,40,100000)
# findcycles(1,200,40,300000)
# findcycles(1,300,200,1000000)   
# findcycles(1,400,200,2000000)
# findcycles(1,500,200,2000000)

## How to estimate curve complex distance? Write code to find unicorn paths?... (but no boundary)
## or just find a good pA and iterate.
