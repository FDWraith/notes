import csv
import itertools
from cvxopt import matrix, solvers
x = []
y = []
with open('winequality-red.csv', newline='') as input:    
    reader = csv.reader(input, delimiter=';')
    for row in itertools.islice(reader, 1, 1501):
        nums = list(map(float, row))
        x.append(nums[:-1])
        y.append(nums[-1])

# d is the dimension of a
d = len(x[0])
# n is the number of training examples
n = len(x)

# build the coefficient matrix for the LP
# note that cvxopt use column major order (see its documentation for details)
G = []
h = []
c = []

# .........
# FILL IN YOUR CODE HERE
# .........

c1 = matrix(c)
G1 = matrix(G)
h1 = matrix(h)
sol = solvers.lp(c1, G1, h1)
results = sol['x']
a = results[0:d]
b = results[d]

# test with the computed a and b
total_error = 0.0
count = 0
with open('winequality-red.csv', newline='') as input:
    reader = csv.reader(input, delimiter=';')
    for row in itertools.islice(reader, 1501, None):
        count += 1
        nums = list(map(float, row))
        x = nums[:-1]
        y = nums[-1]
        error = b-y
        for i in range(d):
            error += x[i] * a[i]
        total_error += abs(error)

print("Average error per test example is %f" % (total_error/count))
