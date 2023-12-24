#!/usr/bin/env python3

import csv
import itertools
from cvxopt import matrix
from cvxopt.modeling import op, variable, sum, min, dot

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

# Variables
a = variable(11)
b = variable()
z = variable()

# Value Matricies
X = matrix(x)
Y = matrix(y)

# Constraints
c1 = (Y - dot(a,X) - b <= z)
c2 = (-Y + dot(a,X) + b <= z)
c3 = (-z <= 0)

# LP problem
lp = op(min(sum(z)), [c1, c2, c3])
lp.solve()

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
        error = b.value[0]-y
        for i in range(d):
            error += x[i] * a.value[i]
        total_error += abs(error)

print("Average error per test example is %f" % (total_error/count))
