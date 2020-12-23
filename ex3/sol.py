import numpy as np
import sys

#Read the input
A = 30
A = int(A/10)
B = 10
B = int(B/10)
tuples =  [(5,50),(10,30),(0,0)]

race_len = 0
max_vel  = 0
for (x,y) in tuples:
    race_len += x
    max_vel = max(max_vel, y)
max_vel = int(max_vel/10)
arr = np.full((race_len+1, max_vel), -1)

for i in range(1,A+1):
    arr[i,i] = 1

for i in range(race_len+1):
    if tuples[0] == [0,0]:
        break
    (x,y) = tuples[0]
    if x==0:
        tuples.pop(0)
    else:
        tuples[0] = (x-1,y)
    curr_speed_limit = y
    for j in range(max_vel):
        if i-j < 0 or j > curr_speed_limit:
            continue 
        for k in range(j-A, j+B+1):
            if k < 0:
                arr[i,j] = 1
            if k >= max_vel:
                continue
            if arr[i-int(j/10), k] == 1000:
                continue
            arr[i, j] == max(arr[i-j, k] + 1, arr[i,j])

print(arr)