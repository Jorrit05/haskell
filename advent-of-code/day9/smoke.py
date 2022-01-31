#!/usr/bin/env python

def check_neighbours(nr : int):
  return True

with open('advent-of-code\day9\\test.txt') as f:
    lines = f.readlines()
    print(lines)

matrix = []
for line in lines:
  depth_nr_list = [int(nr) for nr in line if nr != "\n"]
  matrix.append(depth_nr_list)

# print(matrix)
line_lenght = len(matrix[0])

lowest_list = []
for lists in matrix:
  lowest = [nr for nr in lists if check_neighbours(nr)]
  lowest_list = lowest_list + lowest


print(lowest_list)
