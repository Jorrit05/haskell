# Return double of n
def addition(n):
    return n + n

# We double all numbers using map()
numbers = (1, 2, 3, 4)
result = map(addition, numbers)
print(list(result))

class myObject:
  def __init__(self, num = 0):
    self.var = num

myobj = myObject()
myobj2 = myObject(1)
print(myobj.var)
print(myobj2.var)
print(myobj == myobj)

