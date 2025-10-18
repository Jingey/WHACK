import random
import csv

def getParam():
    return random.randrange(0,1000) 

def getFunc():
    return random.randrange(1,11) 


def genData(name):
    with open(name, 'w', newline="") as csvfile:
        writer = csv.writer(csvfile, delimiter=',')
        writer.writerow(["p", "q", "f", "out"])
        for i in range(0, 10000):
            f = getFunc()
            p = getParam()
            q = getParam()

            if f > 0.5:
                writer.writerow([p, q, f, p+q])
            else:
                writer.writerow([p, q, f, p-q])

