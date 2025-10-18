import random
import pandas as pd

def getParam():
    return random.randrange(0,1000)

def getFunc():
    return random.randrange(1,11)


def genData(name):
    data = {}

    for i in range(0, 10000):
        f = getFunc()
        p = getParam()
        q = getParam()

        if f >= 1 and f < 5:
            data.update({i: {'p': p, 'q': q, 'f': f, 'out': p+q}})
        else:
            data.update({i: {'p': p, 'q': q, 'f': f, 'out':p-q}})

    df = pd.DataFrame(data = data).T
    df.to_csv(name)