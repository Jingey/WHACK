import random

def getRandomInt():
    return random.randrange(0, 1 << 20)

def to_list_bin(val, padding):
    pass


def getCUData():
    code = getRandomInt()
    result = 0

    return to_list_bin(code, 21) + to_list_bin(result, 38)