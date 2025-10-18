import random
from alu import AluEmulator

alu = AluEmulator()

def getRandomInt():
    return random.randrange(0, 1 << 34)

def to_list_bin(val, padding):
    val_str = bin(val)[2:]
    res = [int(i) for i in val_str]
    return ([0] * (padding - len(res))) + res

def getAluData():
    code = getRandomInt()
    result = alu.run(code)

    return to_list_bin(code, 35) + to_list_bin(result, 19)