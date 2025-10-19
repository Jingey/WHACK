import random
from cu_validator import make_valid_bit_string
from cu import CuEmulator

cu = CuEmulator()

def getRandomInt():
    return random.randrange(0, 1 << 20)

def to_list_bin(val, padding):
    val_str = bin(val)[2:]
    res = [int(i) for i in val_str]
    return ([0] * (padding - len(res))) + res

def getCUData():
    code = make_valid_bit_string(getRandomInt())
    result = cu.run(code)

    return to_list_bin(code, 21) + to_list_bin(result, 41)