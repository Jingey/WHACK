import random
from alu import AluEmulator

alu = AluEmulator()

def getRandomInt():
    return random.randrange(0, 1 << 34)

def getAluData():
    alu.run(getRandomInt());