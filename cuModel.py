import tensorflow as tf
import pandas as pd
from genCUData import getCUData
from genData import genData


class CuModel:
    def __init__(self):
        self.FILENAME = "ALUDATA.csv"
        self.SIZE = 1000
        self.COLNAMES = []
        self.INPUTS = []
        self.OUTPUTS = []

        for i in range(35):
            self.COLNAMES.append(f"in_{i}")
            self.INPUTS.append(f"in_{i}")

        for i in range(19):
            self.COLNAMES.append(f"out_{i}")
            self.OUTPUTS.append(f"out_{i}")

        

    def makeData():
        pass

    def makeTestData():
        pass

    def preProc():
        pass

    def compileModel():
        pass

    def makeModel():
        pass

    def evaluateModel()