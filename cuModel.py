import tensorflow as tf
import pandas as pd
from genCUData import getCUData
from genData import genData
from baseModel import baseModel


class CuModel(baseModel):
    def __init__(self):
        super().__init__(
            filename="CUDATA.csv",
            filesize=1000,
            i_size=21,
            o_size=41,
            add_func=getCUData
        )

        arr=[
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(48, activation="relu"),
            tf.keras.layers.Dense(35, activation="relu"),
            tf.keras.layers.Dense(41, activation="relu")
        ]

        for elem in arr:
            self.model.add(elem)
            

    def makeData(self):
        return super().makeData()
    
    def makeTestData(self, filename):
        return super().makeTestData(filename)
    
    def preProc(self, filename):
        return super().preProc(filename)
    
    def compileModel(self):
        return super().compileModel()
    
    def makeModel(self, E, B):
        return super().makeModel(E, B)
    
    def evaluateModel(self, filename):
        return super().evaluateModel(filename)
    
model = CuModel()
model.compileModel()
model.makeModel(800, 8000)
model.evaluateModel()
