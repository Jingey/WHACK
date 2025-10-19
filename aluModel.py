from genData import genData
import tensorflow as tf
import pandas as pd
from genAluData import getAluData
import numpy as np
from baseModel import baseModel


class AluModel(baseModel):
    def __init__(self):
        super().__init__(
            filename="ALUDATA.csv",
            filesize=10_000_00,
            i_size=35,
            o_size=19,
            add_func=getAluData,
        )

        arr = [
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(48, activation="relu"),
            tf.keras.layers.Dense(35, activation="sigmoid"),
            tf.keras.layers.Dense(19, activation="sigmoid"),
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


if __name__ == "__main__":
    model = AluModel()
    model.compileModel()
    model.makeModel(800, 200_000)
    model.evaluateModel("ALUTESTDATA.csv")
    model.save_model("alu_model")
