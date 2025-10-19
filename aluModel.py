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
<<<<<<< HEAD
            filesize=1_000_00,
=======
            filesize=10_000_00,
>>>>>>> 6ab1e84ce5b45a63211a154ea79fcc866ab00430
            i_size=35,
            o_size=19,
            add_func=getAluData,
        )

        arr = [
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(48, activation="relu"),
<<<<<<< HEAD
            tf.keras.layers.Dense(19, activation="relu"),
=======
            tf.keras.layers.Dense(35, activation="sigmoid"),
            tf.keras.layers.Dense(19, activation="sigmoid"),
>>>>>>> 6ab1e84ce5b45a63211a154ea79fcc866ab00430
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
<<<<<<< HEAD
    model.makeModel(800, 150_000)
=======
    model.makeModel(800, 200_000)
>>>>>>> 6ab1e84ce5b45a63211a154ea79fcc866ab00430
    model.evaluateModel("ALUTESTDATA.csv")
    model.save_model("alu_model")
