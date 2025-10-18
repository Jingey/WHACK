from genData import genData
import tensorflow as tf
import pandas as pd
from genAluData import getAluData
import numpy as np


class AluModel:
    def __init__(self):
        self.FILENAME = "ALUDATA.csv"
        self.SIZE = 100_000
        self.COLNAMES = []
        self.INPUTS = []
        self.OUTPUTS = []

        for i in range(35):
            self.COLNAMES.append(f"in_{i}")
            self.INPUTS.append(f"in_{i}")

        for i in range(19):
                self.COLNAMES.append(f"out_{i}")
                self.OUTPUTS.append(f"out_{i}")




        # Define model

        self.makeData()
        self.train_df, self.validate_df = self.preProc(self.FILENAME)

        self.train_in = self.train_df[self.INPUTS]
        self.valid_in = self.validate_df[self.INPUTS]

        self.train_out = self.train_df[self.OUTPUTS]
        self.valid_out = self.validate_df[self.OUTPUTS]

        self.model = tf.keras.Sequential([
            tf.keras.layers.Input(shape=(self.train_in.shape[1],)),
            tf.keras.layers.Dense(64, activation="relu"),
            tf.keras.layers.Dense(48, activation="relu"),
            tf.keras.layers.Dense(35, activation="relu"),
    #    ] + [tf.keras.layers.Dense(35, activation="relu") for i in range(9)] +[
            tf.keras.layers.Dense(19, activation="relu"),

        ])


        print(self.model.summary())

    def makeData(self):
        genData(self.FILENAME, self.SIZE, self.COLNAMES, getAluData)

    def makeTestData(self):
        genData("aluTestData.csv", self.SIZE, self.COLNAMES, getAluData)


    def preProc(self, filename):
        df = pd.read_csv(filename, index_col=False)
        train_df = df.sample(frac=0.75)
        validation_df = df.drop(train_df.index)
        return train_df, validation_df

    def compileModel(self):
        self.model.compile(
            optimizer = tf.keras.optimizers.RMSprop(),
            loss = tf.keras.losses.MeanSquaredError(), # Because gives calculate in range [0,1)
            metrics  = [tf.keras.metrics.BinaryAccuracy(name="binary_accuracy", dtype=None, threshold=0.5)]
            )
    
    def makeModel(self, E, B):
        losses = self.model.fit(
            self.train_in,
            self.train_out,
            validation_data = (self.valid_in, self.valid_out),
            epochs = E,
            batch_size = B,
        )

        print(losses.history)

    def evaluateModel(self):
        self.makeTestData()
        self.testUnits, self.testValidUnits = self.preProc("aluTestData.csv")

        self.testUnits.to_csv("ALUTESTMODELDATA.csv")

        self.ti = self.testUnits[self.INPUTS]
        self.to = self.testUnits[self.OUTPUTS]

        loss, accuracy = self.model.evaluate(self.ti, self.to)
        print(f"Losses Mean Squared Error: {loss}, Accuracy Mean Absolute Error: {accuracy}")
        print(self.model(self.ti.head(1)))



model = AluModel()
model.compileModel()
model.makeModel(800, 8000)
model.evaluateModel()
