import tensorflow as tf
import pandas as pd
from genCUData import getCUData
from genData import genData


class baseModel:
    def __init__(self, filename, filesize, i_size, o_size, add_func):
        self.FILENAME = filename
        self.SIZE = filesize
        self.COLNAMES = []
        self.INPUTS = []
        self.OUTPUTS = []
        self.addFunc = add_func

        for i in range(i_size):
            self.COLNAMES.append(f"in_{i}")
            self.INPUTS.append(f"in_{i}")

        for i in range(o_size):
            self.COLNAMES.append(f"out_{i}")
            self.OUTPUTS.append(f"out_{i}")

        self.makeData()
        self.train_df, self.validate_df = self.preProc(self.FILENAME)

        self.train_in = self.train_df[self.INPUTS]
        self.train_out = self.train_df[self.OUTPUTS]

        self.valid_in = self.validate_df[self.INPUTS]
        self.valid_out = self.validate_df[self.OUTPUTS]

        self.model = tf.keras.Sequential()
        self.model.add(
            tf.keras.layers.Input(shape=(self.train_in.shape[1],)),
        )

    def makeData(self):
        genData(self.FILENAME, self.SIZE, self.COLNAMES, self.addFunc)

    def makeTestData(self, filename):
        genData(filename, self.SIZE, self.COLNAMES, self.addFunc)

    def preProc(self, filename):
        df = pd.read_csv(filename, index_col=False)
        train_df = df.sample(frac=0.75, random_state=42)
        validation_df = df.drop(train_df.index)

        return train_df, validation_df

    def compileModel(self):
        self.model.compile(
            optimizer=tf.keras.optimizers.RMSprop(),
            loss=tf.keras.losses.MeanSquaredError(),  # Because gives calculate in range [0,1)
            metrics=[
                tf.keras.metrics.BinaryAccuracy(
                    name="binary_accuracy", dtype=None, threshold=0.5
                )
            ],
        )

    def makeModel(self, E, B):
        losses = self.model.fit(
            self.train_in,
            self.train_out,
            validation_data=(self.valid_in, self.valid_out),
            epochs=E,
            batch_size=B,
        )

        print(losses.history)

    def evaluateModel(self, filename):
        self.makeTestData(filename)
        self.testUnits, self.testValidUnits = self.preProc(filename)

        self.ti = self.testUnits[self.INPUTS]
        self.to = self.testUnits[self.OUTPUTS]

        loss, accuracy = self.model.evaluate(self.ti, self.to)
        print(
            f"Losses Mean Squared Error: {loss}, Accuracy Mean Absolute Error: {accuracy}"
        )
        print(self.model(self.ti.head(1)))

    def save_model(self, file_name):
        self.model.save(f"{file_name}.keras")
