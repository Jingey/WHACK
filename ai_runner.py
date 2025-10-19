import numpy as np
import tensorflow as tf


class AiRunner:
    def __init__(self, model_name, input_size, output_size):
        self.model = tf.keras.models.load_model(f"{model_name}.keras")
        self.input_size = input_size
        self.output_size = output_size

    def run(self, bit_str: int):
        val_str = bin(bit_str)[2:]
        res = [int(i) for i in val_str]
        input_list = ([0] * (self.input_size - len(res))) + res

        result = self.model(tf.expand_dims(np.asarray(input_list), axis=0))

        result_int = 0
        for i in result[0]:
            if i > 0.5:
                result_int |= 0b1
            result_int <<= 1

        return result_int
