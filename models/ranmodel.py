from models.randomdata import genData
import tensorflow as tf
import pandas as pd

genData("testdata.csv")
df = pd.read_csv("testdata.csv")
df.head()


train_df = df.sample(frac=0.75)
validation_df = df.drop(train_df.index)
train_df.drop(train_df.columns[0], axis='columns');
validation_df.drop(validation_df.columns[0], axis='columns');


maxVal = train_df.max(axis=0) #max alon columns
minVal = train_df.min(axis=0)
rng = maxVal - minVal
print("min",minVal, "\n")
print("max", maxVal, "\n")
print("rng",rng,"\n")


train_df = (train_df - minVal)/rng
validation_df = (validation_df - minVal)/rng

train_df.to_csv("traindb.csv");


inp_train = train_df.drop('out', axis=1) # drop output per row
inp_valid = validation_df.drop('out', axis=1)

out_train = train_df['out']
out_valid = validation_df['out']

# shape is the number of rows and columns of the dataframe
inp_shape = [inp_train.shape[1]] # how many inputs to first layer of neural network

# Make model - sequential

model = tf.keras.Sequential([
    tf.keras.layers.Input(shape=inp_shape),
    tf.keras.layers.Dense(units=1)
])

print(model.summary())

model.compile(
    optimizer=tf.keras.optimizers.RMSprop(),  # Optimizer
    # Loss function to minimize
    loss=tf.keras.losses.MeanSquaredError(),
    # List of metrics to monitor
    metrics=['accuracy'],
)



losses = model.fit(
    inp_train, 
    out_train,
    validation_data=(inp_valid, out_valid),
    epochs = 100
    )

print(losses.history)

print("Evaluate")
genData("modelTest.csv")
df2 = pd.read_csv("modelTest.csv")
df2.head()

mi = df2.min(axis=0)
ma = df2.max(axis=0)
r = ma-mi

df2 = (df2 - mi)/r

testinputs = df2.drop('out', axis=1)
testOutputs = df2['out']

loss, accuracy = model.evaluate(testinputs, testOutputs)
print("\n Test loss", loss)
print("\n Test accuracy", accuracy)