# -*- coding: utf-8 -*-
"""
Created on Wed Feb 24 18:11:00 2021

@author: suhaib
"""

# Graduation Project 
import Metrica_IO as mio
import Metrica_Viz as mviz
import Metrica_Velocities as mvel
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mplsoccer.pitch import Pitch
import statsmodels.api as sm
import tensorflow as tf
from tensorflow.keras import Sequential
from tensorflow.keras.layers import Dense
from tensorflow.keras.layers import LSTM
from tensorflow.keras.layers import Dropout
from tensorflow.keras.layers import BatchNormalization
plt.style.use('ggplot')



pitch = Pitch(pitch_color='#aabb97', line_color='white',
              stripe_color='#c2d59d', stripe=True)  # optional stripes
fig, ax = pitch.draw()

# Reading data & cleaning data
# set up initial path to data
DATADIR = 'C:/Users/suhai/Desktop/All Folders/R Project/FootballAnalyticsCourse/LaurieOnTracking-master/tracking data'
game_id = 2 # let's look at sample match 2

# read in the event data
events = mio.read_event_data(DATADIR,game_id)

# read in tracking data
tracking_home = mio.tracking_data(DATADIR,game_id,'Home')
tracking_away = mio.tracking_data(DATADIR,game_id,'Away')

# reverse direction of play in the second half so that home team is always attacking from right->left
tracking_home,tracking_away,events = mio.to_single_playing_direction(tracking_home,tracking_away,events)

# Convert positions from metrica units to meters (note change in Metrica's coordinate system since the last lesson)
tracking_home = mio.to_metric_coordinates(tracking_home)
tracking_away = mio.to_metric_coordinates(tracking_away)
events = mio.to_metric_coordinates(events)

# remove subistitus 
ball = tracking_away[['ball_x', 'ball_y']]
tracking_home = tracking_home.iloc[:,0:24]
tracking_away = tracking_away.iloc[:,0:24]

# Calculate player velocities
tracking_home = mvel.calc_player_velocities(tracking_home,smoothing=True,filter_='moving_average')
tracking_away = mvel.calc_player_velocities(tracking_away,smoothing=True,filter_='moving_average')

# remove time and period 
tracking_home = tracking_home.drop(['Period','Time [s]'], axis = 1)
tracking_away = tracking_away.drop(['Period','Time [s]'], axis = 1)

data = pd.concat([tracking_home, tracking_away, ball], axis = 1)
data = data.dropna()



corr = data.corr()
import seaborn as sns
f, ax = plt.subplots(figsize=(10, 8))

sns.heatmap(corr, mask=np.zeros_like(corr, dtype=np.bool), cmap=sns.diverging_palette(220, 10, as_cmap=True),
            square=True, ax=ax)

# split test and train sets
data_train = data.loc[0:(data.shape[0] * 0.75),:]
data_test = data.loc[(data.shape[0] * 0.75)+1:data.shape[0],:]

# Linear model
def lm (data, p, team):
    
    vx = data.loc[:,(team + "_" + p + "_vx")]
    vy = data.loc[:,(team + "_" + p + "_vy")]
    
    y = data.drop(columns = [(team + "_" + p + "_x") , (team + "_" + p + "_y"), (team + "_" + p + "_vx"), (team + "_" + p + "_vy")])
    y = sm.add_constant(y)
    
    modelx = sm.OLS(vx,y).fit()
    modely = sm.OLS(vy,y).fit()

    return modelx, modely

#build home players model
home = []
for i in range(1,12):
     home.append(lm(data_train, str(i), 'Home'))

#build away players model
away = []
for i in range(15,26):
     away.append(lm(data_train, str(i), 'Away'))

# printing out R squared
for i in range(0,11): 
    print(home[i][0].rsquared_adj)
    print(home[i][1].rsquared_adj)

for i in range(0,11): 
    print(away[i][0].rsquared_adj)
    print(away[i][1].rsquared_adj)


'''
team = 'Home'
p = '1'
y = data_test.drop(columns = [(team + "_" + p + "_x") , (team + "_" + p + "_y"), (team + "_" + p + "_vx"), (team + "_" + p + "_vy")])
y = sm.add_constant(y)
pred = home[0][0].predict(y)
((data_test['Home_1_vx'] - pred).abs()).mean()
'''


# Deep neural network

# split data into input and output
team = 'Home'
p = '5'
X_train = data_train.drop(columns = [(team + "_" + p + "_vx"), (team + "_" + p + "_vy")])
X_test = data_test.drop(columns = [(team + "_" + p + "_vx"), (team + "_" + p + "_vy")])

Y_train = data_train.loc[:,[(team + "_" + p + "_vx"), (team + "_" + p + "_vy")]]
Y_test = data_test.loc[:,[(team + "_" + p + "_vx"), (team + "_" + p + "_vy")]]

# determine the number of input features
n_features = X_train.shape[1]

# define model
model = Sequential()
model.add(Dense(120, activation='relu', kernel_initializer='he_normal', input_shape=(n_features,)))
model.add(Dense(80, activation='relu', kernel_initializer='he_normal'))
model.add(Dense(60, activation='relu', kernel_initializer='he_normal'))
model.add(Dense(30, activation='relu', kernel_initializer='he_normal'))
model.add(Dense(10, activation='relu', kernel_initializer='he_normal'))
model.add(Dense(2))

# compile the model
model.compile(optimizer='adam', loss='mse', metrics=['accuracy'])

# fit the mode
model.fit(X_train, Y_train, epochs=150, batch_size=32, verbose=0)

# evaluate the model
loss, acc = model.evaluate(X_test, Y_test, verbose=0)
print('Test Accuracy: %.3f' % acc)



# Recurrent Neural Network

def RNN_trans (data, n, team, p):
    
    x = data.drop(columns = [(team + "_" + p + "_vx"), (team + "_" + p + "_vy")])
    y = data.loc[:,[(team + "_" + p + "_vx"), (team + "_" + p + "_vy")]]
    
    ncol = x.shape[1]
    cols, names = list(), list()
	# input sequence (t-n, ... t-1)
    for i in range(n, 0, -1):
        cols.append(x.shift(i))
        names += [('var%d(t-%d)' % (j+1, i)) for j in range(ncol)]
        
        
    x = pd.concat(cols, axis=1)
    x.columns = names
    
	# drop rows with NaN values
    x.dropna(inplace=True)  	
    y = y.shift(n)
    y.dropna(inplace=True)
    return x,y
   
x2,y2 = RNN_trans(data, 25, 'Home', '5')
x_train = x2.loc[0:(x2.shape[0] * 0.75),:]
x_test = x2.loc[(x2.shape[0] * 0.75)+1:x2.shape[0],:]

y_train = x2.loc[0:(y2.shape[0] * 0.75),:]
y_test = x2.loc[(y2.shape[0] * 0.75)+1:y2.shape[0],:]



sec = 25
n_features = 88
d = series_to_supervised(data, sec, 1)


# split into input and outputs
n_obs = sec * n_features


# reshape input to be 3D [samples, timesteps, features]
x_train, x_test = x_train.values, x_train.values
x_test = x_test.values
x_train = x_train.reshape((x_train.shape[0], sec, n_features))
x_test = x_test.reshape((x_test.shape[0], sec, n_features))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)
 
# design network
model = Sequential()
model.add(LSTM(100, input_shape=(x_train.shape[1], x_train.shape[2])))
model.add(Dense(16))
model.add(Dense(2))
model.compile(loss='mae', optimizer='adam')
# fit network
history = model.fit(x_train, y_train, epochs=30, batch_size=72,validation_data=(x_test, y_test), verbose=2, shuffle=False)
loss, acc = model.evaluate(x2, y2, verbose=0)
# plot history
plt.plot(history.history['loss'], label='train')
plt.plot(history.history['val_loss'], label='test')
plt.legend()
plt.show()
 
df = pd.DataFrame()
df['t'] = [x for x in range(10)]

df['t-1'] = df.shift(1)
print(df)


