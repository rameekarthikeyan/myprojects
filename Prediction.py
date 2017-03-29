import kagglegym
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import linear_model
from sklearn.feature_selection import SelectFromModel

pd.set_option('mode.chained_assignment',None)

# The "environment" is our interface for code competitions
env = kagglegym.make()

# We get our initial observation by calling "reset"
observation = env.reset()

# Note that the first observation we get has a "train" dataframe
print("Train has {} rows".format(len(observation.train)))

nwdf = (observation.train)
gpdf = nwdf.groupby('timestamp', as_index = False)

def mdfn(x):
    x.fillna(0, inplace = True)
    sampx = x.iloc[:,2:(len(x.columns)-1)]
    sampy = x['y']
    clf = linear_model.LinearRegression()
    newc = clf.fit(sampx, sampy)
    return pd.Series(newc.coef_)
    
vr = gpdf.apply(mdfn)
avgvar = vr.median()
mnnn = np.transpose(avgvar.as_matrix())

    
#The "target" dataframe is a template for what we need to predict:
print("Target column names: {}".format(", ".join(['"{}"'.format(col) for col in list(observation.target.columns)])))


while True:
    sampxtest = (observation.features).iloc[:,2:(len((observation.features).columns))]  
    sampxtest.fillna(0, inplace = True)
    predictY = pd.DataFrame(np.dot(sampxtest.as_matrix(), mnnn))
    (observation.target)['y'] = predictY
    target = observation.target
    timestamp = observation.features["timestamp"][0]
        
    if timestamp % 100 == 0:
        print("Timestamp #{}".format(timestamp))

    # We perform a "step" by making our prediction and getting back an updated "observation":
    observation, reward, done, info = env.step(target)
    #print(reward)
    if done:
        print("Public score: {}".format(info["public_score"]))
        break

