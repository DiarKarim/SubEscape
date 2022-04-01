import numpy as np
import numpy.linalg
import matplotlib.pyplot as plt
import pandas as pd
import os
import random
from scipy.optimize import curve_fit
from scipy.stats import norm
from scipy.ndimage import interpolation
from statsmodels.stats.anova import AnovaRM
import time
import sys
import math
import pylab as py
from scipy.signal import savgol_filter
from scipy.signal import find_peaks
import scipy.stats as sci 
import seaborn as sns
import statannot
import json
from scipy import signal

import multiprocessing as mp

# sns.set_style(style='whitegrid')
def main():
    pool = mp.Pool(mp.cpu_count())
    result = pool.map(my_func, [4,2,3])
    return result

def main2(inputList):
    pool = mp.Pool(mp.cpu_count())
    result = pool.map(my_func, inputList)
    return result

def main3(inputList):
    pool = mp.Pool(mp.cpu_count())
#     result = pool.map(ResizeArray, inputList)
    result = pool.map(my_func, inputList)
    return result








def main4(inputList):
    pool = mp.Pool(mp.cpu_count())
    result = pool.map(LoadFiles2DataFrame, inputList)
    return result


def main5(inputList):
    pool = mp.Pool(mp.cpu_count())
    result = pool.map(ComputeMetrics, inputList)
    return result





def my_func(x):
    return (x**x)

def GetAng(a,b,c):
    ang = math.degrees(math.atan2(c[1] - b[1], c[0] - b[0]) - math.atan2(a[1] - b[1], a[0] - b[0]))
    return 360 - (ang + 360 if ang < 0 else ang)

def ResizeArray(data, newSize):
    x = data
    i = newSize

    lenX = len(x)

    if lenX == 0:
        lenX = 1
        z = i / lenX
    else:
        z = i / lenX
        x_int = interpolation.zoom(x,z)

    return x_int

def ReadFiles(path, files):
    # Define data frame variable
    df = None 
    startTime = time.time()
  
    print('Number of files: ', len(files))
    
    # Load each file into the data frame 
    for i in range(len(files)): 
    
        if ".json" in files[i] and "Phase" in files[i] and "QNumber" not in files[i]:    
        # if "txt.json" in files[i] and "Phase" in files[i]:
              # print(files[i])

            # elapsedTime = time.time() - startTime
            # print('Time passed: ', np.round(elapsedTime), '\t sec \r')

            # Extract file name info and add to the dataframe 
            fileWords = files[i].split("_")

            # Extract phase 
            idx = fileWords.index("Phase")
            phase = fileWords[idx + 1]

            # Extract user ID 
            idx = fileWords.index("trial")
            userID = fileWords[idx - 1]

            # Add trial number to data frame 
            try:
#                 print('Path and File: ', [path + '/' + files[i]])
                tmpDF = pd.read_json(path + '/' + files[i])
                tmpDF.insert(0, "Phase", phase, True)
                tmpDF.insert(0, "UserID", userID, True)

                if df is None:
                    df = tmpDF
                else:
                    df = pd.concat((df, tmpDF))
            except Exception as e:
                print('My_Err: ', e)
    return df

def translate(value, leftMin, leftMax, rightMin, rightMax):
    # Figure out how 'wide' each range is
    leftSpan = leftMax - leftMin
    rightSpan = rightMax - rightMin

    # Convert the left range into a 0-1 range (float)
    valueScaled = float(value - leftMin) / float(leftSpan)

    # Convert the 0-1 range into a value in the right range.
    return rightMin + (valueScaled * rightSpan)

def unique(list1):
 
    # initialize a null list
    unique_list = []
     
    # traverse for all elements
    for x in list1:
        # check if exists in unique_list or not
        if x not in unique_list:
            unique_list.append(x)

#     print list
#     for x in unique_list:
#         print (x)

    return unique_list

def Extract_DialAngle(dataFrame):
    
    tmpDF = dataFrame #pd.read_json(path + f, orient='index')

    # Extract pose data from each of the metrics 
    dial = []
    target = []
    score = []

    # tmpDF.loc['dialData'].values[0][i]
    for i in tmpDF.loc['dialData'].values[0]: 
        metricParts = str.split(i,';')
        dial.append(float(metricParts[0]))
        target.append(float(metricParts[1]))
        score.append(float(metricParts[2]))

    data = {'Dial_Raw' : dial,
           'Target' : target,
           'Score' : score,}

    tmpDF2 = pd.DataFrame(data)
#     tmpDF2.insert(0,'PtxID',ptxIDs)
#     tmpDF2.insert(1,'trial',trial)
#     tmpDF2.insert(2,'group',group)
#     tmpDF2.insert(3,'hand',hand)
#     tmpDF2.insert(4,'age',age)
#     tmpDF2.insert(5,'sex',sex)

    return tmpDF2

def Extract_FingerData(dataFrame, fingerType):
    
    tmpDF = dataFrame
    frameColumns = []
    
    if 'virtual' in fingerType:
        fingerType = 'vfingData' 
        frameColumns = ['VFrame','VJoint','VxPos','VyPos','VzPos','Vrot']
    else:
        fingerType = 'fingData'
        frameColumns = ['Frame','Joint','xPos','yPos','zPos','rot']
    
    # Extract finger data ----------------------------------------
    frame = []
    joint = []
    xPos = []
    yPos = []
    zPos = []
    rot = [] 
    for i in np.arange(len(tmpDF.loc[fingerType].values[0])):
        metricParts = str.split(tmpDF.loc[fingerType].values[0][i],';')
        frame.append(int(metricParts[0]))
        joint.append(metricParts[1])
        xPos.append(float(metricParts[2]))
        yPos.append(float(metricParts[3]))
        zPos.append(float(metricParts[4]))
        rot.append(float(metricParts[5]))

    data2 = {frameColumns[0] : frame,
           frameColumns[1] : joint,
           frameColumns[2] : xPos,
           frameColumns[3] : yPos,
           frameColumns[4] : zPos,
           frameColumns[5] : rot,}

    tmpDF3 = pd.DataFrame(data2)
    
    return tmpDF3

def DialAngleRecon(df_all, fingerType):
    
    jointsOfInterest = ['b_l_thumb1', 'b_l_thumb2', 'b_l_thumb3',
                    'b_l_index1', 'b_l_index2', 'b_l_index3',
                    'b_l_middle1', 'b_l_middle2', 'b_l_middle3',
                    'b_l_ring1', 'b_l_ring2', 'b_l_ring3',
                    'b_l_pinky1', 'b_l_pinky2', 'b_l_pinky3',
                    'b_l_wrist']

    childJoints = jointsOfInterest[:-1]
    graphData = False

    invX = 0
    invY = 0
    invZ = 0

    # Reconstruct Original Dial Angles from relative finger joints and wrist positions 
    for j in childJoints:
        
        if 'virtual' in fingerType:
            mask = (df_all['VJoint'] == j)
            maskWrist = (df_all['VJoint'] == jointsOfInterest[-1]) 

            invX +=  np.sqrt((df_all[mask]['VxPos'].values - df_all[maskWrist]['VxPos'].values) ** 2)
            invY +=  np.sqrt((df_all[mask]['VyPos'].values - df_all[maskWrist]['VyPos'].values) ** 2)
            invZ +=  np.sqrt((df_all[mask]['VzPos'].values - df_all[maskWrist]['VzPos'].values) ** 2)
        else:
            mask = (df_all['Joint'] == j)
            maskWrist = (df_all['Joint'] == jointsOfInterest[-1]) 

            invX +=  np.sqrt((df_all[mask]['xPos'].values - df_all[maskWrist]['xPos'].values) ** 2)
            invY +=  np.sqrt((df_all[mask]['yPos'].values - df_all[maskWrist]['yPos'].values) ** 2)
            invZ +=  np.sqrt((df_all[mask]['zPos'].values - df_all[maskWrist]['zPos'].values) ** 2)

            
    cA_Temp = np.nansum([invX * 0.067, (invY * 0.067)], axis=0)
    currentAngle = np.nansum([cA_Temp, (invZ * 0.067)], axis=0)

    leftMin = np.nanmin(currentAngle) # leftMin = 0.045
    leftMax = np.nanmax(currentAngle) # leftMax = 0.115
    dialAngleRecon = []

    for cA in currentAngle:
        dialAngleRecon.append(translate(cA, leftMin, leftMax, 0.1, 179.9))

    dialAngleRecon = [i + 7.5 for i in dialAngleRecon] # Add offset value of 7.5 degrees to the reconstructed dial angle to match the original Unity environment and script (see MoveDial.cs)

    return dialAngleRecon

# Original Unity app code for reference ----------------------------------------------------
# Vector3 currentHandData = new Vector3();--------------------------------------------------
# for (int i = 0; i < virtualDigits.Length - 1; i++)----------------------------------------
# {-----------------------------------------------------------------------------------------
#-----currentHandData += virtualDigits[16].InverseTransformPoint(virtualDigits[i].position);
# }{----------------------------------------------------------------------------------------
# float currentAngle = (currentHandData.x * 0.067f) +---------------------------------------
#----------------(currentHandData.y * 0.067f) +---------------------------------------------
#----------------(currentHandData.z * 0.067f);----------------------------------------------
# currentAngle = Mathf.Abs(currentAngle);---------------------------------------------------


from numpy import sin
from numpy import sqrt
from numpy import arange
from pandas import read_csv
from scipy.optimize import curve_fit
from matplotlib import pyplot

# Fit a line to the raw dial angle data

# define the true objective function
def objective1(x, a, b, c, d): # sine combined with 2nd order polynomial 
	return a * sin(b - x) + c * x**2 + d

def objective2(x, a, b, c, d, e, f): # 5th order polynomial 
	return (a * x) + (b * x**2) + (c * x**3) + (d * x**4) + (e * x**5) + f

def objective3(x, a, p, v, f): # Sine function 
    # x = input data 
    # a = amplitude
    # f = frequency aka period (2*pi/f)
    # p = phase shift
    # v = vertical shift 
    return a * sin((f * x) + p) + v

def objective4(x, m, b): # Linear function 
    return (m * x) + b

def FitCurve(y, target, func = 1):
    
    # the dataset
    N = len(y)
    x = np.linspace(0, 4*np.pi, N)
    
    # define a sequence of inputs between the smallest and largest known inputs
    x_line = arange(min(x), max(x), 1)
    
    # summarize the parameter values
    if func == 1:
        popt, _ = curve_fit(objective1, x, y) # curve fit
        a, b, c, d = popt # objective1  
        y_line = objective1(x_line, a, b, c, d) # calculate the output for the range
    elif func == 2:
        popt, _ = curve_fit(objective2, x, y) # curve fit
        a, b, c, d, e, f = popt # objective2
        y_line = objective2(x_line, a, b, c, d, e, f) # calculate the output for the range
    elif func == 3:
        popt, _ = curve_fit(objective3, x, y) # curve fit
        a, b, c, f = popt # objective3
        y_line = objective3(x_line, a, b, c, f) # calculate the output for the range
    elif func == 4:
        popt, _ = curve_fit(objective4, x, y) # curve fit
        a, b = popt # objective3
        y_line = objective4(x_line, a, b) # calculate the output for the range
        
    print(popt)

    # create a line plot for the mapping function
    # plot input vs output
    pyplot.scatter(x, target, color='k',linewidth=5)
    pyplot.plot(x, y,'r--',linewidth=3)
    pyplot.plot(x_line, y_line, '-', color='green', linewidth=2)
    pyplot.show()
    
def FitLine(y, target, func = 1, showPlot = False):
    
    # the dataset
    # N = len(y)
    x = target #np.linspace(0, 4*np.pi, N)
    
    # define a sequence of inputs between the smallest and largest known inputs
    x_line = arange(min(x), max(x), 1)
    
    # summarize the parameter values
    popt, _ = curve_fit(objective4, x, y) # curve fit
    a, b = popt # objective3
    y_line = objective4(x_line, a, b) # calculate the output for the range

    # create a line plot for the mapping function
    # plot input vs output
    if showPlot:
        pyplot.plot(target, y, 'ko', markersize=0.5)
        pyplot.plot(x_line, y_line, 'r--', linewidth=0.75)
    
    return a, b



#*********************************************************************************************
#***************************** Load file to dataframe ****************************************
#*********************************************************************************************



def LoadFiles2DataFrame(path):

    fileLoaded = False
    startTime = time.time()
    if not fileLoaded:

        folders = os.listdir(path)
        folderNames = unique(folders)
        df_all = None

        for f in folderNames:            
            folderWords = str.split(f,'_')
            if 'json' not in folderWords[-1]:
                continue
            else:
                ptxID = folderWords[1]
                age = folderWords[2]
                sex = folderWords[3]
                hand = folderWords[4]
                group = folderWords[5]
                trial = int(folderWords[9])

                # Read raw data file 
                try:
                    tmpDF1 = pd.read_json(path + f, orient='index')

    # ******************************************************************************
    # ******************* Extract data section **************************************

                    # Extract real finger data --------------------------------- 
                    tmpDF2 = Extract_FingerData(tmpDF1, 'real')

                    # Extract virtual finger data ------------------------------
                    tmpDF3 = Extract_FingerData(tmpDF1, 'virtual')

                    # Extract dial angle data ----------------------------------                    
                    tmpDial = Extract_DialAngle(tmpDF1) 

                    # Dial Angle Reconstruction -------------------------------- 
                    # Use tmpDF2 for real and tmpDF3 for virtual finger 
                    dialAngReconstructed = DialAngleRecon(tmpDF3, 'virtual') # Virtual finger      

                    # Combine raw and reconstructed dial angle data into one dataframe 
                    tmpDial.insert(1,'Dial_Recon', dialAngReconstructed)

                    # Combine all data frames ---------------------------------
                    tmpDF2 = pd.concat((tmpDF3, tmpDF2))
                    tmpDF2 = pd.concat((tmpDial, tmpDF2))
                    tmpDF2.insert(0,'PtxID',ptxID)
                    tmpDF2.insert(1,'trial',trial)
                    tmpDF2.insert(2,'group',group)
                    tmpDF2.insert(3,'hand',hand)
                    tmpDF2.insert(4,'age',age)
                    tmpDF2.insert(5,'sex',sex)

    # ******************************************************************************
    # ******************************************************************************

                    # Concatenate all resultant data frames into one parent data frame 
                    if df_all is None:
                        df_all = tmpDF2
                    else:
                        df_all = pd.concat([df_all, tmpDF2])          

                except Exception as e:
                    print('Ptx: ', ptxID,' Warning: ', e)
    #     df

    elapsedTime = np.round((time.time() - startTime) / 60.0, 0)
    print('Processing time: ', elapsedTime, ' minutes')
    
    return df_all



#*********************************************************************************************
#***************************** Create metrics dataframe **************************************
#*********************************************************************************************

def ComputeMetrics(df_all):
    # For each participant show all the trials, with target, raw and reconstructed dial angles
    # fig, axs = plt.subplots(8,7)

    from sklearn.metrics import mean_squared_error

    showPlot = True
    
    plt.rcParams["figure.figsize"] = (10,3)
    plt.figure()

    participants = pd.unique(df_all['PtxID'])
    trials = pd.unique(df_all['trial'])
    trials = np.sort(trials)

    df_metrics = None 

    for ptx, pt in enumerate(participants):
        print('Participant: ', ptx, ' / ', len(participants))

        slopes = []
        intercepts = []
        MSE = []
        trialz = []

        for tr in trials:

            # Extract group automatically 
            gmask = (df_all['PtxID'] == pt)
            grp = df_all[gmask]['group']
            group = grp.values[0]
            trialz.append(tr)
            
            # Data collection part 
            mask = (df_all['PtxID'] == pt) &  (df_all['trial'] == tr) & (df_all['group'] == group) 
            dial = df_all[mask]['Dial_Raw'].values
            dialReconRaw = df_all[mask]['Dial_Recon'].values
            targetRaw = df_all[mask]['Target'].values
            
            # Check data and clean up i.e. remove NaNs etc. 
            if not dialReconRaw.tolist(): 
                # print('Warning: --> Empty array')
                slopes.append(np.nan)
                intercepts.append(np.nan)
                MSE.append(np.nan)
            else:
                dialRecon = dialReconRaw[~numpy.isnan(dialReconRaw)]
                target = targetRaw[~numpy.isnan(targetRaw)]

                # Detrend data by subtracting 90 degrees, i.e. the bias in the data? <--- check why exactly this is ???? 
                dial2_detrend = dialRecon - 90
                target_detrend = target - 90

                # MSE computation 
                mze = mean_squared_error(target, dialRecon)
                MSE.append(mze)

                plt.subplot(8, 7,tr+1)
                # Plot regression line through data 
                slope, intercept = FitLine(dial2_detrend, target_detrend, 4, showPlot)
                slopes.append(slope)
                intercepts.append(intercept)

                if showPlot:
                    txt = plt.text(-60, 60, 'MSE:        '  + str(np.round(mze,1)) + '\nSlope:      ' + str(np.round(slope,2)) + '\nIntercept: ' + str(np.round(intercept,1)), fontsize=3, color="Blue")
                    txt.set_bbox(dict(facecolor='white', alpha=0.5, edgecolor='red'))
                    plt.xticks(fontsize = 3)
                    plt.yticks(fontsize = 3)

                    # Show y label on first plot
                    if tr == 0:
                        plt.ylabel('Reconstructed Dial', fontsize = 10)
                        plt.legend(['Data','Fitted'], loc='lower right')
                    # Show x label on middle plot
                    if tr == 2:
                        plt.xlabel('Target', fontsize = 10)

                    # plt.tight_layout()

        # Data storage part 
        dat_metrics = {'Trial' : trialz,
                       'Slope' : slopes,
                       'Intercept' : intercepts,
                       'MeanSqErr' : MSE,}

        tmpDf = pd.DataFrame(dat_metrics)
        tmpDf.insert(0, 'PtxID', pt)
        tmpDf.insert(1, 'Group', group)

        if df_metrics is None:
            df_metrics = tmpDf
        else:
            df_metrics = pd.concat((df_metrics, tmpDf))

        plt.savefig('ptx_' + str(ptx) + '_' + str(np.round(time.time())) + "Dial_Reconstruction.png", dpi = 1200)
        plt.show()

    return df_metrics