"""
Script to fit the confidence weighted psychometric functions
"""

import os
from os import listdir
import pandas as pd
import seaborn as sns
import itertools
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def logistic_func(x, k, x_0, L=1):
    return L / (1 + np.exp(-k * (x - x_0)))


DIR=os.getcwd()

# Set the directory to  the summarized results files (output from data wrangling script )
data_dir_sum= DIR+"/analysis/results/experiment/summarized_data"
# Get all files in that directory
files_sum = [file for file in listdir(data_dir_sum) if file.endswith('.csv')]  # only get the results csv files
files_sum = [k for k in files_sum if 'P1_1_former' not in k]
files_sum = [k for k in files_sum if 'P1_2_current' not in k]

#Set the directory to where the raw results files are
data_dir_raw=DIR +"/analysis/data_experiment"

# Iterate over files (since one file=participant, basically iterating over participants)
results=[]
for file_sum in files_sum:
        # Read in summarized data as pandas dataframe
        df_sum = pd.read_csv(data_dir_sum + '/' + file_sum)
        #Get participant name and LSHS score
        participant=df_sum["subj"][1]
        print(participant)
        #Get the unsummarized data file
        sub_dir = data_dir_raw +'/'+ file_sum.split('_')[0] + '/ME1'  # get the directory for one participant
        files_raw = [file for file in listdir(sub_dir) if file.endswith('.csv')]  # only get the results csv files
        files_raw = [k for k in files_raw if 'practice' not in k]  # remove the results from the practice block

        participant_results = []
        for file_raw in files_raw:
            df = pd.read_csv(sub_dir + '/' + file_raw)
            participant_results.append(df)
        df_raw = pd.concat(participant_results)
        df_raw = df_raw.dropna(subset=['AFC', 'response_time', 'button_press_length'])

        results_list=[]

        #Using min max normalisation
        df_raw['min_max_normalised'] = df_raw['button_press_length'].transform(lambda x: (x - x.min()) / (x.max() - x.min()))

        # Get the averaged confidence measure per morph ratio
        confidence_results= []
        morph_ratios=[0.0, 0.2, 0.35, 0.5, 0.65, 0.8, 1.0]
        plt.figure(figsize=(10, 5))
        for morph_ratio in morph_ratios:
            df= df_raw[df_raw["morph_ratio"]==morph_ratio]
            avrgd_minmax_confidence = df['min_max_normalised'].mean()
            confidence_results.append((morph_ratio, avrgd_minmax_confidence))

        df_con = pd.DataFrame(confidence_results, columns=['morph', 'avrgd_minmax_confidence'])

        # Subset the data frame to the all condition
        df_sub = df_sum[df_sum["continuum"] == "all"]
        df_sub = df_sub[df_sub["spkr"] == "all"]

        # Merge with the confidence measure
        merged_df = pd.merge(df_sub, df_con, on='morph', how='inner')
        merged_df['weighted_response_minmax'] = merged_df['%yes'] * merged_df['avrgd_minmax_confidence']

        # Fit the model to the data
        params, covariance = curve_fit(logistic_func, merged_df['morph'],
                                                      merged_df['weighted_response_minmax'], p0=[1, 0, 0.8],  maxfev=10000)
        k_fitted, x_0_fitted, L_fitted = params
        print(x_0_fitted)

        # Plot the results
        x_fit = np.linspace(0, 1.0, 100)
        y_fit = logistic_func(x_fit, *params)
        plt.scatter(merged_df['morph'], merged_df['weighted_response_minmax'], color='black', marker='o')
        plt.plot(x_fit, y_fit, label='Fitted Sigmoidal Curve', color='red')
        x_fit = np.linspace(0, 1.0, 100)
        y_fit = logistic_func(x_fit, *params)
        plt.plot(x_fit, y_fit, label='Fitted Sigmoidal Curve', color='red')
        plt.axvline(x_0_fitted, color='gray', linestyle='--', label='y = 0.5 (Turning Point)')
        plt.savefig('/Users/hannahsmacbook/PycharmProjects/AVH/analysis/results/experiment/confidence_weighted_pf/plots/'
                    + participant + '_confidence_psychometric-function_results')
        plt.close()
        results.append((participant,x_0_fitted))
        del(params, covariance)

df = pd.DataFrame(results, columns=['subj', 'PSE'])
df.to_csv('/Users/hannahsmacbook/PycharmProjects/AVH/analysis/confidence-weighted_psychometric-functions.csv')