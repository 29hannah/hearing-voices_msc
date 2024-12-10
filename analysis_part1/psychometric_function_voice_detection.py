"""
Script to fit the psychometric functions
For this psignifit is required
"""

from os import listdir
import os
import pandas as pd
import itertools
import matplotlib.pyplot as plt
import pythonpsignifitmaster.psignifit as ps

DIR = os.getcwd()

# Set the directory to  the summarized results files (output from data wrangling script )
data_dir_sum = DIR+"/analysis/results/experiment/summarized_data"
# Get all files in that directory
files_sum = [file for file in listdir(data_dir_sum) if file.endswith('.csv')]  # only get the results csv files
files_sum = [k for k in files_sum if 'P1_1_former' not in k]


# Set the directory to where the raw results files are
data_dir_raw = DIR + "/analysis/data_experiment"

# Define the results directory and create them if they don't already exist
results_plots_dir = DIR+"/analysis/results/experiment/psychometric_function_results/plots"
if not os.path.exists(results_plots_dir):
    os.makedirs(results_plots_dir)
    print("path did not exist, path created")
results_files_dir = DIR+"/analysis/results/experiment/psychometric_function_results/files"
if not os.path.exists(results_files_dir):
    os.makedirs(results_files_dir)
    print("path did not exist, path created")

# Get all possible factor combinations
loudspeakers = [3, 15, 27, 68, "all"]
continua = ["whisper_2", "babble", "all"]
combinations = list(itertools.product(loudspeakers, continua))

# Iterate over files (since one file=participant, basically iterating over participants)
for file_sum in files_sum:
    # Read in summarized data as pandas dataframe
    df_sum = pd.read_csv(data_dir_sum + '/' + file_sum)
    # Get participant name
    participant = df_sum["subj"][1]
    print(participant)
    # Get the unsummarized data file
    if file_sum == 'P1_2_current_summarized-data.csv':
        sub_dir = "/Users/hannahsmacbook/PycharmProjects/AVH/analysis/data_experiment/P1_2_current/ME1"
    elif file_sum == 'P1_1_former_summarized-data.csv':
        sub_dir = "/Users/hannahsmacbook/PycharmProjects/AVH/analysis/data_experiment/P1_1_former/ME1"
    else:
        sub_dir = data_dir_raw + '/' + file_sum.split('_')[0] + '/ME1'  # get the directory for one participant

    participant_results = []
    results_list = []
    for combination in combinations:
        # Subset the data frame of summarized data based on factor combinations
        loudspeaker = combination[0]
        continuum = combination[1]
        df_sub = df_sum[df_sum["continuum"] == continuum]
        df_sub = df_sub[df_sub["spkr"] == str(loudspeaker)]

        if df_sub["yes"].std() == 0:  # When all answers the same, Nan Value
            threshold = float("Nan")
            width = float("Nan")
            lambda_pf = float("Nan")
            gamma = float("Nan")
            eta = float("Nan")
        elif df_sub[df_sub["morph"] == 0.0]["%yes"].iloc[0] > 0.5:
            threshold = float("Nan")
            width = float("Nan")
            lambda_pf = float("Nan")
            gamma = float("Nan")
            eta = float("Nan")
        elif df_sub[df_sub["morph"] == 1.0]["%yes"].iloc[0] < 0.5:
            threshold = float("Nan")
            width = float("Nan")
            lambda_pf = float("Nan")
            gamma = float("Nan")
            eta = float("Nan")

        else:
            # Fit the Psychometric Function and get the PSE
            data = df_sub.copy()
            data = data[['morph', 'yes', 'N']]
            data_np = data.to_numpy()

            # Define how to fit psychometric function
            options = dict()  # initialize as an empty dictionary
            options['sigmoidName'] = 'logistic'  # choose a cumulative Gauss as the sigmoid
            options['expType'] = 'equalAsymptote'
            result = ps.psignifit(data_np, options)
            threshold = result['Fit'][0]
            width = result['Fit'][1]
            lambda_pf = result['Fit'][2]
            gamma = result['Fit'][3]
            eta = result['Fit'][4]

            del result['Posterior']
            del result['weight']

            # Plot the results
            fig, axs = plt.subplots(1, 1, figsize=(10, 8))
            # Psychometric function
            ps.psigniplot.plotPsych(result,  plotThresh=False,  extrapolLength=.01, axisHandle=axs)
            axs.axvline(x=threshold)
            axs.set_ylim(-0.1, 1.1)
            axs.set_ylabel("% Voice responses")
            axs.set_xlabel("Morph ratio")
            axs.axhline(y=0.5)
            # Save and close the plot
            plt.savefig(results_plots_dir + '/' + participant + '_' + str(combination[0]) + str(combination[1]) +
                        '_psychometric-function_results')
            plt.close()

            # Save the results
        results_list.append((participant, combination[0], combination[1], threshold, width, lambda_pf, gamma, eta))

    # Save the results
    df_results = pd.DataFrame(results_list, columns=['subj', 'spkr', 'continuum', 'PSE', 'width', 'lambda', 'gamma',
                                                     'eta'])
    df_results.to_csv(results_files_dir+'/'+participant + '_psychometric-function_results.csv')

print("Done, all participants' data analysed")


# Append all data to one dataframe and save it to R project
files_psych = [file for file in listdir(results_files_dir) if file.endswith('.csv')]
appended_data = []
for file in files_psych:
    print(file)
    data = pd.read_csv(results_files_dir + '/' + file)
    appended_data.append(data)


results = pd.concat(appended_data)
results.to_csv('/Users/hannahsmacbook/AVH/Analysis/summarised-data_pf_all-participants.csv')
