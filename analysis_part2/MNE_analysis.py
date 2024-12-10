"""Script to analyse EEG data"""
import json
import pandas as pd
import pathlib
import os
import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.axes_grid1 import make_axes_locatable
import mne
from mne.channels import find_ch_adjacency
from mne.stats import spatio_temporal_cluster_test
from mne.viz import plot_compare_evokeds


def ignore_conds(d, *keys):
    return dict(filter(lambda key_value: key_value[0] not in keys, d.items()))


# Set directories
DIR = pathlib.Path(os.getcwd())
behavior_results_DIR = DIR / "analysis" / "results" / "study" / "behavior"
EEG_DIR = DIR / "analysis" / "results" / "study" / "EEG"
with open(DIR / "analysis" / "settings" / "preproc_config.json") as file:
    cfg = json.load(file)

# Define subjects to include in the analysis
subjs = ['sub_07', 'sub_08', 'sub_09', 'sub_10', 'sub_11',  'sub_12', 'sub_13', 'sub_14', 'sub_15', 'sub_16', 'sub_18',
         'sub_19', 'sub_20', 'sub_21', 'sub_22', 'sub_23', 'sub_24', 'sub_25', 'sub_26', 'sub_27', 'sub_28']


# Get the behavioural data and prepare for analysis in R
appended_data = []
for subj in subjs:
    data = pd.read_csv(str(behavior_results_DIR) + '/' + subj + '_summarized-behavior.csv')
    appended_data.append(data)
df_behav = pd.concat(appended_data)
df_behav.to_csv("/Users/hannahsmacbook/EEG_voice/behavioural_data.csv")


# ANALYSIS OF THE EEG DATA
# Read in the EEG data
evokeds, evokeds_avrgd = cfg["epochs"][f"event_id"].copy(), cfg["epochs"][f"event_id"].copy()
for key in cfg["epochs"][f"event_id"]:
    evokeds[key], evokeds_avrgd[key] = list(), list()

for subj in subjs:
    evokeds_folder = str(EEG_DIR) + "/" + subj + "/evokeds"
    evoked = mne.read_evokeds(evokeds_folder + "/" + subj + '-ave.fif')
    for condition in evoked:
        if condition.comment in evokeds:
            evokeds[condition.comment].append(condition.crop(-0.2, 0.5))
            if len(evokeds[condition.comment]) == len(subjs):
                evokeds_avrgd[condition.comment] = mne.grand_average(
                    evokeds[condition.comment])
            else:
                continue

# Get the combined evokeds
combined_evokeds = mne.combine_evoked([evokeds_avrgd["morph/0.0"], evokeds_avrgd["morph/0.4"],
                                      evokeds_avrgd["morph/0.6"], evokeds_avrgd["morph/1.0"]],
                                      weights=[0.25, 0.25, 0.25, 0.25])

# Update the plot parameters
# Plot the results
plt.rcParams.update({
    'font.family': 'Times New Roman',
    'axes.facecolor': 'white',
    'figure.facecolor': 'white',
    'axes.grid': True,
    'grid.color': 'grey',
    'grid.linestyle': '-',
    'grid.linewidth': 0.5,
    'font.size': 16
})

"""
# Plot the GFP (mne implemenation)
plot_compare_evokeds(
    ignore_conds(
        evokeds, "deviant", "morph/0.4", "morph/0.6"),
    colors=["firebrick", "darkblue"],
    show=False,
    legend= "upper right"
)
"""
# Get and plot the difference wave for the GFP:
# Get the GFP per participant per stimulus condition
conditions = ['morph/0.0', 'morph/1.0']
compared_gfps = []
for condition in conditions:
    gfps = []
    for subj_idx in enumerate(subjs):
        gfp = evokeds[condition][subj_idx[0]].data.std(axis=0, ddof=0)
        gfps.append(gfp)
    compared_gfps.append(gfps)


gfp_results = []
for x in range(len(compared_gfps)):
    result = np.vstack(compared_gfps[x])
    ci_lower = []
    ci_upper = []
    means = []
    for i in range(result.shape[1]):
        column = result[:, i]
        mean = np.mean(column)
        n_bootstrap = 10000  # Number of bootstrap samples
        bootstrap_means = []
        np.random.seed(42)  # For reproducibility
        for _ in range(n_bootstrap):
            bootstrap_sample = np.random.choice(column, size=len(column), replace=True)  # Sample with replacement
            bootstrap_means.append(np.mean(bootstrap_sample))  # Calculate the mean for this bootstrap sample
        lower_percentile = 2.5  # Lower bound for 95% CI
        upper_percentile = 97.5  # Upper bound for 95% CI
        lower_ci = np.percentile(bootstrap_means, lower_percentile)
        upper_ci = np.percentile(bootstrap_means, upper_percentile)

        ci_lower.append(lower_ci)
        ci_upper.append(upper_ci)
        means.append(mean)
    gfp_results.append((x, ci_lower, ci_upper, means))

# Get the difference wave
difference_gfp = np.subtract(gfp_results[0][3], gfp_results[1][3])

# Plot the results
fig, axs = plt.subplots()
axs.plot(combined_evokeds.times, [x * 1e6 for x in gfp_results[0][3]], color="firebrick")
axs.plot(combined_evokeds.times, [x * 1e6 for x in gfp_results[1][3]], color="darkblue")
axs.plot(combined_evokeds.times, [x * 1e6 for x in difference_gfp], color="black")
axs.fill_between(combined_evokeds.times, difference_gfp * 1e6, color="gray", alpha=0.2)
axs.fill_between(combined_evokeds.times, [x * 1e6 for x in gfp_results[0][2]], [x * 1e6 for x in gfp_results[0][1]],
                 color="lightsalmon", alpha=0.3)
axs.fill_between(combined_evokeds.times, [x * 1e6 for x in gfp_results[1][2]], [x * 1e6 for x in gfp_results[1][1]],
                 color="lightblue", alpha=0.3)

# Adjusting the plot and including a legend
axs.set(xlabel="Time (s)", ylabel="GFP (µV)")
axs.set_xlim(-0.1, 0.5)
plt.tight_layout()
plt.legend()
blue_patch = plt.Line2D([0], [0], color='firebrick', lw=2, label='Morph 0.0')
red_patch = plt.Line2D([0], [0], color='darkblue', lw=2, label='Morph 1.0')
black_patch = plt.Line2D([0], [0], color='black', lw=2, label='Difference wave')
plt.legend(handles=[blue_patch, red_patch, black_patch], loc='upper right')

plt.savefig('gfp.pdf', format='pdf')

# Difference Topography
difference_wave = mne.combine_evoked([evokeds_avrgd["morph/1.0"], evokeds_avrgd["morph/0.0"]], weights=[-1, 1])
difference_wave.plot_topomap(times=[0.10, 0.2, 0.35])
plt.savefig('gfp_topomap.pdf', format='pdf')

"""
Permutation Cluster Test
To test for difference in at least one evoked response with an F test to compare the mean of several (>2) groups
Based on https://mne.tools/dev/auto_tutorials/stats-sensor-space/75_cluster_ftest_spatiotemporal.html
"""

# Get data of all epochs for one condition in one numpy array
for subj in subjs:
    epochs_folder = EEG_DIR / subj / "epochs"
    epochs = mne.read_epochs(epochs_folder / pathlib.Path(subj + '-epo.fif'))
    conditions = ['morph/0.0', 'morph/1.0']
    epochs.equalize_event_counts(conditions)
    event_ids = [1, 2]
    indices = [np.where(epochs.events[:, 2] == event_id)[0] for event_id in event_ids]
    if subj == "sub_07":  # = first subject
        X_total = [epochs.get_data()[idx, :, :].transpose(0, 2, 1) for idx in indices]
    else:
        X = [epochs.get_data()[idx, :, :].transpose(0, 2, 1) for idx in indices]
        for event_id in event_ids:
            X_total[event_id-1] = np.concatenate((X_total[event_id-1], X[event_id-1]))

# Set the test parameters
adjacency, ch_names = find_ch_adjacency(epochs.info, ch_type="eeg")  # compute channel adjacency
tail = 0  # F-test-> upper tail only

# Run the test
cluster_stats = spatio_temporal_cluster_test(
    X_total,
    n_permutations=1000,
    tail=tail,
    n_jobs=None,
    buffer_size=None,
    adjacency=adjacency
)

F_obs, clusters, p_values, _ = cluster_stats

# Save the results of the permutation cluster test
np.save('F_obs.npy', F_obs)
np.savez('clusters', *clusters)
np.save('p_values.npy', p_values)

"""
# Read the results from the permutation cluster test
F_obs = np.load('F_obs.npy')
p_values = np.load('p_values.npy')
clusters= np.load('clusters.npz')
clusters = [tuple(clusters[key]) for key in clusters.files]
"""

# Subset to significant clusters
p_accept = 0.05
good_cluster_inds = np.where(p_values < p_accept)[0]
print(len(good_cluster_inds))

# Plot the results of the permutation cluster test
ch_inds_all = np.arange(0, 65, 1, dtype=int)
# Loop over significant clusters and plot the results
for i_clu, clu_idx in enumerate(good_cluster_inds):
    # Get the parameters per cluster
    time_inds, space_inds = np.squeeze(clusters[clu_idx])
    ch_inds = np.unique(space_inds)
    time_inds = np.unique(time_inds)

    # Get the topography for F statistic in the time frame
    f_map = F_obs[time_inds, ...].mean(axis=0)
    # Get the signals at the respective sensors
    sig_times = epochs.times[time_inds]

    # Initialise figure
    fig, ax_topo = plt.subplots(1, 1, figsize=(10, 3), layout="constrained")
    plt.tight_layout()
    # Plot the F score topo map
    f_evoked = mne.EvokedArray(f_map[:, np.newaxis], epochs.info, tmin=0)
    f_evoked.plot_topomap(
        times=0,
        axes=ax_topo,
        cmap="Reds",
        vlim=(np.min, np.max),
        show=False,
        colorbar=False
    )
    image = ax_topo.images[0]
    ax_topo.set_title("")
    divider = make_axes_locatable(ax_topo)
    ax_colorbar = divider.append_axes("right", size="5%", pad=0.05)
    plt.colorbar(image, cax=ax_colorbar)
    ax_topo.set_xlabel(
        "Averaged F-map ({:0.3f} - {:0.3f} s)".format(*sig_times[[0, -1]])
    )
    # ERP plot
    ax_signals = divider.append_axes("right", size="300%", pad=1.2)
    title = f"Cluster #{i_clu + 1}, {len(ch_inds)} sensor"

    plot_compare_evokeds(
        ignore_conds(
            evokeds, "deviant"),
        title=title,
        picks=ch_inds,
        colors=["firebrick", "lightsalmon", "lightblue", "darkblue"],
        axes=ax_signals,
        show=False,
        split_legend=True,
        truncate_yaxis="auto",
        ci=None
    )

    # Add the legend using custom entries
    red_patch = plt.Line2D([0], [0], color='firebrick', lw=2, label='Morph 0.0')
    lightred = plt.Line2D([0], [0], color='lightsalmon', lw=2, label='Morph 0.4')
    lightblue = plt.Line2D([0], [0], color='lightblue', lw=2, label='Morph 0.6')
    blue_patch = plt.Line2D([0], [0], color='darkblue', lw=2, label='Morph 1.0')
    legend = plt.legend(handles=[red_patch, lightred, lightblue, blue_patch], loc='upper right', fontsize=12)

    # Temporal cluster extent
    ymin, ymax = ax_signals.get_ylim()
    ax_signals.fill_betweenx(
        (ymin, ymax), sig_times[0], sig_times[-1], color="lightgrey", alpha=0.3
    )
plt.show()
plt.savefig('permutation_cluster-test.pdf', format='pdf')

# Plot compared evokeds for C1 and Cz
fig, axs = plt.subplots(1, 2, figsize=(10, 3))

fig_C1 = mne.viz.plot_compare_evokeds([evokeds["morph/0.0"], evokeds["morph/0.4"], evokeds["morph/0.6"],
                                       evokeds["morph/1.0"]], picks=["C1"], ylim=dict(eeg=[-2, 4]),
                                      colors=["firebrick", "lightsalmon", "lightblue", "darkblue"],
                                      show_sensors="upper left", legend=False, ci=False, axes=axs[0])

fig_Cz = mne.viz.plot_compare_evokeds([evokeds["morph/0.0"], evokeds["morph/0.4"], evokeds["morph/0.6"],
                                       evokeds["morph/1.0"]], picks=["Cz"], ylim=dict(eeg=[-2, 4]),
                                      colors=["firebrick", "lightsalmon", "lightblue", "darkblue"],
                                      show_sensors="upper left", legend=False, ci=False, axes=axs[1])

for ax in fig_Cz:  # Assuming fig_C1 is a list of axes
    ax.legend(handles=[red_patch, lightred, lightblue, blue_patch], bbox_to_anchor=(0.90, 0.86), fontsize=12)

plt.show()
plt.savefig('evoked-responses.pdf', format='pdf')

# Amplitude measurements
conditions = ['morph/0.0', 'morph/0.4', 'morph/0.6', 'morph/1.0']
results = []
for subj_idx in enumerate(subjs):
    for condition in conditions:
        evok = evokeds[condition][subj_idx[0]]
        gfp = evok.data.std(axis=0, ddof=0)
        amp_126_450 = gfp[time_inds].mean() * 1e6
        amp_126_250 = evokeds[condition][subj_idx[0]].copy().crop(tmin=0.126, tmax=0.25).data.std(axis=0, ddof=0).mean() * 1e6
        amp_250_450 = evokeds[condition][subj_idx[0]].copy().crop(tmin=0.25, tmax=0.45).data.std(axis=0, ddof=0).mean() * 1e6
        voice_responses = df_behav.loc[(df_behav['subj'] == subj_idx[1]) &
                                       (df_behav['Morph ratio'] == float(condition[-3:]))].iloc[0]['%Voice']
        result = (subj_idx[1], float(condition[-3:]), amp_126_450, amp_126_250, amp_250_450, voice_responses)
        results.append(result)

df = pd.DataFrame(results, columns=['subj', 'condition', 'amp_126-450', 'amp_126-250', 'amp_250-450', '%Voice'])
df.to_csv("/Users/hannahsmacbook/EEG_voice/EEG_data.csv")

