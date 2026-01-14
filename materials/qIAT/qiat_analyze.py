import pandas as pd
from io import StringIO
import numpy as np
import os
PAIRED_EVENS = [0, 1]
try:
    from config import *
except ImportError:
    # If config doesn't exist, create a minimal one
    LOCAL_ASSETS_FOLDER = ""
try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    HAS_PSUTIL = False
import gc
try:
    from logger_util import print_with_timestamp
except ImportError:
    def print_with_timestamp(message):
        print(message)


# Function to log memory usage
def log_memory_usage(stage):
    if HAS_PSUTIL:
        process = psutil.Process(os.getpid())
        mem_info = process.memory_info()
        print_with_timestamp(f"{stage} - RSS: {mem_info.rss / (1024 * 1024)} MB, VMS: {mem_info.vms / (1024 * 1024)} MB")
    else:
        print_with_timestamp(f"{stage} - Memory logging unavailable (psutil not installed)")



# Define the parse_and_analyze function
def parse_and_analyze(file_path, qiat_column_name='qIAT', id_column_name='ResponseId', task_name='qIAT',
                      exclusion_too_fast=300, exclusion_max_fast_perc=0.1, exclusion_max_error_perc=0.3,
                      exclusion_max_latency=30000, analyze_max_latency=10000, analyze_min_latency=400):
    
    CHUNK_SIZE = 1002 # Number of participants to read in each chunk. We use 1002 so we could support 3000 in 3 chunks

    log_memory_usage("At the beginning")
    # Detect file type
    file_extension = os.path.splitext(file_path)[1].lower()

    # Determine how to read the file in chunks
    if file_extension == '.csv':
        chunk_iter = pd.read_csv(file_path, chunksize=CHUNK_SIZE)
    elif file_extension in ['.xls', '.xlsx']:
        chunk_iter = pd.read_excel(file_path, chunksize=CHUNK_SIZE)
    else:
        raise ValueError("Unsupported file format")
    
    # Initialize the Excel writer and create the sheets
    new_file_path = f"{LOCAL_ASSETS_FOLDER + task_name}.xlsx"
    with pd.ExcelWriter(new_file_path, engine='openpyxl', mode='w') as writer:
        # Write empty DataFrames to create the sheets
        pd.DataFrame().to_excel(writer, sheet_name='Participants')
        pd.DataFrame().to_excel(writer, sheet_name='Trials')
    
    is_first_chunk = True
    
    # Process each chunk of data
    participant_row_offset = 0
    trial_row_offset = 0

    # Process each chunk of data
    for i, chunk in enumerate(chunk_iter):
        print_with_timestamp(f"Processing chunk {i + 1}")
        
        parsed = qiat_parse_quoted(chunk, id_column_name, qiat_column_name)
                
        participants, trials = analyze_data(parsed,
                                            exclusion_too_fast=exclusion_too_fast,
                                            exclusion_max_fast_perc=exclusion_max_fast_perc,
                                            exclusion_max_error_perc=exclusion_max_error_perc,
                                            exclusion_max_latency=exclusion_max_latency,
                                            analyze_max_latency=analyze_max_latency,
                                            analyze_min_latency=analyze_min_latency)
        
        # Append the results to the existing Excel file
        with pd.ExcelWriter(new_file_path, engine='openpyxl', mode='a', if_sheet_exists='overlay') as writer:
            
            # Write the participants data
            participants.to_excel(writer, sheet_name='Participants', startrow=participant_row_offset, index=False, header=is_first_chunk)
            participant_row_offset += len(participants)

            # Write the trials data
            trials.to_excel(writer, sheet_name='Trials', startrow=trial_row_offset, index=False, header=is_first_chunk)
            trial_row_offset += len(trials)

        if is_first_chunk:
            # dealing with headers in the first chunk
            participant_row_offset += 1
            trial_row_offset += 1
            # Set is_first_chunk to False after processing the first chunk
            is_first_chunk = False
        log_memory_usage("after processing chunk " + str(i))
        # Invoke garbage collection after writing
        gc.collect()

    log_memory_usage("At the end")
    return new_file_path



def merge_dataframes_and_compute(con_data, incon_data):
    context = "merge_dataframes_and_compute"


    # Rename columns for each dataframe except 'id'
    con_columns = {col: col + '_con' for col in con_data.columns if col != 'id'}
    incon_columns = {col: col + '_incon' for col in incon_data.columns if col != 'id'}

    con_data_renamed = con_data.rename(columns=con_columns)
    incon_data_renamed = incon_data.rename(columns=incon_columns)

    # Merge the dataframes on 'id', ensuring all IDs are included
    merged_df = pd.merge(con_data_renamed, incon_data_renamed, on='id', how='outer')

    # Calculate pooled standard deviation
    merged_df['pooled_sd'] = np.sqrt(((merged_df['variance_con'] * (merged_df['n_con'] - 1)) +
                                      (merged_df['variance_incon'] * (merged_df['n_incon'] - 1))) /
                                     ((merged_df['n_con'] + merged_df['n_incon']) - 2))

    # Calculate D-scores
    merged_df['dscore'] = (merged_df['mean_con'] - merged_df['mean_incon']) / merged_df['pooled_sd']

    return merged_df



def calculate_dscore(grouped_data, cong, incong):
    context = "calculate_dscore"


    # Initialize a DataFrame to store the results
    results = pd.DataFrame()

    for (id, block), group in grouped_data:
        # Compute mean, variance, and count for each group
        mean = group['latency'].mean()
        variance = group['latency'].var(ddof=1)  # ddof=1 for sample variance
        count = group['latency'].count()
        condition = group['condition'].iloc[0]

        # Store the results
        new_row = pd.DataFrame({
            'id': [id],
            'block': [block],
            'mean': [mean],
            'variance': [variance],
            'n': [count],
            'condition': condition
        })

        results = pd.concat([results, new_row], ignore_index=True)

    results_2 = results.pivot(index='id', columns='block').sort_index(level=1, axis=1)
    results_2.columns = [f'{col[0]}_block_{int(col[1])}' if (col[1] and not col[0] == "condition") else col[0] for col in results_2.columns]
    results_2.reset_index(inplace=True)
    results_2 = results_2.loc[:, ~results_2.columns.duplicated()].copy()

    # Calculate D-Score for each congruent/incongruent pair
    for i in range(len(cong)):
        con_block = cong[i]
        incon_block = incong[i]

        # Filter data for congruent and incongruent blocks
        con_data = results[results['block'] == con_block].reset_index()
        incon_data = results[results['block'] == incon_block].reset_index()

        # merged the two dfs and computes dscore and sd
        merged_df = merge_dataframes_and_compute(con_data, incon_data)

        # Rename the columns dynamically based on i
        merged_df = merged_df.rename(columns={'dscore': f'dscore_{i}', 'pooled_sd': f'pooled_sd_{i}'})

        # Merge the dynamically named columns into results_2 dataframe
        results_2 = pd.merge(results_2, merged_df[['id', f'dscore_{i}', f'pooled_sd_{i}']], on='id', how='left')

    # Calculate aggregated d-score if multiple conditions are present
    if len(cong) > 1:
        dscore_cols = [f'dscore_{i}' for i in range(len(cong))]
        results_2['dscore'] = results_2[dscore_cols].mean(axis=1)
    else:
        results_2['dscore'] = results_2['dscore_0']

    # Multiplying 'd_score' by -1 if 'condition' is not 0
    results_2.loc[results_2['condition'] != 0, 'dscore'] = results_2.loc[results_2['condition'] != 0, 'dscore'] * -1
    results_2.drop('dscore_0', axis=1, inplace=True)

    return results_2



def analyze_data(
        data,
        cong=[4],
        incong=[6],
        exclusion_too_fast=300,
        exclusion_max_fast_perc=0.1,
        exclusion_max_error_perc=0.3,
        exclusion_max_latency=30000,
        analyze_max_latency=10000,
        analyze_min_latency=400
        ):


    if len(cong) != len(incong):
        raise ValueError(f'length of congruent {len(cong)} doesn\'t match length of incongruent {len(incong)}')

    # if no data, return empty dataframes
    if data.empty:
        return pd.DataFrame(), pd.DataFrame()
      
    # Data Preparation
    data['isEven'] = data.index % 2
    data['isPairedEven'] = data.index % 4
    data['isPairedEven'] = data['isPairedEven'].isin(PAIRED_EVENS)

    # Grouping and Numbering
    data['trialN'] = data.groupby('id').cumcount() + 1
    data['trialNBlock'] = data.groupby(['id', 'block']).cumcount() + 1

    # Random Lefts Calculation
    unique_ids = data['id'].unique()
    random_lefts = {id: np.random.choice(range(20), size=10, replace=False) for id in unique_ids}
    data['isLeft'] = data.apply(lambda row: row['trialNBlock'] % 20 in random_lefts[row['id']], axis=1)

    # Create a boolean Series to indicate valid blocks
    valid_blocks = data['block'].isin(cong + incong)

    # Use this Series to filter the DataFrame
    filtered_data_for_validity = data[valid_blocks]

    # Aggregating nTrials, maxLatency, percFast, percError
    participants_validity = filtered_data_for_validity.groupby('id').agg(
        nTrials=pd.NamedAgg(column='block', aggfunc=lambda x: sum(x.isin(cong + incong))),
        maxLatency=pd.NamedAgg(column='latency', aggfunc='max'),
        percFast=pd.NamedAgg(column='latency', aggfunc=lambda x: sum(x <= exclusion_too_fast) / len(x)),
        percError=pd.NamedAgg(column='correct', aggfunc=lambda x: sum(x.astype(bool).apply(lambda val: not val)) / len(x))
    ).reset_index()

    participants_validity['exclude'] = participants_validity.apply(
        lambda row: (
                row['percError'] > exclusion_max_error_perc or
                row['percFast'] > exclusion_max_fast_perc or
                row['maxLatency'] > exclusion_max_latency or
                row['nTrials'] < 40
        ), axis=1
    )

    # Filter data for valid blocks and latency
    filtered_data = data[(data['block'].isin(cong + incong)) & (data['latency'] >= analyze_min_latency) & (
                data['latency'] <= analyze_max_latency)]

    # Some rearrangements to make final file clearer - moved here to avoid errors in case of no valid participants

    data['stimulus'] += 1  # so that stimulus count from 1 and not from 0

    # Convert boolean 'True' and 'False' to 1 and 0 for 'isEven' and 'isLeft' columns
    data['isPairedEven'] = data['isPairedEven'].astype(int)
    data['isLeft'] = data['isLeft'].astype(int)

    # Rename 'isLeft' column to 'inHalf1'
    data.rename(columns={'isLeft': 'inHalf1'}, inplace=True)

    # Reorder the trial's DataFrame
    data = data[[
        'id', 'condition', 'trialN', 'block', 'trialNBlock', 'group',
        'stimulus', 'latency', 'correct', 'isEven', 'isPairedEven', 'inHalf1'
    ]]

    # if no valid participants, return partial result
    if len(participants_validity[participants_validity['exclude'] == False]) == 0:
        return participants_validity, data


    # Use this filtered data for D-Score calculations
    grouped_data = filtered_data.groupby(['id', 'block'])

    # Computing all parameters for entire task
    dscores_all = calculate_dscore(grouped_data, cong, incong)

    # Computing all parameters for even trials
    boolean_filter_even = filtered_data['isEven'] == 0
    dscores_even = calculate_dscore(filtered_data[boolean_filter_even].groupby(['id', 'block']), cong, incong)

    # Computing all parameters for odd trials, and so on ...
    boolean_filter_odd = filtered_data['isEven'] == 1
    dscores_odd = calculate_dscore(filtered_data[boolean_filter_odd].groupby(['id', 'block']), cong, incong)

    # Create boolean series directly from 'filtered_data'
    isPairedEven_filter = filtered_data['isPairedEven']
    isLeft_filter = filtered_data['isLeft']

    dscores_paired_even = calculate_dscore(filtered_data[isPairedEven_filter].groupby(['id', 'block']), cong, incong)
    dscores_paired_odd = calculate_dscore(filtered_data[~isPairedEven_filter].groupby(['id', 'block']), cong, incong)
    dscores_random_lefts = calculate_dscore(filtered_data[isLeft_filter].groupby(['id', 'block']), cong, incong)
    dscores_random_rights = calculate_dscore(filtered_data[~isLeft_filter].groupby(['id', 'block']), cong, incong)

    final_result = participants_validity
    final_result = final_result.merge(dscores_all, on='id', how='left')

    final_result = final_result.merge(dscores_even[['id', 'dscore']], on='id', how='left', suffixes=('', '_even'))
    final_result = final_result.merge(dscores_odd[['id', 'dscore']], on='id', how='left', suffixes=('', '_odd'))
    final_result = final_result.merge(dscores_paired_even[['id', 'dscore']], on='id', how='left', suffixes=('', '_paired_even'))
    final_result = final_result.merge(dscores_paired_odd[['id', 'dscore']], on='id', how='left', suffixes=('', '_paired_odd'))
    final_result = final_result.merge(dscores_random_lefts[['id', 'dscore']], on='id', how='left', suffixes=('', '_random_half1'))
    final_result = final_result.merge(dscores_random_rights[['id', 'dscore']], on='id', how='left', suffixes=('', '_random_half2'))

    # Change 'pooled_sd_0' to 'pooled_sd' in final_result
    final_result.rename(columns={'pooled_sd_0': 'pooled_sd'}, inplace=True)

    # Reorder the final result DataFrame
    final_result = final_result[[
        'id', 'nTrials', 'maxLatency', 'percFast', 'percError', 'exclude', 'condition',
        'n_block_4', 'mean_block_4', 'variance_block_4', 'n_block_6', 'mean_block_6',  'variance_block_6',
        'pooled_sd', 'dscore', 'dscore_even', 'dscore_odd', 'dscore_paired_even', 'dscore_paired_odd',
        'dscore_random_half1', 'dscore_random_half2'
    ]]
    return final_result, data

def qiat_parse_quoted(df, id_col, data_col):
    # optimized version of qiat_parse_quoted
    
    
    # Ensuring the specified columns exist in the DataFrame
    if id_col not in df.columns or data_col not in df.columns:
        raise ValueError(f"Columns {id_col} or {data_col} not found in the DataFrame.")

    # Dropping rows where data_col is NA or empty
    df_filtered = df.dropna(subset=[data_col])
    df_filtered = df_filtered[df_filtered[data_col] != '']

    # Parse CSV data from the column
    csv_list = []
    for index, row in df_filtered.iterrows():
        try:
            # Adjusted the read_csv call to use 'on_bad_lines'
            csv_df = pd.read_csv(StringIO(row[data_col]), on_bad_lines='skip')
            if not csv_df.empty:
                csv_df['id'] = row[id_col]  # Directly add the id_col to csv_df
                csv_list.append(csv_df)  # Append directly to csv_list
        except Exception as e:
            print_with_timestamp(f"Malformed CSV at index {index}: {e}")
            csv_list.append(pd.DataFrame())

    # Concatenate all DataFrames
    if not csv_list:
        return pd.DataFrame()

    result = pd.concat(csv_list, ignore_index=True)

    return result

