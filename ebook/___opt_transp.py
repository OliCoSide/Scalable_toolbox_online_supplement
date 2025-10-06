import os
import json
import glob
import pandas as pd
import numpy as np
from equipy.fairness import FairWasserstein

# Set up paths
source_file_directory = os.path.dirname(os.path.abspath(__file__))
os.chdir(source_file_directory)
output_dir = "transported"
os.makedirs(output_dir, exist_ok=True)

# Utility functions
def clean_output_directory(directory):
    for file in glob.glob(os.path.join(directory, "*.json")):
        os.remove(file)

def load_json_file(file_path):
    with open(file_path, 'r') as file:
        return json.load(file)

def save_json_file(data, file_path):
    """Save data to a JSON file, ensuring numpy arrays are converted to lists."""
    def convert_ndarray(obj):
        if isinstance(obj, np.ndarray):
            return obj.tolist()  # Convert numpy array to list
        raise TypeError(f"Object of type {type(obj)} is not JSON serializable")

    with open(file_path, 'w') as file:
        json.dump(data, file, default=convert_ndarray, indent=4)

def fair_wasserstein_transform(bests_train, bests_valid, bests_test, sens_train, sens_valid, sens_test):
    sens_train_pd = pd.DataFrame(sens_train)
    sens_valid_pd = pd.DataFrame(sens_valid)
    sens_test_pd = pd.DataFrame(sens_test)
    
    b_train_np = np.array(bests_train)
    b_valid_np = np.array(bests_valid)
    b_test_np = np.array(bests_test)
    
    wst = FairWasserstein()
    wst.fit(np.array(bests_train), sens_train_pd)
    return wst.transform(b_train_np, sens_train_pd), wst.transform(b_valid_np, sens_valid_pd), wst.transform(b_test_np, sens_test_pd)

def process_scenario(scenario_name, preds, sims_train, sims_valid, sims_test, output_dir, filter_condition=None):
    sens_train = [item['D'] for item in sims_train[scenario_name]]
    sens_valid = [item['D'] for item in sims_valid[scenario_name]]
    sens_test = [item['D'] for item in sims_test[scenario_name]]

    preds_train = preds['train']
    preds_valid = preds['valid']
    preds_test = preds['test']

    # Transform the filtered data using FairWasserstein
    eps_y_train, eps_y_valid, eps_y_test = fair_wasserstein_transform(
        preds_train, preds_valid, preds_test,
        sens_train, sens_valid, sens_test
    )

    # Adjust the output file name to reflect the filter condition
    result = {"train": eps_y_train, "valid": eps_y_valid, "test": eps_y_test}
    condition_label = "market" if filter_condition is None else filter_condition.replace("=", "_")
    save_json_file(result, os.path.join(output_dir, f'{scenario_name}_bary_mapping.json'))


# Main execution
def main():
    clean_output_directory(output_dir)

    # Define base scenarios
    base_scenarios = ["Scenario1", "Scenario2", "Scenario3"]
    
    # Generate the preds_paths dynamically
    preds_paths = {}

    for scenario in base_scenarios:
        preds_paths[f"{scenario}"] = f'preds/{scenario}_best_estimate.json'

    # Define the sample paths
    sample_paths = {
        "train": 'simuls/train_scenarios.json',
        "valid": 'simuls/valid_scenarios.json',
        "test": 'simuls/test_scenarios.json'
    }

    # Load sample data once
    sims_train = load_json_file(sample_paths["train"])
    sims_valid = load_json_file(sample_paths["valid"])
    sims_test = load_json_file(sample_paths["test"])

    # Process all scenarios and  labels using a loop
    for scenario_label, preds_path in preds_paths.items():
        # Split the scenario_label into scenario and selection_label
        base_scenario = scenario_label.split('_')[0]  # Extract the base scenario (e.g., 'original', 'neutral')

        # Load the predictions for this scenario
        preds = load_json_file(preds_path)

        # Apply the correct filter based on the label
        process_scenario(base_scenario, preds, sims_train, sims_valid, sims_test, output_dir, filter_condition=None)  # Full market

if __name__ == "__main__":
    main()
