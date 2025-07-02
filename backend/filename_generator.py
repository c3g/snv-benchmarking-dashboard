# filename_generator.py
# Generate standardized filenames for hap.py files based on metadata input

import pandas as pd
from populate_metadata import load_csv_metadata
from config import METADATA_CSV_PATH


# Metadata CSV file path
metadata_file_path = METADATA_CSV_PATH

def strip_value(value):
    if value is None or pd.isna(value): 
        return None
    return str(value).strip()

def preview_row(row):
    """
    Print experiment metadata in a readable format
    
    Args:
        row: pandas Series containing experiment metadata
    """
    print(f"Name: {row.get('name')}")
    print(f"Technology: {row.get('technology')}")
    print(f"Platform: {row.get('platform_name')}")
    print(f"Caller: {row.get('caller_name')}")
    print(f"Truth Set: {row.get('truth_set_name')}")

def create_standardized_filename(row, experiment_id):
    """
    Create standardized filename using format: 
    {sample}_{technology}_{platform}_{caller}_{truthset}_{experiment_id}.csv
    
    Args:
        row: pandas Series containing experiment metadata
        experiment_id: ID number for the experiment
        
    Returns:
        str: standardized filename
    """
    
    # Extract sample name (first part before underscore)
    sample = strip_value(str(row.get('name', '')).split('_')[0])
    
    # Clean all metadata components using clean_value function
    technology = strip_value(row.get('technology', ''))
    platform = strip_value(row.get('platform_name', ''))
    caller = strip_value(row.get('caller_name', ''))
    truthset = strip_value(row.get('truth_set_name', ''))
    
    # Build filename with cleaned components
    filename = f"{experiment_id:03d}_{sample}_{technology}_{platform}_{caller}_{truthset}.csv" # 3 zero paddings for correct order
    
    return filename

def update_csv_with_filename(file_path, row_position, new_filename):
    """
    Update the metadata CSV file with the generated filename
    
    Args:
        file_path: path to metadata CSV file
        row_position: position of the row to update (0-based)
        new_filename: the generated filename to insert
        
    Returns:
        bool: True if successful, False otherwise
    """
    
    try:
        # Read the CSV file
        df = pd.read_csv(file_path)
        
        # Update the file_name column for the specific row and save back to csv
        df.loc[row_position, 'file_name'] = new_filename
        df.to_csv(file_path, index=False)
        
        print(f"Updated CSV file: Set file_name to '{new_filename}' for row {row_position + 1}")
        return True
        
    except Exception as e:
        print(f"Error updating CSV file: {e}")
        return False

# ============================================================================
# GENERATE FILE NAME (Interactive)
# ============================================================================

def generate_filename(file_path):
    """
    Main function to generate standardized filename for selected experiment
    
    Args:
        file_path: path to metadata CSV file
    """
    
    # Load metadata from CSV file
    df = load_csv_metadata(file_path)
    if df is None:
        print("Error: Could not load metadata file")
        return
    
    # Get user's choice
    try:
        choice = input(f"\nChoose Experiment number (1 to {len(df)}): ").strip()
        user_choice = int(choice)
        
        # Validate input range
        if user_choice < 1 or user_choice > len(df):
            print(f"Error: Please choose a number between 1 and {len(df)}")
            return
        
        # Convert user input (1-based) to pandas position (0-based)
        row_position = user_choice - 1
        row = df.iloc[row_position]
        
        # Display selected experiment details
        print(f"\n--- Experiment {user_choice} Details ---")
        preview_row(row)
        
        # Use user's choice as experiment ID (1-based numbering)
        experiment_id = user_choice
        
        # Generate standardized filename
        filename = create_standardized_filename(row, experiment_id)
        
        # Display result
        print(f"\n--- Generated Filename ---")
        print(f"Filename: {filename}")
        
        # Second prompt
        update_choice = input(f"\nDo you want to update the CSV file with this filename? (y/n): ").strip().lower()
        
        if update_choice in ['y', 'yes']:
            # Update the CSV file
            success = update_csv_with_filename(file_path, row_position, filename)
            
            if success:
                print(f"Row {user_choice} now has filename: {filename}")
            else:
                print(f"Failed to update CSV file")
        
    except ValueError:
        print("Error: Please enter a valid number")
    except Exception as e:
        print(f"Error: {e}")


if __name__ == "__main__":
    generate_filename(metadata_file_path)