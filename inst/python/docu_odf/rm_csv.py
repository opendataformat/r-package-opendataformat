import os

def make(temp_dir):
    files_in_directory = os.listdir(temp_dir)
    filtered_files = [file for file in files_in_directory if file.endswith(".csv")]
    for file in filtered_files:
        path_to_file = os.path.join(temp_dir, file)
        os.remove(path_to_file)
