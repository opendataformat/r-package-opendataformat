import os.path
import pandas as pd

def make(temp_dir, color):
    if (os.path.isfile(temp_dir + "/dataset.csv") == True) :
        # print header
        print(color.UNDERLINE + color.BOLD + "Dataset" + color.END + "\n")
        # get data
        dataset_df = pd.read_csv(temp_dir + "/dataset.csv", encoding='utf8')
        dataset_df = dataset_df.fillna("")
        # print dataset information
        for i in list(dataset_df.columns):
            print(color.BOLD + i + ": " + color.END + str(dataset_df[i].loc[0]))
        if (os.path.isfile(temp_dir + "/variables.csv") == True) :        
          variables_df =  pd.read_csv(temp_dir + "/variables.csv", encoding='utf8', usecols = ['variable'])
          variables_list = variables_df['variable'].values.tolist()
          variables = ', '.join([str(item) for item in variables_list])
          print(color.BOLD + "variables: " + color.END + str(variables))
    else:
        print("")
    
