import docu_odf.docu_print.dataset as dataset
import docu_odf.docu_print.variables as variables

def make(temp_dir):
    class color:
       BOLD = '\033[1m'
       UNDERLINE = '\033[4m'
       END = '\033[0m'
    dataset.make(temp_dir, color)
    print("\n")
    variables.make(temp_dir, color)
