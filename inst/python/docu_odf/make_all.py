import docu_odf.docu_print.make_print as make_print
import docu_odf.docu_html.make_html as make_html
import docu_odf.rm_csv as rm_csv
import webbrowser

def make(temp_dir, style):
    if style == "all" :
      make_html.make(temp_dir)
      make_print.make(temp_dir)
      # webbrowser.open(temp_dir + "/docu.html", new=0, autoraise=True)
      rm_csv.make(temp_dir)
    if style == "html" :
      make_html.make(temp_dir)
      # webbrowser.open(temp_dir + "/docu.html", new=0, autoraise=True)
      rm_csv.make(temp_dir)
    if style == "print":
      make_print.make(temp_dir)
      rm_csv.make(temp_dir)



