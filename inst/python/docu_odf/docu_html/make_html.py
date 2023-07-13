import os
from os.path import exists
import docu_odf.docu_html.head as head
import docu_odf.docu_html.css as css
import docu_odf.docu_html.body as body

def make(temp_dir):
    # check if temp_dir exists
    isExist = os.path.exists(temp_dir)
    if not isExist:
        os.makedirs(temp_dir)
    # open html file and write
    f = open(temp_dir + "/docu.html", "w", encoding="utf-8")
    # write head
    f.write(head.make("Documentation", css.make()))
    f.close()
    # open html file and append body
    f = open(temp_dir + "/docu.html", "a", encoding="utf-8")
    f.write(body.make(temp_dir))
    f.close()
