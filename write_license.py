import os
import shutil
import codecs

license_file = "LICENSE.txt"
folders = ['sdktool', 'shared', 'solarpilot', 'ssc', 'tcs', 'tcsconsole']

# function to prepend file to source code
def lines_prepender(filename, lines):
    with open(filename, 'r+') as f:
        content = f.read()
        f.seek(0,0)
        f.writelines(lines + '\n\n' + content)

# prepend files
top_dir = os.getcwd()
for folder in folders:
    os.chdir(folder)
    files = os.listdir()
    for file in files:
        if file.endswith("h") or file.endswith("cpp"):
            tmpfile = "tmp.txt"
            with open(tmpfile, 'w', encoding='utf-8') as outfile:
                for f in [os.path.join(top_dir, license_file), file]:
                     with codecs.open(f, 'r', encoding='utf-8') as infile:
                        for line in infile:
                            outfile.write(line)
                        #shutil.copyfileobj(infile, outfile, 1024 * 1024 * 10)
            shutil.copyfile(tmpfile, file)
            os.remove(tmpfile)

    os.chdir(top_dir)





