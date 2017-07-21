import os
import shutil
import codecs
import time

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
license_path = os.path.join(top_dir, license_file)
for folder in folders:
    os.chdir(folder)
    files = os.listdir()
    for file in files:
        if file.endswith("h") or file.endswith("cpp"):
            try:
                tmpfile = "tmp.txt"
                with open(tmpfile, 'w', encoding='utf-8') as outfile:
                    for f in [license_path, file]:
                        if f == file:
                            print("Writing: " + file)
                        with codecs.open(f, 'r', encoding='utf-8') as infile:
                            for line in infile:
                                if f == license_path:
                                    outfile.write(line.rstrip('\n'))
                                else:
                                    outfile.write(line)
                            outfile.write('\n')
                shutil.copyfile(tmpfile, file)
                os.remove(tmpfile)
            except (UnicodeDecodeError, PermissionError):
                print("File: " + f + " failed!")
                time.sleep(2)
                outfile.close()
                if os.path.exists(tmpfile):
                    os.remove(tmpfile)
    os.chdir(top_dir)





