import pandas as pd
from pathlib import Path
import requests
import sys
import os
import platform

help_info = """
Command Line Options to run this script in order to generate CSVs of time elapsed for tests and to compare them

'python compare_elapsed_time.py gtest_log <path to gtest log file>' 
    Creates the csv in the directory of the log

'python compare_elapsed_time.py compare <path to csv or gtest log> <branch to compare>' 
    Compares the results from a previous run of Github actions by downloading the artifact csv of the given branch
"""

def convert_log_to_csv(gtest_log_path):
    test_groups = []
    test_names = []
    test_times = []
    with open(gtest_log_path, 'r') as f:
        for i in range(4):
            next(f)
        for line in f:
            if line[0] != '[':
                continue
            if 'OK' in line or 'FAIL' in line:
                test_arr = line.split('] ')[1].split("(")
                test_name = test_arr[0].strip()
                test_name = test_name.split('.')
                test_group = test_name[0]
                test_name = test_name[-1]
                test_time = float(test_arr[1].split(" ms")[0])
            elif 'test suites' in line:
                test_group = "Total"
                test_name = "Total"
                test_time = float(line.split("(")[1].split(" ms")[0])
            elif 'total' in line:
                test_arr = line.split('from ')[1].split("(")
                test_group = test_arr[0].split()[0]
                test_name = "Total"
                test_time = float(test_arr[1].split(" ms")[0])
            else:
                continue
            test_groups.append(test_group)
            test_names.append(test_name)
            test_times.append(test_time)

    test_df = pd.DataFrame({"Test Group": test_groups, "Test Name": test_names, "Test Times [ms]": test_times})
    out_path = gtest_log_path.parent / "gtest_elapsed_times.csv"
    test_df.to_csv(out_path)
    print(f"Output csv: {out_path}")
    return test_df

def get_workflow_artifact_branch(base_branch):
    base_branch = 'patch'
    access_token = os.getenv("GITHUB_TOKEN")

    headers = {
        'Accept': 'application/vnd.github+json',
        'Authorization': f'Bearer {access_token}',
        'X-GitHub-Api-Version': '2022-11-28',
    }

    response = requests.get('https://api.github.com/repos/NREL/ssc/actions/artifacts', headers=headers)

    print(response)

    artifacts = response.json()['artifacts']

    artifacts = [a for a in artifacts if a['workflow_run']['head_branch'] == base_branch]

    if sys.platform == 'linux':
        platform = "Linux"
    elif sys.platform == 'win32':
        platform = "Windows"
    elif sys.platform == 'darwin':
        platform = "Mac"
    else:
        raise RuntimeError(f"Unrecognized platform {sys.platform}")
    

if __name__ == "__main__":

    if len(sys.argv) == 1:
        raise RuntimeError("Options are 'gtest_log' or 'compare'. Use 'help' to see details")

    if sys.argv[1] == "help":
        print(help_info)
        exit()
    elif sys.argv[1] == "gtest_log":
        if len(sys.argv) < 3:
            raise RuntimeError("Provide path to gtest log file")
        gtest_log_path = Path(sys.argv[2])
        convert_log_to_csv(gtest_log_path)
    elif sys.argv[1] == "compare":
        if len(sys.argv) < 4:
            raise RuntimeError("Provide path to csv or gtest log file, and the name of the branch for comparison")
        filename = Path(sys.argv[2])
        base_branch = sys.argv[3]
        if Path(filename).suffix != ".csv":
            test_df = convert_log_to_csv(filename)
            base_test_df = get_workflow_artifact_branch(base_branch)
            #compare
    else:
        raise RuntimeError("Options are 'gtest_log' or 'compare'. Use 'help' to see details")
 
        

