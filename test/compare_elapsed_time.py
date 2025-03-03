import pandas as pd
from pathlib import Path
import requests
import sys
import os
import zipfile
import io
from platform import processor

help_info = """
Command Line Options to run this script in order to generate CSVs of time elapsed for tests and to compare them

'python compare_elapsed_time.py gtest_log <path to gtest log file>' 
    Creates the csv in the directory of the log

'python compare_elapsed_time.py compare <path to csv or gtest log> <branch to compare>' 
    Compares the results from a previous run of Github actions by downloading the artifact csv of the given branch
"""
access_token = os.getenv("GH_TOKEN")

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
    test_df.to_csv(out_path, index=False)
    print(f"Output csv: {out_path}")
    return test_df

def get_workflow_artifact_branch(base_branch):
    headers = {
        'Accept': 'application/vnd.github+json',
        'Authorization': f'Bearer {access_token}',
        'X-GitHub-Api-Version': '2022-11-28',
    }

    response = requests.get('https://api.github.com/repos/NREL/ssc/actions/artifacts', headers=headers)

    if response.status_code != 200:
        print(response.json())
        raise Exception("Failed to Get Workflow Artifacts List")

    artifacts = response.json()['artifacts']

    artifacts = [a for a in artifacts if a['workflow_run']['head_branch'] == base_branch]

    platform = None
    if sys.platform == 'linux':
        platform = "Linux"
    elif sys.platform == 'win32':
        platform = "Windows"
    elif sys.platform == 'darwin':
        proc = processor()
        if "x86_64" in proc:
            platform = "Mac Intel"
        elif "arm" in proc:
            platform = "Mac Arm"
    else:
        raise RuntimeError(f"Unrecognized platform {sys.platform}")
    
    artifacts = [a for a in artifacts if (platform in a['name']) and ("Test Time Elapsed" in a['name'])]

    headers = {
    'Accept': 'application/vnd.github+json',
    'Authorization': f'Bearer {access_token}',
    'X-GitHub-Api-Version': '2022-11-28',
    }

    response = requests.get(artifacts[0]['archive_download_url'], headers=headers)

    z = zipfile.ZipFile(io.BytesIO(response.content)) 
    file_dir = Path(__file__).parent
    z.extractall(file_dir)
    test_df_base = pd.read_csv(file_dir / "gtest_elapsed_times.csv")
    os.remove(file_dir / "gtest_elapsed_times.csv")
    return test_df_base
    
def get_feature_branch():
    workflow_id = os.getenv("WORKFLOW_ID")
    if workflow_id is None:
        raise Exception("Environment variable 'workflow_id' not defined")

    headers = {
    'Accept': 'application/vnd.github+json',
    'Authorization': f'Bearer {access_token}',
    'X-GitHub-Api-Version': '2022-11-28',
    }

    response = requests.get(f'https://api.github.com/repos/NREL/ssc/actions/runs/{workflow_id}', headers=headers)
    if response.status_code != 200:
        print(response.json())
        raise Exception(f"Failed to Get Feature Branch from Workflow ID {workflow_id}")
    
    return response.json()['head_branch']


def compare_time_elapsed(new_test_df, base_test_df, default_branch):
    feature_branch = get_feature_branch()
    compare_df = new_test_df.merge(base_test_df, how='outer', suffixes=[f" {feature_branch}", f" {default_branch}"], on=['Test Group', 'Test Name'])
    print(compare_df)
    compare_df.to_csv(Path(__file__).parent / "compare_times.csv", index=False)


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
        base_test_df = get_workflow_artifact_branch(base_branch)
        if Path(filename).suffix != ".csv":
            test_df = convert_log_to_csv(filename)
        else:
            test_df = pd.read_csv(filename)
        compare_time_elapsed(test_df, base_test_df, default_branch=base_branch)
    else:
        raise RuntimeError("Options are 'gtest_log' or 'compare'. Use 'help' to see details")
 
        
