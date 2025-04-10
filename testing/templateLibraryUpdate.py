import xml.etree.ElementTree as ET
import subprocess
import os
import argparse
from urllib.parse import urlparse
import re

def version_to_tag(version):
    return version.replace(".", "-")

def update_metadata(file_path, new_version):
    tree = ET.parse(file_path)
    root = tree.getroot()

    # Check if template library exists
    for lib in root.findall("onlineLibrary"):
        name = lib.attrib["name"]
        print(f"  ↪ Found template library: {name}")

    tree.write(file_path, encoding="utf-8", xml_declaration=True)
    return tree, root


def run_console_command(args):
    result = subprocess.run(args, capture_output=True, text=True)
    print(result.stdout)
    if result.returncode != 0:
        print("❌ Console error:", result.stderr)
        return False
    return True

import urllib.request
import tempfile
import zipfile

import zipfile
import tempfile
import urllib.request
import subprocess
import os
import re
from urllib.parse import urlparse

def list_scenarios(console_path, lib_path, results_only=False):
    result = subprocess.run(
        [console_path, "--list", "--scenarios", f"--lib={lib_path}"],
        capture_output=True,
        text=True
    )

    if result.returncode != 0:
        print(f"❌ Failed to list scenarios: {result.stderr}")
        return []

    lines = result.stdout.splitlines()
    if len(lines) < 2:
        print("⚠️ No scenarios listed.")
        return []

    header = lines[0]

    # Get fixed column slices by header name
    id_start = header.index("Id")
    is_result_start = header.index("IsResult")

    # Estimate column widths by jumping to the next header or EOL
    is_result_end = header.index("IsReadOnly") if "IsReadOnly" in header else None
    id_end = header.index("ProjectId") if "ProjectId" in header else is_result_start

    scenario_ids = []

    for line in lines[1:]:
        id_str = line[id_start:id_end].strip()
        is_result_str = line[is_result_start:is_result_end].strip().lower()

        if not id_str.isdigit():
            continue

        if results_only:
            if is_result_str == "yes":
                scenario_ids.append(id_str)
        else:
            if is_result_str == "no":
                scenario_ids.append(id_str)

    label = "Result" if results_only else "Non-result"
    print(f"📋 {label} scenario IDs: {', '.join(scenario_ids)}")
    return scenario_ids

def delete_scenarios(console_path, lib_path, scenario_ids):
    if not scenario_ids:
        print("⚠️ No scenarios to delete.")
        return

    for sid in scenario_ids:
        print(f"🗑️ Deleting scenario ID: {sid}")
        result = subprocess.run(
            [console_path, "--delete", "--scenario",  f"--sid={sid}", f"--lib={lib_path}", "--force"],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            print(f"❌ Failed to delete scenario {sid}: {result.stderr.strip()}")
        else:
            print(f"✅ Deleted scenario {sid}")


def update_library(console_path, lib_path):
    result = subprocess.run(
        [console_path, "--update",  f"--lib={lib_path}"],
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print(f"❌ Failed to update library: {result.stderr}")
        return []

    # Parse scenario IDs using regex
    sids = re.findall(r"\bID: (\d+)", result.stdout)
    print("✅ Library successfully updated.")
    return sids


 
def run_libraries(root, console_path, temp_dir=None):
    for lib in root.findall("onlineLibrary"):
        if temp_dir is None:
            temp_dir = tempfile.gettempdir()
        else:
            os.makedirs(temp_dir, exist_ok=True)

        lib_name = lib.attrib["name"]
        lib_url = lib.attrib["libraryLocation"]
        parsed = urlparse(lib_url)
        filename = os.path.basename(parsed.path)

        print(f"⬇️ Downloading: {filename}")
        try:
            zip_file_path = os.path.join(temp_dir, filename)
            urllib.request.urlretrieve(lib_url, zip_file_path)
            print(f"📥 Saved to: {zip_file_path}")
        except Exception as e:
            print(f"❌ Failed to download {filename}: {e}")
            continue

        # Unzip the .ssimbak
        extract_folder = os.path.join(temp_dir, f"{lib_name}_unzipped")
        os.makedirs(extract_folder, exist_ok=True)

        try:
            print(f"🗜️ Unzipping: {zip_file_path} → {extract_folder}")
            with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
                zip_ref.extractall(extract_folder)
        except zipfile.BadZipFile:
            print(f"❌ Invalid ZIP archive: {filename}")
            continue

        # Find the .ssim
        lib_path = next(
            (os.path.join(extract_folder, f) for f in os.listdir(extract_folder) if f.endswith(".ssim")),
            None
        )
        if not lib_path:
            print(f"❌ No .ssim found in {extract_folder}")
            continue

        ## update the library
        update_library(console_path, lib_path)

        ## delete old results scenarios
        result_sids = list_scenarios(console_path, lib_path, results_only=True)
        delete_scenarios(console_path, lib_path, result_sids)

        # List scenario IDs
        sids = list_scenarios(console_path, lib_path, results_only=False)
        if not sids:
            print(f"⚠️  No scenarios found in {lib_path}")
            continue

        # Run only the listed scenario IDs
        sid_str = ",".join(sids)
        print(f"▶️ Running scenarios {sid_str} in: {lib_path}")
        result = subprocess.run(
            [console_path, "--run", f"--lib={lib_path}", f"--sids={sid_str}" ],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            print(f"❌ Scenario run failed:\n{result.stderr}")
        else:
            print(f"✅ Scenario run completed:\n{result.stdout}")



def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("meta", help="Path to meta-data.xml")
    parser.add_argument("--version", required=True, help="New package version (e.g., 4.3.7)")
    parser.add_argument("--console", default="SyncroSim.Console.exe", help="Path to SyncroSim console")
    parser.add_argument("--tempdir", default=None, help="Temporary directory")
    args = parser.parse_args()

    tree, root = update_metadata(args.meta, args.version)
    run_libraries(root, args.console, args.tempdir)

if __name__ == "__main__":
    main()
