import xml.etree.ElementTree as ET
import subprocess
import argparse
import os

def reinstall_package_from_folder(console_path, metadata_path, folder_path):
    # Step 1: Parse metadata.xml to find package name
    try:
        tree = ET.parse(metadata_path)
        root = tree.getroot()
        package_name = root.attrib["name"]
        print(f"📦 Package to reinstall: {package_name}")
    except Exception as e:
        print(f"❌ Failed to parse metadata: {e}")
        return

    # Step 2: Uninstall the package
    print(f"🧹 Uninstalling package: {package_name}")
    uninstall_cmd = [console_path, f"--removeall={package_name}"]
    result = subprocess.run(uninstall_cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"⚠️ Uninstall failed or package not found: {result.stderr.strip()}")
    else:
        print("✅ Package uninstalled successfully.")

    # Step 3: Install the package from folder
    print(f"📂 Installing package from folder")
    xinstall_cmd = [console_path, f"--xinstall={folder_path}", "--force"]
    result = subprocess.run(xinstall_cmd, capture_output=True, text=True)
    if result.returncode != 0:
        print(f"❌ Install from folder failed: {result.stderr.strip()}")
    else:
        print("✅ Package installed successfully from folder.")



def main():
    parser = argparse.ArgumentParser(description="SyncroSim Package Utilities")
    
    parser.add_argument("--meta", required=True, help="Path to meta-data.xml")
    parser.add_argument("--packagemanager", default="SyncroSim.PackageManager.exe", help="Path to SyncroSim Package Manager")
    parser.add_argument("--reinstall", action="store_true", help="Reinstall package using metadata and --folder path")
    parser.add_argument("--folder", help="Folder path to use with --reinstall")

    args = parser.parse_args()

    if args.reinstall:
        if not args.folder:
            print("❌ You must provide --folder when using --reinstall")
            return
        reinstall_package_from_folder(args.packagemanager, args.meta, args.folder)



if __name__ == "__main__":
    main()
