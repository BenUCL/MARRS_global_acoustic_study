import zipfile
import os
import pandas as pd
from pathlib import Path
import shutil
import xml.etree.ElementTree as ET

BASE_DIR = os.getenv("BASE_DIR")
if not BASE_DIR:
  raise ValueError("BASE_DIR environment variable is not set.")

KMZ_PATH = os.path.join(BASE_DIR, "marrs_acoustics/data/maps/kenya_sites.kmz")
CSV_PATH = os.path.join(BASE_DIR, "marrs_acoustics/data/maps/kenya_sites.csv")

def kmz_to_csv_et(kmz_path: str, csv_path: str) -> None:
  """
  Converts a KMZ file to CSV by extracting KML placemark data using ElementTree.
  
  Args:
    kmz_path: Path to the KMZ file.
    csv_path: Path to save the output CSV file.
  """
  kmz_path = Path(kmz_path)
  csv_path = Path(csv_path)
  
  # Create a temporary folder to extract files
  temp_dir = kmz_path.parent / "temp_kmz_extract"
  temp_dir.mkdir(parents=True, exist_ok=True)
  
  # Extract KMZ (ZIP) contents
  with zipfile.ZipFile(kmz_path, "r") as kmz:
    kmz.extractall(temp_dir)
  
  # Find the KML file
  kml_file = next(temp_dir.glob("*.kml"), None)
  if not kml_file:
    print("No KML file found inside the KMZ.")
    return
  
  # Define KML namespace and parse the KML file
  ns = {"kml": "http://www.opengis.net/kml/2.2"}
  tree = ET.parse(kml_file)
  root = tree.getroot()
  
  placemarks = []
  for placemark in root.findall(".//kml:Placemark", ns):
    name_elem = placemark.find("kml:name", ns)
    name = name_elem.text if name_elem is not None else ""
    point = placemark.find("kml:Point", ns)
    if point is not None:
      coords_elem = point.find("kml:coordinates", ns)
      if coords_elem is not None:
        coords_text = coords_elem.text.strip()
        if coords_text:
          # Coordinates format: lon,lat,alt (altitude is optional)
          lon, lat, *_ = coords_text.split(",")
          placemarks.append({
              "Name": name,
              "Latitude": lat.strip(),
              "Longitude": lon.strip()
          })
  
  if placemarks:
    df = pd.DataFrame(placemarks)
    df.to_csv(csv_path, index=False)
    print(f"Conversion complete! CSV saved at: {csv_path}")
  else:
    print("No placemarks found in the KML file.")
  
  shutil.rmtree(temp_dir)

if __name__ == "__main__":
  kmz_to_csv_et(KMZ_PATH, CSV_PATH)
