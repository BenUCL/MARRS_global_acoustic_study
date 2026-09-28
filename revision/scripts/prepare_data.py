"""Copy required CSV inputs and derive checked site metadata; never alter sources."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import shutil
import pandas as pd
from docx import Document

ROOT = Path(__file__).resolve().parents[1]

def main() -> None:
    """Copy a bounded input set and record content hashes using relative paths."""
    parser = argparse.ArgumentParser()
    parser.add_argument('--source', type=Path, required=True)
    args = parser.parse_args()
    source = args.source.resolve()
    baseline = ROOT / 'scratch/source_hashes.json'
    baseline.parent.mkdir(exist_ok=True)
    hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest()
              for p in (ROOT / 'docs').iterdir() if p.is_file()}
    if baseline.exists():
        assert json.loads(baseline.read_text()) == hashes, 'Source documents changed since initial snapshot'
    else:
        baseline.write_text(json.dumps(hashes, indent=2))

    files = list((source / 'results/functions').glob('*.csv'))
    files += list((source / 'results/functions/stats/summary_outputs').glob('*.csv'))
    for country in ['australia', 'indonesia', 'kenya', 'maldives', 'mexico']:
        directory = source / f'output_dir_{country}'
        files.append(directory / 'raw_file_list.csv')
        files.extend(p for p in directory.glob('agile_outputs/*/*_inference.csv')
                     if p.name == p.parent.name + '_inference.csv')
    manifest = []
    for p in files:
        relative = p.relative_to(source)
        target = ROOT / 'data' / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        if not target.exists():
            shutil.copy2(p, target)
        digest = hashlib.sha256(p.read_bytes()).hexdigest()
        assert hashlib.sha256(target.read_bytes()).hexdigest() == digest
        manifest.append({'file': str(relative), 'sha256': digest, 'bytes': p.stat().st_size})
    (ROOT / 'outputs/input_manifest.json').write_text(json.dumps(manifest, indent=2))
    rows = []
    for row in Document(ROOT / 'docs/Supplementary 1.docx').tables[0].rows[1:]:
        v = [' '.join(c.text.split()) for c in row.cells]
        def number(text: str):
            match = re.search(r'\d+(?:\.\d+)?', text)
            return float(match.group()) if match else None
        rows.append(dict(country=v[0].lower(), site=v[1], habitat=v[2],
                         reef_stars=number(v[4]), age_months=number(v[5]),
                         coral_cover=number(v[7]) if '%' in v[7] else None,
                         cover_censored='<' in v[7], cover_approximate='Approx' in v[7],
                         fish_biomass=number(v[7]) if 'biomass' in v[7] else None,
                         degradation=v[6], survey=v[7], days_reported=number(v[8])))
    metadata = pd.DataFrame(rows)
    # The 35-month Mexican restoration site is R1 in the analysis data; the supplied
    # supplementary table's R2 label was confirmed as a transcription error.
    mexico_mid = (metadata.country == 'mexico') & (metadata.site == 'R2') & (metadata.age_months == 35)
    if mexico_mid.sum() != 1:
        raise ValueError('Expected one 35-month Mexican R2 metadata row to correct to R1')
    metadata.loc[mexico_mid, 'site'] = 'R1'
    metadata.to_csv(ROOT / 'data/site_metadata.csv', index=False)
    print(f'Copied and verified {len(manifest)} CSV files; extracted {len(metadata)} site records.')

if __name__ == '__main__':
    main()
