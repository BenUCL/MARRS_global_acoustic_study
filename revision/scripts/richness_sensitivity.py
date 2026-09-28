"""Rebuild richness and thin complete Indonesian files to both four-minute phases."""
from pathlib import Path
import json
import pandas as pd
ROOT = Path(__file__).resolve().parents[1]
KEY = ['country', 'site', 'date', 'treatment']
TREATMENTS = {'D': 'degraded', 'H': 'healthy', 'N': 'newly_restored', 'R': 'restored'}

def filenames(frame: pd.DataFrame, country: str) -> pd.DataFrame:
    """Extract local recording dates and scheduled minutes without touching audio."""
    out = frame.copy()
    out.columns = out.columns.str.strip()
    names = out['filename'].str.replace('\\', '/', regex=False).str.rsplit('/').str[-1]
    parts = names.str.extract(r'^[a-z]+_([DHNR]\d+)_(\d{8})_(\d{2})(\d{2})(\d{2})\.')
    if parts.isna().any().any():
        raise ValueError('Unexpected recording filename')
    out['country'] = country
    out['site'] = parts[0]
    out['date'] = parts[1].astype(int)
    out['treatment'] = parts[0].str[0].map(TREATMENTS)
    out['minute'] = parts[2].astype(int)*60+parts[3].astype(int)
    out['basename'] = names
    return out

def main() -> None:
    """Keep both complementary phases and compare reconstructed source counts."""
    baseline = pd.read_csv(ROOT/'data/results/functions/phonic_richness.csv')
    variants = {name: [] for name in ['reconstructed', 'phase0', 'phase2']}
    coverage_rows = []
    for country in sorted(baseline.country.unique()):
        directory = ROOT/'data'/f'output_dir_{country}'
        raw = filenames(pd.read_csv(directory/'raw_file_list.csv'), country)
        assert not raw.basename.duplicated().any()
        detections = []
        for p in sorted(directory.glob('agile_outputs/*/*_inference.csv')):
            if p.parent.name in ['snap', 'snaps']:
                continue
            d = pd.read_csv(p)
            d.columns = d.columns.str.strip()
            d = filenames(d[d.logit >= 1].copy(), country)
            d['sound'] = p.parent.name
            detections.append(d)
        detected = pd.concat(detections, ignore_index=True)
        for variant in variants:
            if country == 'indonesia' and variant != 'reconstructed':
                phase = int(variant[-1])
                r = raw[raw.minute % 4 == phase]
                d = detected[detected.minute % 4 == phase]
                expected = 360
            else:
                r, d = raw, detected
                expected = 720 if country == 'indonesia' else 360
            coverage = r.groupby(KEY).size().reset_index(name='n_files')
            valid = coverage[coverage.n_files >= 0.9*expected].copy()
            # Count only detections from files in the input manifest.
            d = d[d.basename.isin(r.basename)]
            counts = d.groupby(KEY)['sound'].nunique().reset_index(name='count')
            result = valid.merge(counts, on=KEY, how='left').fillna({'count':0})
            result['count'] = result['count'].astype(int)
            variants[variant].append(result[KEY+['count']])
            coverage_rows.append({'country':country,'variant':variant,'valid_days':len(result)})
    summary = {}
    for name, blocks in variants.items():
        data = pd.concat(blocks, ignore_index=True)
        data.to_csv(ROOT/'data'/f'richness_{name}.csv', index=False)
        compared = baseline.merge(data,on=KEY,how='outer',suffixes=('_original','_new'),indicator=True)
        summary[name] = {'rows':len(data),'unmatched':int((compared['_merge']!='both').sum()),
                         'changed_days':int((compared.count_original != compared.count_new).sum()),
                         'mean_change':float((compared.count_new-compared.count_original).mean())}
        compared.to_csv(ROOT/'outputs'/f'richness_audit_{name}.csv',index=False)
    pd.DataFrame(coverage_rows).to_csv(ROOT/'outputs/richness_coverage.csv',index=False)
    (ROOT/'outputs/richness_audit.json').write_text(json.dumps(summary,indent=2))
    print(json.dumps(summary,indent=2))

if __name__ == '__main__':
    main()
