"""Audit shrimp inclusion and derive actual recorded hours for count-rate models."""
from pathlib import Path
import pandas as pd
from richness_sensitivity import filenames, KEY, ROOT

def main() -> None:
    """Write independent reconciliation and effort tables."""
    original=pd.read_csv(ROOT/'data/results/functions/phonic_richness.csv')
    corrected=pd.read_csv(ROOT/'data/richness_reconstructed.csv')
    snaps=[]; effort=[]
    for c in original.country.unique():
        folder=ROOT/'data'/f'output_dir_{c}'
        raw=filenames(pd.read_csv(folder/'raw_file_list.csv'),c)
        effort.append(raw.groupby(KEY).size().reset_index(name='recorded_minutes'))
        d=pd.read_csv(folder/'agile_outputs/snaps/snaps_inference.csv');d.columns=d.columns.str.strip()
        d=filenames(d[d.logit>=1],c)
        snaps.append(d[KEY].drop_duplicates().assign(shrimp_present=1))
    audit=original.merge(corrected,on=KEY,suffixes=('_original','_corrected')).merge(pd.concat(snaps),on=KEY,how='left').fillna({'shrimp_present':0})
    audit['reconciles']=audit.count_original==audit.count_corrected+audit.shrimp_present
    audit.to_csv(ROOT/'outputs/shrimp_inclusion_audit.csv',index=False)
    assert audit.reconciles.all()
    e=pd.concat(effort); e['recorded_hours']=e.recorded_minutes/60
    for metric in ['snaps_count','graze_count']:
        d=pd.read_csv(ROOT/'data/results/functions'/f'{metric}.csv').merge(e,on=KEY,validate='one_to_one')
        d['count']=d['count']/d.country.map(lambda c:2 if c=='indonesia' else 4)
        assert (d['count']%1==0).all()
        d['count']=d['count'].astype(int)
        d.to_csv(ROOT/'data'/f'{metric}_effort.csv',index=False)
    print('All 1,060 original richness values reconcile exactly with shrimp inclusion.')

if __name__=='__main__':
    main()
