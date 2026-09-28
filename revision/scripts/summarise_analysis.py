"""Summarise sensitivity models, site-level age checks and source reconciliation."""
from pathlib import Path
import itertools
import json
import numpy as np
import pandas as pd
from scipy.stats import spearmanr
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'outputs'

def main() -> None:
    frames=[pd.read_csv(p) for p in sorted((OUT/'analysis').glob('*_coefficients.csv'))]
    combined=pd.concat(frames,ignore_index=True)
    combined['change_percent']=100*np.expm1(combined.estimate)
    combined['lower_percent']=100*np.expm1(combined.lower95)
    combined['upper_percent']=100*np.expm1(combined.upper95)
    combined.to_csv(OUT/'all_model_coefficients.csv',index=False)
    ages=pd.read_csv(ROOT/'data/results/functions/sites_age.csv')
    results=[]
    for metric in ['phonic_richness','settlement_cuescape','graze_count','snaps_count']:
        if metric=='phonic_richness': d=pd.read_csv(ROOT/'data/richness_reconstructed.csv')
        elif metric in ['graze_count','snaps_count']:
            d=pd.read_csv(ROOT/'data'/f'{metric}_effort.csv');d['count']/=d.recorded_hours
        else:
            d=pd.read_csv(ROOT/'data/results/functions'/f'{metric}.csv');d['count']/=(d.max_poss_count/720)
        site=d.groupby(['country','site','treatment'],as_index=False)['count'].mean().merge(ages,on=['country','site'])
        site=site[(site.country=='indonesia')&(site.treatment=='restored')]
        rho=spearmanr(site.age,site['count']).statistic
        null=[spearmanr(site.age,perm).statistic for perm in itertools.permutations(site['count'])]
        p=np.mean(np.abs(null)>=abs(rho)-1e-10)
        results.append(dict(metric=metric,sites=len(site),rho=rho,exact_two_sided_p=p))
    pd.DataFrame(results).to_csv(OUT/'indonesia_age_site_permutation.csv',index=False)
    meta=pd.read_csv(ROOT/'data/site_metadata.csv').dropna(subset=['reef_stars'])
    design=pd.get_dummies(meta[['country','habitat']],drop_first=True,dtype=float)
    design['intercept']=1.;design['stars100']=meta.reef_stars/100
    report={'stars_design_columns':design.columns.tolist(),'stars_design_rank':int(np.linalg.matrix_rank(design)),
            'stars_design_ncolumns':len(design.columns),'matched_restored_sites':len(meta),
            'mexico_label_correction':'The 35-month Mexican restoration site is R1; R2 in the supplied Table S1 was corrected as a transcription error.'}
    (OUT/'covariate_design_audit.json').write_text(json.dumps(report,indent=2))
    print(pd.DataFrame(results).to_string(index=False)); print(report)

if __name__=='__main__': main()
