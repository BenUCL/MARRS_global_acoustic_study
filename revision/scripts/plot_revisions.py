"""Update model values while preserving the submitted Figures 4–5 design.

Plot conventions come from manuscript_docs/figures.ipynb, cells 40–41.
The submitted manuscript is authoritative for the final PowerPoint arrangement,
'Ecosystem process' label and shared legend. Original inputs are read-only.
"""
from pathlib import Path
from statistics import NormalDist
import json

import matplotlib
matplotlib.use('Agg')
import matplotlib.lines as mlines
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
ECO_MAPPING = {
    'snaps_count': 'Snapping\nshrimp activity',
    'graze_count': 'Parrotfish\ngrazing',
    'phonic_richness': 'Phonic\nrichness',
    'settlement_cuescape': 'Fish recruitment\ncuescape',
}
TREATMENT_INFO = {
    'treatmenthealthy': {'label': 'Healthy', 'offset': .25, 'color': '#1b7837'},
    'treatmentnewly_restored': {'label': 'Early-stage\nrestoration', 'offset': -.25, 'color': '#c2a5cf'},
    'treatmentrestored': {'label': 'Mid-stage\nrestoration', 'offset': 0., 'color': '#acd39e'},
}
COUNTRY_LIMITS = {
    'australia': (-200, 3000), 'indonesia': (-200, 8000),
    'kenya': (-200, 200000), 'maldives': (-150, 20000),
    'mexico': (-150, 30000),
}
Z75 = NormalDist().inv_cdf(.875)


def load(metric: str, country: str | None = None) -> pd.DataFrame:
    """Load fitted log coefficients and both central Wald intervals."""
    if metric == 'settlement_cuescape' and country:
        data = pd.read_csv(ROOT / 'data/results/functions/stats/summary_outputs/country_combined_results.csv')
        data = data[(data.eco_function == metric) & (data.country == country)
                    & (data.scale == 'Log-Scale')].copy()
        data = data.rename(columns={'Estimate': 'estimate', 'Lower_95': 'lower95',
                                    'Upper_95': 'upper95', 'Lower_75': 'lower75',
                                    'Upper_75': 'upper75', 'p_value': 'p'})
    else:
        scenario = {'settlement_cuescape': 'baseline', 'phonic_richness': 'corrected_baseline',
                    'graze_count': 'actual_effort', 'snaps_count': 'actual_effort'}[metric]
        if country:
            scenario = ('nb_' if metric == 'phonic_richness' else 'actual_effort_') + country
        data = pd.read_csv(ROOT / 'outputs/analysis' / f'{metric}__{scenario}_coefficients.csv')
        data['lower75'] = data.estimate - Z75 * data.se
        data['upper75'] = data.estimate + Z75 * data.se
    data = data[data.term.isin(TREATMENT_INFO)].copy()
    data['eco_function'] = metric
    data['country'] = country or 'global'
    # Transform endpoints, not the standard error: percentage intervals are asymmetric.
    for column in ['estimate', 'lower95', 'upper95', 'lower75', 'upper75']:
        data[column + '_percent'] = 100 * np.expm1(data[column])
    assert (data.lower95 <= data.lower75).all()
    assert (data.upper75 <= data.upper95).all()
    return data


def counts(metric: str, country: str | None) -> dict[str, int]:
    """Count the site-days actually used by the corresponding fitted indicator."""
    data = pd.read_csv(ROOT / 'data/results/functions' / f'{metric}.csv')
    if metric == 'graze_count':
        data = data[data.country != 'kenya']
    if country:
        data = data[data.country == country]
    return data.groupby('treatment').size().to_dict()


def legend_handles(scale: float) -> list:
    """Retain the submitted habitat order and original palette."""
    result = []
    for term in ['treatmentnewly_restored', 'treatmentrestored', 'treatmenthealthy']:
        info = TREATMENT_INFO[term]
        result.append(mlines.Line2D([], [], color=info['color'], marker='o',
                                   markersize=6 * scale, lw=3 * scale, label=info['label']))
    result.append(mlines.Line2D([], [], color='black', ls='--', lw=scale, label='Degraded'))
    return result


def panel(ax, country: str | None, scale: float = 1., labels: bool = True,
          xlabel: bool = True) -> pd.DataFrame:
    """Draw the original grouped ecosystem-process plot with revised values."""
    metrics = [m for m in ECO_MAPPING if not (country == 'kenya' and m == 'graze_count')]
    frames = []
    for ybase, metric in enumerate(metrics):
        data = load(metric, country)
        n = counts(metric, country)
        for row in data.itertuples():
            info = TREATMENT_INFO[row.term]
            y = ybase + info['offset']
            ax.plot([row.lower95_percent, row.upper95_percent], [y, y],
                    color=info['color'], lw=3 * scale, solid_capstyle='round')
            ax.plot([row.lower75_percent, row.upper75_percent], [y, y],
                    color=info['color'], lw=5 * scale, solid_capstyle='round')
            ax.plot(row.estimate_percent, y, 'o', color=info['color'], markersize=7 * scale)
            sample_n = n[row.term.removeprefix('treatment')]
            ax.text(1.025, y, f'(N={sample_n})', transform=ax.get_yaxis_transform(),
                    ha='left', va='center', fontsize=10 * scale, clip_on=False)
        data['N'] = data.term.map(lambda term: n[term.removeprefix('treatment')])
        frames.append(data)
    ax.axvline(0, ls='--', color='black', lw=scale)
    ax.set_xscale('symlog', linthresh=1)
    ax.set_xlim(*(COUNTRY_LIMITS[country] if country else (-150, 1200)))
    ax.set_ylim(-.425, len(metrics) - .575)
    ax.set_yticks(range(len(metrics)), [ECO_MAPPING[m] for m in metrics] if labels else [])
    ax.set_ylabel('Ecosystem process' if labels else '', fontweight='bold', fontsize=12 * scale)
    ax.set_xlabel('Increase over degraded (%)' if xlabel else '', fontweight='bold', fontsize=12 * scale)
    ax.spines[['top', 'right']].set_visible(False)
    for spine in ['left', 'bottom']:
        ax.spines[spine].set_color('black')
        ax.spines[spine].set_linewidth(1.5 * scale)
    ax.tick_params(axis='both', which='both', direction='in', length=6 * scale,
                   width=1.5 * scale, colors='black', labelsize=11 * scale)
    ax.grid(False)
    if country:
        ax.set_title(country.capitalize(), fontweight='bold', fontsize=12 * scale, pad=5 * scale)
    # Retain the original limits unless a new interval would be hidden.
    combined = pd.concat(frames, ignore_index=True)
    low, high = ax.get_xlim()
    if combined.lower95_percent.min() < low or combined.upper95_percent.max() > high:
        ax.set_xlim(min(low, combined.lower95_percent.min() * 1.05),
                    max(high, combined.upper95_percent.max() * 1.05))
    return combined


def save(fig, name: str) -> None:
    """Overwrite the rejected redesign files with original-style replacements."""
    directory = ROOT / 'outputs/figures'
    for extension in ['png', 'pdf']:
        fig.savefig(directory / f'{name}.{extension}', dpi=600, transparent=True)
    plt.close(fig)


def main() -> None:
    """Recreate the submitted single-panel and five-country figure arrangements."""
    (ROOT / 'outputs/figures').mkdir(exist_ok=True)
    plt.rcParams.update({'font.family': 'sans-serif', 'font.sans-serif': ['DejaVu Sans'],
                         'axes.grid': False, 'axes.labelcolor': 'black', 'text.color': '#262626'})
    fig = plt.figure(figsize=(9, 5.34))
    ax = fig.add_axes([.20, .12, .505, .842])
    tables = [panel(ax, None)]
    fig.legend(handles=legend_handles(1), title='Habitat type',
               title_fontproperties={'weight': 'bold', 'size': 12},
               loc='center left', bbox_to_anchor=(.805, .52), fontsize=11,
               labelspacing=1.5, handletextpad=1, frameon=False)
    save(fig, 'figure4_revised')

    fig = plt.figure(figsize=(9, 10.58))
    positions = {'australia': (.16, .705), 'indonesia': (.595, .705),
                 'maldives': (.16, .365), 'mexico': (.595, .365), 'kenya': (.16, .05)}
    for country, (left, bottom) in positions.items():
        ax = fig.add_axes([left, bottom, .33, .277])
        tables.append(panel(ax, country, scale=.70, labels=left == .16,
                            xlabel=country in ['mexico', 'kenya']))
    fig.legend(handles=legend_handles(.70), title='Habitat type',
               title_fontproperties={'weight': 'bold', 'size': 10},
               loc='center', bbox_to_anchor=(.765, .165), fontsize=9,
               labelspacing=1.5, handletextpad=1, frameon=False)
    save(fig, 'figure5_revised')
    pd.concat(tables, ignore_index=True).to_csv(ROOT / 'outputs/figures/plotted_values.csv', index=False)
    provenance = {'source': 'manuscript_docs/figures.ipynb, cells 40–41; submitted Figures 4–5',
                  'intervals': [75, 95], 'percentage_transform': '100 * (exp(log_coefficient) - 1)',
                  'palette': {k: v['color'] for k, v in TREATMENT_INFO.items()},
                  'country_layout': [['Australia', 'Indonesia'], ['Maldives', 'Mexico'], ['Kenya', 'Habitat type legend']],
                  'note': 'N counts follow fitted data; grazing excludes Kenya. Original model results are not refitted.'}
    (ROOT / 'outputs/figures/style_verification.json').write_text(json.dumps(provenance, indent=2))

if __name__ == '__main__':
    main()
