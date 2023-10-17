import argparse
from pathlib import Path
import re
import numpy as np
import pandas as pd
import sklearn.metrics as metrics
from sklearn.utils import resample


FILES = [
    "data/CSAW-CC (screen-detected only)_Breast.csv",
    "data/CSAW-CC (screen-detected only)_Episode.csv",
    "data/CSAW-CC_Breast.csv",
    "data/CSAW-CC_Episode.csv",
    "data/CMMD_Breast.csv",
    "data/CMMD_Episode.csv",
    "data/BREAST_Breast.csv",
    "data/BREAST_Episode.csv",
    "data/ADMANI Prospective_Breast.csv",
    "data/ADMANI Prospective_Episode.csv",
]


def main(args):

    seed = args.seed
    n_bootstraps = args.n

    outputs = []
    summary = []
    for f in FILES:
        df = pd.read_csv(args.base_path / f, low_memory=False)
        ds_name, level = f.split('/')[-1].replace('.csv', '').split('_')
        print(ds_name, level)
        
        roc, pr = generate_cis(df.label, df.prediction, seed, n_bootstraps=n_bootstraps)

        outputs.append({'Dataset': ds_name, 'Level': level, 'AUC': 'ROC', 'AI reader': ci_str(*roc)})
        outputs.append({'Dataset': ds_name, 'Level': level, 'AUC': 'PR', 'AI reader': ci_str(*pr)})

        if level == 'Episode':
            summary.append({
                'Dataset': ds_name,
                'Episodes': df.episode_id.unique().shape[0],
                'Normal': df[~df.outcome.isin([1,2,3])].episode_id.unique().shape[0],
                'Benign': df[df.outcome.isin([3])].episode_id.unique().shape[0],
                'Screen-detected cancer': df[df.outcome == 1].episode_id.unique().shape[0],
                'Interval cancer': df[df.outcome == 2].episode_id.unique().shape[0],
            })

    summary = pd.DataFrame(summary).drop_duplicates()
    table = pd.DataFrame(outputs).set_index(['Dataset', 'Level', 'AUC'])

    summary_tbl = summary.melt(id_vars=['Dataset', 'Episodes'], var_name='Labels', value_name="").groupby(['Dataset','Episodes', 'Labels'], sort=True).sum()

    # re-orders the labels
    LABEL_ORDER = ['CSAW-CC (screen-detected only)', 'CSAW-CC', 'CMMD', 'BREAST', 'ADMANI Prospective']
    CATEGORY_ORDER = ['Normal', 'Benign', 'Screen-detected cancer', 'Interval cancer']

    order = []
    for label in LABEL_ORDER:
        for cat in CATEGORY_ORDER:
            for i, (a, b, c) in enumerate(summary_tbl.index.values):
                if (a == label) and (c == cat):
                    order.append((a, b, c))

    summary_tbl = summary_tbl.reindex(order)

    print(table)

    # print latex version for paper
    format_latex_table(table, summary_tbl)


def ci(arr, confidence=0.95):
    # nyukat implementation matches method='nearest' for lower and method='higher' for upper
    m = (1-confidence) / 2
    lower = np.quantile(arr, m, method='nearest')
    upper = np.quantile(arr, 1 - m, method='nearest')
    return lower, upper


def generate_cis(labels, predictions, seed, n_bootstraps=10):
    # adapted from https://github.com/nyukat/mammography_metarepository/blob/master/evaluation/score.py

    rng = np.random.default_rng(seed=seed)
    seeds = rng.integers(0, 2**32 - 1, n_bootstraps)

    roc_auc = metrics.roc_auc_score(labels, predictions)
    precision, recall, _ = metrics.precision_recall_curve(labels, predictions)
    pr_auc = metrics.auc(recall, precision)

    n_samples = len(labels)
    b_roc_auc_list = []
    b_pr_auc_list = []
    for _, random_state in enumerate(seeds):
        boot = resample(list(zip(labels, predictions)), replace=True, n_samples=n_samples, random_state=random_state)
        b_labels, b_predictions = list(zip(*boot))

        if len(list(set(b_labels))) == 1:
            n_bootstraps -= 1
            continue

        b_roc_auc = metrics.roc_auc_score(b_labels, b_predictions)
        b_roc_auc_list.append(b_roc_auc)

        precision, recall, _ = metrics.precision_recall_curve(b_labels, b_predictions)

        b_pr_auc = metrics.auc(recall, precision)
        b_pr_auc_list.append(b_pr_auc)
    
    roc_lower, roc_upper = ci(b_roc_auc_list)
    pr_lower, pr_upper = ci(b_pr_auc_list)

    return (roc_auc, roc_lower, roc_upper), (pr_auc, pr_lower, pr_upper)


def ci_str(m, l, u, decimal_places=3):
    return f"{m:.{decimal_places}f} ({l:.{decimal_places}f}, {u:.{decimal_places}f})"


def format_latex_table(results, summary_tbl):
    # formats to display in paper, read at own risk
    s = results.to_latex(multirow=True, multicolumn=True, header=True)

    lines = s.split('\n')
    lines.pop(2)  # remove AI reader line
    lines_str = '\n'.join(lines[:-1])


    labels = summary_tbl.reset_index().Labels.values[:4]
    datasets = summary_tbl.reset_index().Dataset.values[::4]

    for i, dataset in enumerate(datasets[:]):
        vals = summary_tbl.loc[dataset].values.flatten()
        for j, row in enumerate(lines_str.split('\n')):
            if f"{dataset}}}" in row:
                lines[j] = row.replace(r"& \m", f"& {labels[0]} & {vals[0]} & \m")
                lines[j+1] = re.sub('&([ \t])+&([ \t])+', f"& {labels[1]} & {vals[1]} & & ", lines[j+1])
                lines[j+3] = lines[j+3].replace(r"& \multi", f"& {labels[2]} & {vals[2]} & \multi")
                lines[j+4] = re.sub('&([ \t])+&([ \t])+', f"& {labels[3]} & {vals[3]} & & ", lines[j+4])

    tbl_s = '\n'.join(lines[:-1])
    tbl_s = tbl_s.replace('{llll}', '{llllll}').replace('\cline{1-4}\n', '').replace('\cline{2-4}\n', '\cline{4-6}\n')
    tbl_s = tbl_s.replace(r'Dataset & Level & AUC &', r'\textbf{Dataset} & \textbf{Episodes} & & & \textbf{AUC} &')
    tbl_s = tbl_s.replace(' (screen-detected only)', r'\textsuperscript{1}')

    before = [r'\begin{table}[ht]', '\small', '\centering']
    after = [r'\vspace{5pt}', '\caption{}', '\label{tab:external_validation}', '\end{table}']
    final = '\n'.join(before + tbl_s.split('\n') + after)

    # add meta repository numbers https://arxiv.org/pdf/2108.04800.pdf
    temp_s = final.split('\n')
    ix = 1
    # CSAW-CC screen only
    temp_s[ix+6] = temp_s[ix+6].replace(r' \\', r' & 0.943 (0.931, 0.954) \\')
    temp_s[ix+7] = temp_s[ix+7].replace(r' \\', r' & 0.495 (0.447, 0.543) \\')
    # CMMD
    temp_s[ix+18] = temp_s[ix+18].replace(r' \\', r' & 0.831 (0.815, 0.846) \\')
    temp_s[ix+19] = temp_s[ix+19].replace(r' \\', r' & 0.859 (0.842, 0.875) \\')
    temp_s[ix+21] = temp_s[ix+21][:re.search('ROC.*\\\\', temp_s[ix+21]).start()] + r'ROC & \multicolumn{1}{c}{-} \\'
    temp_s[ix+22] = temp_s[ix+22][:re.search('PR.*\\\\', temp_s[ix+22]).start()] + r'PR & \multicolumn{1}{c}{-} \\'

    nums = [9, 10, 12, 13, 15, 16, 21, 22, 24, 25, 27, 28, 30, 31, 33, 34]
    nums = [n + 1 for n in nums]
    for i in nums:
        temp_s[i] = temp_s[i].replace(r' \\', r' & \multicolumn{1}{c}{-} \\')

    temp_s.insert(-2, r'\textsuperscript{1} screen-detected cancers only}')
    if temp_s[-8] == '\\cline{1-4} \\cline{4-6}':
        temp_s.pop(-8)

    temp_s.insert(3, r'\def\arraystretch{1.075}')

    tbl_s = '\n'.join(temp_s)
    tbl_s = tbl_s.replace('{llllll}', '{lllllll}').replace(r'-6}', r'-7}')
    tbl_s = tbl_s.replace(r'\textbf{Dataset} & \textbf{Episodes} & & & \textbf{AUC} &', r'\textbf{Dataset} & \textbf{Episodes} & & & \textbf{AUC} & \textbf{BRAIx} & \textbf{GMIC}')
    tbl_s = tbl_s.replace(r'\midrule', r'\specialrule{.4pt}{2pt}{0pt}').replace(r'\bottomrule', r'\specialrule{.8pt}{0pt}{2pt}')
    tbl_s = tbl_s.replace(r'\caption{}', r'\caption{Results of testing the AI reader on external datasets, 95\% confidence interval calculated using 2,000 bootstrap replicates. \newline')

    tbl_s = tbl_s.replace(r'\cline{1-4} \cline{4-7}', r'\cline{2-7}')
    tbl_s = tbl_s.replace(r'\multirow[t]', r'\multirow')

    print(tbl_s)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('--base_path', type=Path, default=Path.cwd(), help='base path for input files')
    parser.add_argument('--seed', default=9837452, type=int)
    parser.add_argument('-n', default=2000, type=int, help='number of bootstrap samples')
    args = parser.parse_args()

    main(args)
