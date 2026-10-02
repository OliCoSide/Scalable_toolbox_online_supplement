# A scalable toolbox for exposing indirect discrimination in insurance rates — online supplement

Online supplement for the CAS Forum article by
[Olivier Côté](https://orcid.org/0009-0000-5632-3472),
[Marie-Pier Côté](https://orcid.org/0000-0003-0383-1689), and
[Arthur Charpentier](https://orcid.org/0000-0003-3654-6286)
(31 March 2026).

- **Paper:** [CAS Forum](https://forum.casact.org/article/163838-a-scalable-toolbox-for-exposing-indirect-discrimination-in-insurance-rates)
- **PDF:** [casact.org](https://www.casact.org/sites/default/files/2026-03/Scalable_Toolbox_for_Exposing_Indirect_Discrimination_Research_Paper.pdf)
- **Rendered supplement:** <https://olicoside.github.io/Scalable_toolbox_online_supplement/>

The chapters measure a pricing structure against three fairness dimensions — actuarial fairness, solidarity, and causality — using a five-premium spectrum (best-estimate, unaware, aware, hyperaware, corrective), policyholder-level dollar metrics, and a partition of the portfolio.

| Chapter | What a pricing actuary uses it for |
| --- | --- |
| `ebook/1_simul_dataset.qmd` | Three simulated scenarios and the theoretical premium spectrum |
| `ebook/2_training_spectrum.qmd` | How the five premiums are estimated |
| `ebook/3_local.qmd` | Proxy vulnerability and other local dollar metrics |
| `ebook/4_dimensions.qmd` | Disparities along the three fairness dimensions |
| `ebook/5_partitioning.qmd` | Segments where unfairness concentrates |
| `ebook/6_integrated_framework.qmd` | One monitoring workbook per scenario |

`docs/` is the site served by GitHub Pages. Estimation helpers sit beside the chapters: `ebook/___lgb_*.R`, `ebook/___train_evtree_scenario.R`, `ebook/___evtree_experiment.R`, and `ebook/___opt_transp.py`.

## Rendering

```sh
cd ebook
quarto render
```

Requires R (tidyverse, jsonlite, lightgbm, evtree, reticulate, latex2exp, kableExtra, DT, openxlsx)
and a Python environment with `equipy`; set its path in `ebook/python_env_path.txt`.
Simulation and prediction caches (`ebook/preds/`, `ebook/simuls/`, `ebook/transported/`)
are not tracked: a fresh clone recomputes them on first render. The disparity workbooks
under `ebook/tables/` and `docs/tables/` are tracked so the download buttons work on the live site.
