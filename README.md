# A scalable toolbox for exposing indirect discrimination in insurance rates — online supplement

Code and reproducible examples for the CAS working paper by
[Olivier Côté](https://orcid.org/0009-0000-5632-3472),
[Marie-Pier Côté](https://orcid.org/0000-0003-0383-1689), and
[Arthur Charpentier](https://orcid.org/0000-0003-3654-6286).

- **Read the supplement:** <https://olicoside.github.io/Scalable_toolbox_online_supplement/>
- **Read the paper:** [CAS working paper (PDF)](https://www.casact.org/sites/default/files/2025-10/_A_Scalable_toolbox_working_paper.pdf)

## What is here

| Path | Contents |
| --- | --- |
| `ebook/` | Quarto book source: chapters `index.qmd`, `1_…` to `6_…`, `9_references.qmd` |
| `ebook/___*.R`, `ebook/___opt_transp.py` | Training scripts (LightGBM, evtree, optimal transport) sourced by the chapters |
| `ebook/extras/` | Sidebar, styles, and commit-history widget |
| `docs/` | Rendered site, served by GitHub Pages |

## Rendering

```sh
cd ebook
quarto render
```

Requires R (tidyverse, jsonlite, lightgbm, evtree, reticulate, latex2exp, kableExtra, DT)
and a Python environment with `equipy`; set its path in `ebook/python_env_path.txt`.
Simulation and prediction caches (`ebook/preds/`, `ebook/simuls/`, `ebook/transported/`)
are not tracked: a fresh clone recomputes them on first render, which takes a while.
