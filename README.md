# The Judgment of Princeton 
Exploring national preferences and wine-tasting scores with Bayesian statistics in Julia
The Judgment of Princeton is an exploratory data science project examining how judges rated French and New Jersey wines in a blind tasting held on June 8, 2012, during an American Association of Wine Economists conference at Princeton University.

Inspired by the landmark 1976 Judgment of Paris, the event offers an opportunity to ask a different question from which wine won?: Do wine origin, judge nationality, and wine style help explain how individual judges assign scores?

This repository contains a Pluto.jl notebook that explores the tasting results and fits three Bayesian models using Turing.jl.
Project status: Exploratory analysis and educational research notebook. The results illustrate patterns in one tasting event; they do not establish causal bias or generalize to all French or American wine judges.

Research questions
- How are the judges' scores distributed?
- How do scores compare between red and white wine flights?
- Do French and New Jersey wines receive different ratings?
- Do judges differ in their overall scoring tendencies?
- Are judge origin and wine origin associated with scoring differences, and do those associations vary by wine flight?
Analysis at a glance
Stage	Methods	Purpose
Data acquisition	HTTP.jl, CSV.jl, DataFrames.jl	Download and prepare the published tasting data
Exploratory analysis	Grouped summaries, histograms, box plots	Describe wine scores by flight and wine origin
Model 1	Bayesian judge and wine effects	Estimate variation across individual judges and wines
Model 2	Bayesian additive nationality and flight effects	Compare average scores by judge origin, wine origin, and flight
Model 3	Bayesian two-way interactions	Explore whether the associations change across groups
Visualization	StatsPlots.jl	Inspect prior simulations, posterior distributions, contrasts, and predictions


Dataset
The notebook downloads the semicolon-delimited Wines2012.csv file from Richard McElreath's Statistical Rethinking repository:

View the dataset

The analysis uses the following fields:
Field	Meaning
score	Score awarded to a wine
wine	Wine identifier
judge	Judge identifier
flight	Red or white tasting flight
wine.amer	Indicator of American wine origin
judge.amer	Indicator of American judge origin


Additional numeric indices and binary predictors are constructed inside the notebook for model fitting. The dataset is fetched at runtime, so an internet connection is required unless the data-loading cell is adapted to use a local copy.
Bayesian models
All three models assume that observed scores are normally distributed around a modeled expected score, with residual variation estimated from the data. Parameter estimation uses Turing.jl and NUTS sampling.
Model 1 — Individual judges and wines
The first model estimates an expected score from a judge-specific baseline and a wine-specific contribution:
expected score = judge effect + wine effect
It explores which judges tend to award higher or lower scores while accounting for which wine they tasted. The notebook visualizes posterior distributions for judges and selected pairwise contrasts, including comparisons with John Foy.
Model 2 — Wine origin, judge origin, and flight
The second model estimates an expected score using additive effects for:
- Red versus white wine flight
- French versus American judge classification
- French versus American wine classification
expected score = flight effect + judge-origin effect + wine-origin effect
This model explores overall group-level score differences. It does not contain an interaction between judge origin and wine origin, so its judge-origin contrast alone does not show that judges disproportionately favor wines from their own country.
Model 3 — Nationality and wine-style interactions
The third model includes the same three predictors along with all pairwise interactions:
expected score = intercept
               + wine-origin effect
               + judge-origin effect
               + flight effect
               + wine-origin × judge-origin
               + wine-origin × flight
               + judge-origin × flight
It compares modeled outcomes for eight combinations of wine origin, judge origin, and red/white flight. This is the notebook's most direct attempt to examine whether national preferences depend on the wine being evaluated.

The notebook uses normally distributed priors for score-related parameters and exponential priors for the residual standard deviation. Each model requests 5,000 NUTS samples in the original source.
Findings discussed in the notebook
The notebook's exploratory discussion highlights several patterns:
1. The score distribution centers in the mid-teens, with greater spread among red wines than white wines.
2. American red wines appear to show comparatively high score variability in the descriptive summaries.
3. Individual judges differ in their typical scoring behavior; the first model highlights John Foy as a relatively generous scorer.
4. The second model's narrative reports a French-minus-American judge contrast of approximately −0.65 score points.
5. The third model's discussion suggests that French judges may particularly favor French red wines, while preferences differ across the other combinations.
Interpretation matters: These are findings reported or interpreted in the existing notebook, not independently replicated estimates. The −0.65 contrast indicates a difference in modeled scoring levels in Model 2, not definitive evidence of prejudice or home-country preference. The interaction estimates from Model 3 would need uncertainty intervals, posterior contrasts, and model checks before supporting stronger conclusions.
Running the notebook
Requirements
- Julia
- Pluto.jl
- Internet access to retrieve Wines2012.csv
The repository includes Project.toml and Manifest.toml to document its original package environment. Because the notebook dates from an older Pluto/Turing ecosystem, it may require dependency updates to run under recent Julia versions.
1. Clone the repository
git clone https://github.com/Aeonglacial/Judgement-of-Princeton.git
cd Judgement-of-Princeton
2. Instantiate the environment
From the repository directory:
julia --project=. -e 'using Pkg; Pkg.instantiate()'
3. Launch Pluto
julia --project=. -e 'using Pluto; Pluto.run()'
Open judgement_of_princeton.jl in Pluto's browser interface. Pluto evaluates reactive notebook cells and displays the accompanying visualizations.
Compatibility note: DataFrames is imported directly by the notebook but is not listed as a direct dependency in the checked-in Project.toml. If the environment reports a missing package, add it to the active environment with import Pkg; Pkg.add("DataFrames"). Older Turing and plotting APIs may also need updating. The setup above has not been runtime-verified against the original manifest.

Repository contents
Judgement-of-Princeton/
├── judgement_of_princeton.jl   # Main Pluto analysis notebook
├── Project.toml                # Julia project dependencies
├── Manifest.toml               # Julia dependency lockfile
├── judgement_of_princeton.jl  Pluto.jl .pdf
│                               # Saved PDF notebook export
└── README.md                   # Project documentation
To browse the original exported analysis without running Julia, see the PDF notebook export.
Limitations and opportunities for improvement
This is a small, single-event observational analysis. Several upgrades would strengthen its statistical interpretation and reproducibility:
- Model diagnostics: Report chain convergence, effective sample sizes, divergences, and posterior predictive checks.
- Uncertainty: Summarize credible intervals and probabilities for key nationality-by-wine-origin contrasts.
- Bias versus scoring severity: Separate judges' overall generosity from preferential scoring of domestic wines.
- Ranking logic: Compute flight-specific rankings and wine labels directly from source data rather than assigning display names in a fixed order.
- Reproducibility: Pin a validated Julia version, explicitly declare every directly imported dependency, and seed stochastic analysis where appropriate.
- Model comparison: Evaluate predictive fit and model complexity rather than interpreting increasingly elaborate interactions alone.
- Robustness: Consider wine and judge effects jointly with nationality interactions, and assess whether results are sensitive to the chosen likelihood and priors.
Tools and technologies
Julia · Pluto.jl · Turing.jl · DataFrames.jl · CSV.jl · HTTP.jl · StatsPlots.jl · Distributions.jl
Data attribution
Tasting data: Wines2012.csv, distributed in Richard McElreath's Statistical Rethinking data repository. The event was organized in the context of the 2012 American Association of Wine Economists conference. This repository provides an independent exploratory analysis and is not an official publication of Princeton University or the event organizers.
License
No license file was visible in the reviewed repository. Refer to the repository owner for reuse permissions; a license can be added if public reuse and contributions are intended.
