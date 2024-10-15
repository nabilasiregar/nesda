## Preparing the Dataset

1. Create a directory named `data` and place the raw dataset files into this folder.

2. Run the following Jupyter notebooks to prepare the data:

   - `prepare_wave1.ipynb`
   - `prepare_wave5.ipynb`

   **Important:** The notebooks must be executed consecutively, starting with `prepare_wave1.ipynb`, followed by `prepare_wave5.ipynb`.

   These steps will generate a dataset for constructing the network, saved as `updated_discrete_data.csv`.

## Generating the Cohort Causal Graph

All R scripts are located in the `R` directory.

### Hill-Climbing (HC) Causal Discovery

1. Run `bn_causal_discovery.R` to perform Hill-Climbing causal discovery.
   
2. Run `hc_demo.R` to visualize the resulting network.
   - If the network looks correct:
   
3. Run `hc_bootstrap.R` to perform bootstrap analysis.
   
4. Run `hc_analysis.R` to generate the analysis table.
   
5. Run `comparison.R` to compare the HC network with and without expert knowledge.

**Note:** When running `bn_causal_discovery.R` without expert knowledge, you still need to provide a blacklist. This is necessary because the tiered logic in `bnlearn` relies on the built-in function `tiers2blacklist(tiers)`, which needs to be included in the blacklist:

```r
    blacklist <- rbind(
    tiers2blacklist(tiers)
    )
```

### TPC
1. Run tpc_causal_discovery.R
2. Run tpc_demo.R (to visualize)
If the network looks like expected then
3. Run tpc_bootstrap.R
4. Run tpc_analysis.R (for the table)