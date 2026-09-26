"""
PIP V – Group 15
Panel Dataset Construction: GDP per capita vs. PM2.5 Exposure

INPUT:  ../data/WDI.csv
OUTPUT: ../data/panel_final.csv  (clean long-format panel, ready for regression)

SOURCE: World Bank World Development Indicators, DataBank export
FORMAT: Wide — one row per country-series, year columns "2000 [YR2000]" etc.
        Missing values coded as ".."
        Footer rows at bottom (blank rows + metadata) — stripped automatically
"""

import pandas as pd
import numpy as np
import os
import sys

# ── Configuration ─────────────────────────────────────────────────────────────

DATA_FILE   = "../data/WDI.csv"
OUTPUT_FILE = "../data/panel_final.csv"
YEARS       = list(range(2000, 2020))   # 2000–2019 inclusive

# Series codes in the file → clean column names for panel_final.csv
INDICATOR_MAP = {
    "EN.ATM.PM25.MC.M3":    "pm25",
    "NY.GDP.PCAP.CD":        "gdp_pc",           # current USD — see PROJECT_CONTEXT.md
    "EN.POP.DNST":           "pop_density",
    "SP.URB.TOTL.IN.ZS":     "urban_pct",
    "NV.IND.TOTL.ZS":        "industry_gdp_pct",
    "NE.TRD.GNFS.ZS":        "trade_gdp_pct",
    "EG.FEC.RNEW.ZS":        "renewables_pct",
    "EN.GHG.CO2.PC.CE.AR5":  "co2_pc",           # reference only — not a regression control
}

# World Bank aggregate region codes — not individual countries, must be removed
AGGREGATE_CODES = {
    "AFE","AFW","ARB","CEB","CSS","EAP","EAR","EAS","ECA","ECS","EMU","EUU",
    "FCS","HIC","HPC","IBD","IBT","IDA","IDB","IDX","INX","LAC","LCN","LDC",
    "LIC","LMC","LMY","LTE","MEA","MIC","MNA","NAC","OED","OSS","PRE","PSS",
    "PST","SAS","SSA","SSF","SST","TEA","TEC","TLA","TMN","TSA","TSS","UMC",
    "WLD","XZN",
}

# ── Load & parse ───────────────────────────────────────────────────────────────

def load_wdi(filepath):
    print(f"Reading: {filepath}")
    if not os.path.exists(filepath):
        sys.exit(
            f"ERROR: '{filepath}' not found.\n"
            f"Make sure WDI.csv is in the data/ folder."
        )

    # Read raw — missing values ".." become NaN
    raw = pd.read_csv(filepath, dtype=str, na_values=["..", "", "NA"])
    raw.columns = [c.strip() for c in raw.columns]

    # Strip footer: blank rows and metadata rows (Country Code is NaN or starts with keywords)
    raw = raw[raw["Country Code"].notna()]
    raw = raw[~raw["Country Name"].str.startswith("Data from database", na=True)]
    raw = raw[~raw["Country Name"].str.startswith("Last Updated", na=True)]

    # Remove World Bank aggregate regions
    raw = raw[~raw["Country Code"].isin(AGGREGATE_CODES)]

    # Identify year columns — format is "2000 [YR2000]", "2001 [YR2001]", etc.
    year_col_map = {}   # "2000 [YR2000]" -> 2000
    for col in raw.columns:
        token = col.strip()[:4]
        if token.isdigit() and int(token) in YEARS:
            year_col_map[col] = int(token)

    if not year_col_map:
        sys.exit(
            "ERROR: Could not identify year columns.\n"
            f"Columns found: {list(raw.columns)}"
        )

    # Keep only the indicators we want
    raw = raw[raw["Series Code"].isin(INDICATOR_MAP.keys())]

    print(f"  {raw['Country Code'].nunique()} countries, "
          f"{raw['Series Code'].nunique()} indicators found")

    # Melt wide → long
    long = raw.melt(
        id_vars=["Country Code", "Series Code"],
        value_vars=list(year_col_map.keys()),
        var_name="year_col",
        value_name="value"
    )
    long["year"]  = long["year_col"].map(year_col_map)
    long["value"] = pd.to_numeric(long["value"], errors="coerce")
    long = long.rename(columns={"Country Code": "country_code"})

    # Pivot: one column per indicator
    panel = long.pivot_table(
        index=["country_code", "year"],
        columns="Series Code",
        values="value",
        aggfunc="first"
    ).reset_index()
    panel.columns.name = None

    # Rename indicator codes to clean names
    present = {k: v for k, v in INDICATOR_MAP.items() if k in panel.columns}
    missing = set(INDICATOR_MAP.keys()) - set(panel.columns)
    if missing:
        print(f"  WARNING: indicators not found in file: {missing}")
    panel = panel.rename(columns=present)

    print(f"  Reshaped to {len(panel)} rows "
          f"({panel['country_code'].nunique()} countries × {panel['year'].nunique()} years)")
    return panel

# ── Transform ─────────────────────────────────────────────────────────────────

def transform(panel):
    # Log transforms for regression
    panel["ln_pm25"]        = np.log(panel["pm25"].clip(lower=0.01))
    panel["ln_gdp_pc"]      = np.log(panel["gdp_pc"].clip(lower=1))
    panel["ln_gdp_pc_sq"]   = panel["ln_gdp_pc"] ** 2   # squared term for EKC/inverted-U test
    panel["ln_pop_density"] = np.log(panel["pop_density"].clip(lower=0.01))

    # Drop rows missing both core variables (DV + IV)
    before = len(panel)
    panel  = panel.dropna(subset=["pm25", "gdp_pc"])
    print(f"\nDropped {before - len(panel)} rows missing pm25 or gdp_pc")

    # Final column order
    col_order = [
        "country_code", "year",
        "pm25", "ln_pm25",
        "gdp_pc", "ln_gdp_pc", "ln_gdp_pc_sq",
        "pop_density", "ln_pop_density",
        "urban_pct", "industry_gdp_pct", "trade_gdp_pct",
        "renewables_pct",
        "co2_pc",   # reference only — do not include as control in regression
    ]
    col_order = [c for c in col_order if c in panel.columns]
    return panel[col_order].sort_values(["country_code", "year"]).reset_index(drop=True)

# ── Report ────────────────────────────────────────────────────────────────────

def report(panel):
    print(f"\n── Final panel ──────────────────────────────")
    print(f"  Rows:      {len(panel)}")
    print(f"  Countries: {panel['country_code'].nunique()}")
    print(f"  Years:     {panel['year'].min()}–{panel['year'].max()}")

    vars_check = [
        "pm25", "gdp_pc", "pop_density", "urban_pct",
        "industry_gdp_pct", "trade_gdp_pct", "renewables_pct", "co2_pc"
    ]
    print(f"\n── Missingness (% of rows) ──────────────────")
    for var in vars_check:
        if var in panel.columns:
            pct  = panel[var].isna().mean() * 100
            flag = "  ⚠ HIGH" if pct > 30 else ""
            print(f"  {var:<25} {pct:5.1f}%{flag}")

    print(f"\n── Descriptive statistics ───────────────────")
    print(panel[[c for c in vars_check if c in panel.columns]].describe().round(2).to_string())

    print(f"\n── Balance check ────────────────────────────")
    obs    = panel.groupby("country_code")["year"].count()
    n_full = (obs == len(YEARS)).sum()
    print(f"  Countries with all {len(YEARS)} years: {n_full}/{len(obs)}")
    print(f"  Min obs per country: {obs.min()} | Max: {obs.max()}")
    if n_full < len(obs):
        print("  → Unbalanced panel — use fixed effects in regression.")

# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    print("=" * 60)
    print("PIP V – Group 15 | Panel Dataset Construction")
    print("=" * 60)

    panel = load_wdi(DATA_FILE)
    panel = transform(panel)
    report(panel)

    panel.to_csv(OUTPUT_FILE, index=False)
    print(f"\n✓ Saved: {OUTPUT_FILE}")
    print("  Python : pd.read_csv('../data/panel_final.csv')")
    print("  R      : read.csv('../data/panel_final.csv')")

if __name__ == "__main__":
    main()
