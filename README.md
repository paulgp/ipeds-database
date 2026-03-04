# IPEDS Database

Build a harmonized [DuckDB](https://duckdb.org/) database from 20+ years of
[IPEDS](https://nces.ed.gov/ipeds/) higher-education data in a single command.

```bash
uv run python build_database.py
```

The result is a ~1 GB database with **27 million rows** across **23 tables**
covering every U.S. postsecondary institution from 1997 to 2024—admissions,
enrollment, completions, tuition, financial aid, graduation rates, staffing,
and more—ready to query with SQL.

## Why?

IPEDS is the most comprehensive source of data on U.S. colleges and
universities. But working with it is painful: hundreds of separate CSV files,
naming conventions that change every few years, undocumented missing-value
codes, and race/ethnicity categories that were overhauled in 2008–2010.

This project handles all of that. One script downloads everything, harmonizes
the schemas across years, and loads it into a fast analytical database you can
query instantly.

## Quick Start

**Prerequisites:** [Python 3.11+](https://python.org) and
[uv](https://github.com/astral-sh/uv).

```bash
git clone https://github.com/paulgp/ipeds-database.git
cd ipeds-database

# Install dependencies and build the database
uv sync
uv run python build_database.py
```

The first run downloads ~720 MB of ZIP files from NCES (cached in `data/raw/`
so rebuilds are fast) and produces `ipeds.duckdb` (~1.1 GB).

Then query with any DuckDB client:

```bash
duckdb ipeds.duckdb
```

```sql
-- MBA degrees over time
SELECT year,
       SUM(CAST(COALESCE(ctotalt, crace24, crace15 + crace16) AS BIGINT)) AS mbas
FROM   c_a
WHERE  CAST(cipcode AS VARCHAR) = '52.0201' AND award_level = 7
GROUP  BY year ORDER BY year;
```

Or from Python:

```python
import duckdb
con = duckdb.connect("ipeds.duckdb", read_only=True)
con.sql("SELECT * FROM _metadata").show()
```

## What's Included

| Table | Description | Years | ~Rows |
|-------|-------------|:-----:|------:|
| **`hd`** | Institutional directory (name, location, sector, Carnegie) | 2002–2024 | 162K |
| **`ic`** | Institutional characteristics | 2000–2024 | 174K |
| **`ic_ay`** | Academic year tuition & fees | 2000–2023 | 90K |
| **`ic_py`** | Program year tuition & fees | 2000–2023 | 61K |
| **`adm`** | Admissions: applicants, admits, enrolled, SAT/ACT | 2014–2023 | 21K |
| **`efia`** | 12-month instructional activity (FTE) | 2002–2024 | 155K |
| **`effy`** | 12-month unduplicated headcount | 2002–2024 | 841K |
| **`ef_a`** | Fall enrollment by race/ethnicity & gender | 2000–2023 | 2.8M |
| **`ef_b`** | Fall enrollment by age | 2000–2023 | 3.3M |
| **`ef_c`** | Residence of first-time students | 2000–2023 | 1.4M |
| **`ef_d`** | Retention rates | 2000–2023 | 146K |
| **`c_a`** | Completions by CIP code, race, gender, award level | 2000–2024 | 6.6M |
| **`sfa`** | Student financial aid (grants, loans, net price) | 2002–2023 | 133K |
| **`gr`** | Graduation rates (150% normal time) | 1997–2023 | 1.2M |
| **`gr200`** | Graduation rates (200% normal time) | 2008–2023 | 88K |
| **`om`** | Outcome measures (8-year outcomes) | 2015–2023 | 368K |
| **`eap`** | Employees by assigned position | 2001–2023 | 8.7M |
| **`sal_is`** | Instructional staff salaries | 2012–2023 | 195K |
| **`al`** | Academic libraries | 2014–2023 | 39K |
| **`f1a`** | Finance: GASB (public) — revenue, expenses, endowment, assets | 2002–2023 | 42K |
| **`f2`** | Finance: FASB (private nonprofit) — revenue, expenses, endowment | 2001–2023 | 43K |
| **`f3`** | Finance: for-profit — revenue, expenses | 2001–2023 | 59K |
| **`flags`** | Survey response status flags | 2004–2024 | 148K |

Every table has a **`unitid`** (institution ID) and **`year`** column for
joining. The **`_metadata`** table provides a machine-readable inventory.

### Built-in Views

| View | Description |
|------|-------------|
| `v_institutions` | Latest directory info per institution (deduplicated) |
| `v_admission_rates` | Admission and yield rates with institution names |
| `v_tuition_trends` | In-state/out-of-state tuition with institution info |

## Key Columns

The `hd` table renames IPEDS codes to readable names:

| Column | Description |
|--------|-------------|
| `institution_name` | Institution name |
| `state` | State abbreviation |
| `sector` | 1 = Public 4-yr, 2 = Private nonprofit 4-yr, 3 = For-profit 4-yr, … |
| `control` | 1 = Public, 2 = Private nonprofit, 3 = Private for-profit |
| `level` | 1 = 4-year, 2 = 2-year, 3 = Less than 2-year |
| `carnegie_basic` | Carnegie classification (harmonized across name changes) |
| `hbcu` | HBCU flag |
| `longitude` / `latitude` | Coordinates |

The `adm` table renames admissions columns:
- `applicants_total`, `admissions_total`, `enrolled_total` (plus `_men`/`_women`)
- `sat_math_25th`, `sat_math_75th`, `act_composite_25th`, etc.

The **`c_a`** (completions) table uses these key codes:

| Column | Values |
|--------|--------|
| `cipcode` | 6-digit CIP code (e.g., `52.0201` = MBA) |
| `award_level` | 5 = Bachelor's, 7 = Master's, 9 = Doctor's (pre-2010), 17 = Research doctorate (2008+), 18 = Professional practice doctorate (2008+) |
| `ctotalt` | Total completions (2008+); use `COALESCE(ctotalt, crace24, crace15 + crace16)` for full series |

The **`f1a`** (public) and **`f2`** (private nonprofit) finance tables include endowment data:

| Column | Description |
|--------|-------------|
| `f1h01` / `f2h01` | Endowment value, beginning of fiscal year |
| `f1h02` / `f2h02` | Endowment value, end of fiscal year |
| `f1h03` / `f2h03` | Net change in endowment |
| `f1h03a` / `f2h03a` | Contributions to endowment |
| `f1h03b` / `f2h03b` | Investment return |
| `f1h03c` / `f2h03c` | Amounts spent/withdrawn |

All other tables preserve original IPEDS column names (lowercased) for
compatibility with the [IPEDS data dictionary](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx).

## Example Queries

See [`examples/query_examples.sql`](examples/query_examples.sql) for 10
ready-to-run queries. A few highlights:

**Most selective universities (2023):**
```sql
SELECT h.institution_name,
       ROUND(a.admissions_total * 100.0 / a.applicants_total, 1) AS admit_rate
FROM   adm a
JOIN   hd h ON a.unitid = h.unitid AND a.year = h.year
WHERE  a.year = 2023 AND a.applicants_total > 20000
ORDER  BY admit_rate LIMIT 10;
```

**Top university endowments (FY2023):**
```sql
SELECT h.institution_name, h.state, CAST(f.f2h02 AS BIGINT) AS endowment
FROM   f2 f
JOIN   hd h ON f.unitid = h.unitid AND h.year = 2023
WHERE  f.year = 2023 AND f.f2h02 > 0
ORDER  BY endowment DESC LIMIT 10;
```

**In-state vs. out-of-state enrollment (first-time undergrads):**
```sql
SELECT c.year,
       SUM(CASE WHEN CAST(c.efcstate AS INT) = CAST(h.fips_state AS INT)
                THEN CAST(c.efres01 AS BIGINT) END) AS in_state,
       SUM(CASE WHEN CAST(c.efcstate AS INT) = 99
                THEN CAST(c.efres01 AS BIGINT) END) AS total
FROM   ef_c c
JOIN   hd h ON c.unitid = h.unitid AND c.year = h.year
WHERE  c.year IN (2022)
GROUP  BY c.year;
```

## Example Figures

The [`examples/figures/`](examples/figures/) directory contains R scripts that
produce publication-quality plots directly from the database:

| Script | Output |
|--------|--------|
| `degree_plots.R` | Bachelor's, MBA, and PhD degrees over time |
| `mba_expanded_plot.R` | MBA vs. competing business master's programs |
| `mba_stacked_plot.R` | Stacked area chart of business master's degrees |
| `econ_masters_plot.R` | Economics master's reclassification story |
| `residence_plot.R` | In-state vs. out-of-state enrollment trends |

<p align="center">
<img src="examples/figures/mba_stacked.png" width="700" alt="Business Master's Degrees stacked area chart">
</p>

<p align="center">
<img src="examples/figures/econ_masters.png" width="700" alt="Economics Master's reclassification">
</p>

<p align="center">
<img src="examples/figures/residence_pct.png" width="700" alt="Student residence trends">
</p>

## Harmonization Notes

### Cross-Year Schema Changes

IPEDS survey instruments change frequently. This builder handles the
differences by taking the union of all columns across years; missing columns
for a given year are `NULL`.

Key changes the builder handles:

- **Race/ethnicity (2008–2010):** IPEDS switched from 5 categories to 9
  (separating Asian/Pacific Islander, adding Two or More Races). Both old
  and new columns are preserved.
- **Completions totals column:** Named `crace15 + crace16` (2000–2001),
  `crace24` (2002–2007), then `ctotalt` (2008+). Use
  `COALESCE(ctotalt, crace24, crace15 + crace16)` for a continuous series.
- **Doctorate types (2008+):** Pre-2010 used `award_level = 9` for all
  doctorates. Post-2008 splits into 17 (research) and 18 (professional
  practice). No institution overlap in the 2008–2009 transition years.
- **Carnegie classification:** Column name changed across years (`carnegie` →
  `ccbasic` → `c15basic` → `c18basic` → `c21basic`); the `hd` table maps all
  to `carnegie_basic`.
- **Admissions (ADM):** Became a separate survey in 2014. Prior admissions data
  is in the IC survey.
- **Financial aid (SFA):** Grew from 58 columns (2002) to 692 columns (2023)
  as net-price and income-bracket reporting expanded.
- **Residence (EF_C):** Full institutional coverage in even years only
  (2009+ odd years cover ~2,200 vs ~5,500 institutions in even years).
- **Finance (F1A/F2/F3):** Three separate tables by accounting standard—`f1a`
  (GASB, public institutions), `f2` (FASB, private nonprofits), `f3`
  (for-profit). Endowment columns use parallel naming (`f1h02` vs `f2h02`).
  Column counts vary from 72 to 522 across years as reporting expanded.

### Missing Values

IPEDS uses blank, `.` (SAS missing), and letter codes for missing data. All
are converted to `NULL`. Columns with >70% numeric values are auto-cast to
`DOUBLE`.

### Known Gaps

- `EF2001D` (2001 retention rates): returns HTTP 404 from NCES
- `IC_AY` 2009–2012: downloaded as 0-row placeholder files
- Pre-2012 salary data (`SAL` without `_IS` suffix): uses a different schema;
  omitted for now

## Building a Subset

To rebuild only specific tables (faster for development):

```bash
uv run python build_database.py hd c_a adm
```

## Project Structure

```
ipeds-database/
├── build_database.py          # ETL pipeline (single file, ~600 lines)
├── pyproject.toml             # Python dependencies
├── uv.lock                    # Locked dependency versions
├── LICENSE                    # MIT
├── README.md
├── examples/
│   ├── query_examples.sql     # 10 example SQL queries
│   └── figures/               # R scripts and output PNGs
│       ├── degree_plots.R
│       ├── mba_expanded_plot.R
│       ├── mba_stacked_plot.R
│       ├── econ_masters_plot.R
│       ├── residence_plot.R
│       └── *.png
├── data/raw/                  # Cached NCES downloads (~720 MB, gitignored)
└── ipeds.duckdb               # Output database (~1.1 GB, gitignored)
```

## Requirements

- Python 3.11+
- [uv](https://github.com/astral-sh/uv) (or pip: `pip install duckdb pandas requests beautifulsoup4 lxml`)
- ~2 GB disk space (720 MB downloads + 1.1 GB database)
- Internet connection for initial download

## Data Source

All data is downloaded from the [IPEDS Data Center](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx):

```
https://nces.ed.gov/ipeds/datacenter/data/{SURVEY}{YEAR}.zip
```

IPEDS data is in the public domain (U.S. government work).

## License

Code: [MIT](LICENSE). Data: public domain (NCES/U.S. Department of Education).
