#!/usr/bin/env python3
"""
IPEDS Database Builder
======================
Downloads, parses, cleans, and harmonizes IPEDS data from the National Center
for Education Statistics (NCES) into a single DuckDB database.

Usage:
    uv run python build_database.py            # Build full database
    uv run python build_database.py hd adm     # Build only selected tables
    uv run python build_database.py --help      # Show this help

The builder downloads ZIP files from NCES, extracts CSV data, harmonizes
column names and types across years, and loads everything into DuckDB.
Downloaded files are cached in data/raw/ so subsequent runs skip downloads.

Survey components (20 tables):
    hd       Institutional directory (name, location, control, Carnegie)
    ic       Institutional characteristics (programs, services, athletics)
    ic_ay    Academic year tuition and fees
    ic_py    Program year tuition and fees
    adm      Admissions (applicants, admits, enrolled, SAT/ACT)
    efia     12-month instructional activity (FTE)
    effy     12-month unduplicated headcount enrollment
    ef_a     Fall enrollment by race/ethnicity and gender
    ef_b     Fall enrollment by age
    ef_c     Residence and migration of first-time students
    ef_d     Retention rates
    c_a      Completions: degrees/awards by CIP code, race, gender
    sfa      Student financial aid (grants, loans, net price)
    gr       Graduation rates (150% of normal time)
    gr200    Graduation rates (200% of normal time)
    om       Outcome measures (8-year outcomes)
    eap      Employees by assigned position
    sal_is   Salaries of instructional staff
    al       Academic libraries
    flags    Survey response status flags
"""

import os
import io
import re
import sys
import json
import zipfile
import logging
import hashlib
from pathlib import Path
from collections import defaultdict

import requests
import pandas as pd
import duckdb

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

BASE_URL = "https://nces.ed.gov/ipeds/datacenter/data/"
DATA_DIR = Path("data/raw")
DB_PATH = Path("ipeds.duckdb")

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("ipeds")

# ---------------------------------------------------------------------------
# File manifest: survey -> list of (year, filename) 
# We define these explicitly so we have full control over what goes in.
# ---------------------------------------------------------------------------

def build_manifest():
    """Build the file manifest from the discovered files."""
    manifest = {}
    
    # HD: Directory information
    manifest["hd"] = [(y, f"HD{y}") for y in range(2024, 2001, -1)]
    
    # IC: Institutional characteristics
    manifest["ic"] = [(y, f"IC{y}") for y in range(2024, 1999, -1)]
    
    # IC_AY: Academic year prices
    manifest["ic_ay"] = [(y, f"IC{y}_AY") for y in range(2023, 1999, -1)]
    
    # IC_PY: Program year prices
    manifest["ic_py"] = [(y, f"IC{y}_PY") for y in range(2023, 1999, -1)]
    
    # ADM: Admissions
    manifest["adm"] = [(y, f"ADM{y}") for y in range(2023, 2013, -1)]
    
    # EFIA: 12-month instructional activity
    manifest["efia"] = [(y, f"EFIA{y}") for y in range(2024, 2001, -1)]
    
    # EFFY: 12-month unduplicated headcount
    manifest["effy"] = [(y, f"EFFY{y}") for y in range(2024, 2001, -1)]
    
    # EFA: Fall enrollment by race/ethnicity (A component)
    manifest["ef_a"] = [(y, f"EF{y}A") for y in range(2023, 1999, -1)]
    
    # EFB: Fall enrollment by age
    manifest["ef_b"] = [(y, f"EF{y}B") for y in range(2023, 1999, -1)]
    
    # EFC: Residence and migration
    manifest["ef_c"] = [(y, f"EF{y}C") for y in range(2023, 1999, -1)]
    
    # EFD: Retention rates
    manifest["ef_d"] = [(y, f"EF{y}D") for y in range(2023, 1999, -1)]
    
    # C_A: Completions awards
    manifest["c_a"] = [(y, f"C{y}_A") for y in range(2024, 1999, -1)]
    
    # SFA: Student financial aid (uses academic year naming)
    sfa_files = []
    sfa_files.append((2023, "SFA2223"))
    sfa_files.append((2022, "SFA2122"))
    sfa_files.append((2021, "SFA2021"))
    sfa_files.append((2020, "SFA1920"))
    sfa_files.append((2019, "SFA1819"))
    sfa_files.append((2018, "SFA1718"))
    sfa_files.append((2017, "SFA1617"))
    sfa_files.append((2016, "SFA1516"))
    sfa_files.append((2015, "SFA1415"))
    sfa_files.append((2014, "SFA1314"))
    sfa_files.append((2013, "SFA1213"))
    sfa_files.append((2012, "SFA1112"))
    sfa_files.append((2011, "SFA1011"))
    sfa_files.append((2010, "SFA0910"))
    sfa_files.append((2009, "SFA0809"))
    sfa_files.append((2008, "SFA0708"))
    sfa_files.append((2007, "SFA0607"))
    sfa_files.append((2006, "SFA0506"))
    sfa_files.append((2005, "SFA0405"))
    sfa_files.append((2004, "SFA0304"))
    sfa_files.append((2003, "SFA0203"))
    sfa_files.append((2002, "SFA0102"))
    manifest["sfa"] = sfa_files
    
    # GR: Graduation rates 150%
    manifest["gr"] = [(y, f"GR{y}") for y in range(2023, 1996, -1)]
    
    # GR200: Graduation rates 200% (uses 2-digit year in filename)
    manifest["gr200"] = [(2000 + y, f"GR200_{y:02d}") for y in range(23, 7, -1)]
    
    # OM: Outcome measures
    manifest["om"] = [(y, f"OM{y}") for y in range(2023, 2014, -1)]
    
    # EAP: Employees by assigned position
    manifest["eap"] = [(y, f"EAP{y}") for y in range(2023, 2000, -1)]
    
    # SAL_IS: Salaries - instructional staff
    manifest["sal_is"] = [(y, f"SAL{y}_IS") for y in range(2023, 2011, -1)]
    
    # SAL: Salaries (older format, pre-2012) - these files use different naming;
    # the SAL{year}_IS format only starts in 2012, and pre-2012 data uses
    # different file structures. Omitting for now as the naming convention changed.
    # manifest["sal"] = [(y, f"SAL{y}") for y in range(2011, 2000, -1)]
    
    # AL: Academic libraries
    manifest["al"] = [(y, f"AL{y}") for y in range(2023, 2013, -1)]
    
    # FLAGS: Response status
    manifest["flags"] = [(y, f"FLAGS{y}") for y in range(2024, 2003, -1)]
    
    return manifest


# ---------------------------------------------------------------------------
# Download
# ---------------------------------------------------------------------------

def download_file(filename: str, retries: int = 3) -> Path:
    """Download a zip file if not already cached."""
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    local_path = DATA_DIR / f"{filename}.zip"
    if local_path.exists() and local_path.stat().st_size > 0:
        return local_path
    
    url = f"{BASE_URL}{filename}.zip"
    for attempt in range(retries):
        try:
            log.info(f"Downloading {url}" + (f" (attempt {attempt+1})" if attempt > 0 else ""))
            resp = requests.get(url, timeout=120, stream=False)
            if resp.status_code != 200:
                log.warning(f"  Failed to download {filename}: HTTP {resp.status_code}")
                return None
            local_path.write_bytes(resp.content)
            return local_path
        except (requests.exceptions.Timeout, requests.exceptions.ConnectionError) as e:
            log.warning(f"  Download error (attempt {attempt+1}/{retries}): {e}")
            if attempt == retries - 1:
                return None
            import time
            time.sleep(2 ** attempt)
    return None


# ---------------------------------------------------------------------------
# CSV Reading
# ---------------------------------------------------------------------------

def read_csv_from_zip(zip_path: Path) -> pd.DataFrame:
    """Extract and read the CSV data file from a zip archive."""
    with zipfile.ZipFile(zip_path) as zf:
        # Find the main CSV file (not the dictionary/frequencies file)
        csv_files = [
            f for f in zf.namelist()
            if f.lower().endswith('.csv')
            and '_dict' not in f.lower()
            and '_freq' not in f.lower()
            and '_rv' not in f.lower()
        ]
        if not csv_files:
            # Maybe the data file has a different extension
            csv_files = [f for f in zf.namelist() if f.lower().endswith('.csv')]
        
        if not csv_files:
            log.warning(f"  No CSV found in {zip_path.name}: {zf.namelist()}")
            return None
        
        # Pick the largest CSV (usually the data file)
        csv_file = max(csv_files, key=lambda f: zf.getinfo(f).file_size)
        
        with zf.open(csv_file) as f:
            raw = f.read()
        
        # Try common encodings
        for encoding in ['utf-8', 'latin-1', 'cp1252']:
            try:
                text = raw.decode(encoding)
                break
            except UnicodeDecodeError:
                continue
        else:
            text = raw.decode('latin-1', errors='replace')
        
        # Parse CSV
        try:
            df = pd.read_csv(
                io.StringIO(text),
                low_memory=False,
                on_bad_lines='skip',
                encoding_errors='replace',
            )
        except Exception as e:
            log.warning(f"  Error reading CSV from {zip_path.name}: {e}")
            return None
        
        return df


# ---------------------------------------------------------------------------
# Cleaning utilities
# ---------------------------------------------------------------------------

def clean_column_names(df: pd.DataFrame) -> pd.DataFrame:
    """Standardize column names to lowercase, strip whitespace."""
    df.columns = [c.strip().lower() for c in df.columns]
    return df


def coerce_unitid(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure UNITID is integer type."""
    if 'unitid' in df.columns:
        df['unitid'] = pd.to_numeric(df['unitid'], errors='coerce')
        df = df.dropna(subset=['unitid'])
        df['unitid'] = df['unitid'].astype(int)
    return df


def clean_numeric_columns(df: pd.DataFrame, exclude_cols=None) -> pd.DataFrame:
    """
    Convert columns that should be numeric but contain IPEDS special values.
    IPEDS uses various codes for missing/not applicable:
      - Empty/blank
      - '.' (SAS missing)  
      - 'A' through 'Z' (various imputation flags when embedded)
    """
    exclude = set(exclude_cols or [])
    exclude.update(['unitid', 'instnm', 'addr', 'city', 'stabbr', 'zip', 
                    'chfnm', 'chftitle', 'gentele', 'ein', 'webaddr',
                    'adminurl', 'faidurl', 'applurl', 'npricurl',
                    'opeid', 'countynm', 'countycd', 'longitud', 'latitude',
                    'closedat', 'cyactive', 'ialias'])
    
    for col in df.columns:
        if col in exclude:
            continue
        if df[col].dtype == object:
            # Check if this looks numeric
            sample = df[col].dropna().head(100)
            if len(sample) == 0:
                continue
            # Try converting - IPEDS uses '.' for missing
            converted = pd.to_numeric(
                df[col].replace({'.': None, '': None}), 
                errors='coerce'
            )
            # If we can convert most values, do it
            non_null_orig = df[col].notna().sum()
            non_null_conv = converted.notna().sum()
            if non_null_orig > 0 and non_null_conv / non_null_orig > 0.5:
                df[col] = converted
    
    return df


# ---------------------------------------------------------------------------
# Survey-specific harmonization
# ---------------------------------------------------------------------------

def harmonize_hd(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize institutional directory (HD) data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    
    # Key columns to keep (harmonize names across years)
    # Column renames for consistency
    renames = {}
    
    # Institution name
    for old in ['instnm']:
        if old in df.columns:
            renames[old] = 'institution_name'
    
    # State
    for old in ['stabbr']:
        if old in df.columns:
            renames[old] = 'state'
    
    # City
    if 'city' in df.columns:
        renames['city'] = 'city'
    
    # ZIP
    if 'zip' in df.columns:
        renames['zip'] = 'zip_code'
    
    # FIPS state code
    if 'fips' in df.columns:
        renames['fips'] = 'fips_state'
    
    # Sector (public/private/for-profit, 4yr/2yr/less)
    if 'sector' in df.columns:
        renames['sector'] = 'sector'
    
    # Control of institution
    if 'control' in df.columns:
        renames['control'] = 'control'
    
    # Level (4-year, 2-year, less-than-2-year)
    if 'iclevel' in df.columns:
        renames['iclevel'] = 'level'
    
    # Carnegie classification
    for old in ['c21basic', 'c18basic', 'c15basic', 'ccbasic', 'carnegie']:
        if old in df.columns:
            renames[old] = 'carnegie_basic'
            break
    
    # Degree-granting status
    if 'deggrant' in df.columns:
        renames['deggrant'] = 'degree_granting'
    
    # HBCU
    if 'hbcu' in df.columns:
        renames['hbcu'] = 'hbcu'
    
    # Hospital
    if 'hospital' in df.columns:
        renames['hospital'] = 'hospital'
    
    # Medical
    if 'medical' in df.columns:
        renames['medical'] = 'medical'
    
    # Land grant
    if 'landgrnt' in df.columns:
        renames['landgrnt'] = 'land_grant'
    
    # Tribal
    if 'tribal' in df.columns:
        renames['tribal'] = 'tribal'
    
    # Locale
    if 'locale' in df.columns:
        renames['locale'] = 'locale_code'
    
    # OPEID
    if 'opeid' in df.columns:
        renames['opeid'] = 'opeid'
    
    # Geographic coordinates
    if 'longitud' in df.columns:
        renames['longitud'] = 'longitude'
    if 'latitude' in df.columns:
        renames['latitude'] = 'latitude'
    
    # Size category
    if 'instsize' in df.columns:
        renames['instsize'] = 'size_category'
    
    # Active status
    if 'cyactive' in df.columns:
        renames['cyactive'] = 'currently_active'
    
    # Close date
    if 'closedat' in df.columns:
        renames['closedat'] = 'close_date'
    
    # Region
    if 'obereg' in df.columns:
        renames['obereg'] = 'region'
    
    # County
    if 'countynm' in df.columns:
        renames['countynm'] = 'county_name'
    if 'countycd' in df.columns:
        renames['countycd'] = 'county_fips'
        
    # Website
    if 'webaddr' in df.columns:
        renames['webaddr'] = 'website'
    
    df = df.rename(columns=renames)
    
    # Keep all columns (renamed + original unrenamed)
    return df


def harmonize_ic(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize institutional characteristics."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_ic_ay(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize academic year prices."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    
    renames = {}
    # Tuition and fees
    for old in ['tuition2', 'chg2ay2']:  # in-state tuition
        if old in df.columns:
            renames[old] = 'tuition_in_state'
            break
    for old in ['tuition3', 'chg2ay3']:  # out-of-state tuition
        if old in df.columns:
            renames[old] = 'tuition_out_state'
            break
    for old in ['tuition1', 'chg2ay1']:  # in-district tuition
        if old in df.columns:
            renames[old] = 'tuition_in_district'
            break
    
    df = df.rename(columns=renames)
    df = clean_numeric_columns(df)
    return df


def harmonize_ic_py(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize program year prices."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_adm(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize admissions data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    
    renames = {}
    # Applicants
    if 'applcn' in df.columns:
        renames['applcn'] = 'applicants_total'
    if 'applcnm' in df.columns:
        renames['applcnm'] = 'applicants_men'
    if 'applcnw' in df.columns:
        renames['applcnw'] = 'applicants_women'
    
    # Admissions
    if 'admssn' in df.columns:
        renames['admssn'] = 'admissions_total'
    if 'admssnm' in df.columns:
        renames['admssnm'] = 'admissions_men'
    if 'admssnw' in df.columns:
        renames['admssnw'] = 'admissions_women'
    
    # Enrolled
    if 'enrlt' in df.columns:
        renames['enrlt'] = 'enrolled_total'
    if 'enrlm' in df.columns:
        renames['enrlm'] = 'enrolled_men'
    if 'enrlw' in df.columns:
        renames['enrlw'] = 'enrolled_women'
    
    # Test scores
    for prefix, new_prefix in [('satvr', 'sat_verbal'), ('satmt', 'sat_math'), 
                                ('satwr', 'sat_writing'), ('actcm', 'act_composite'),
                                ('acten', 'act_english'), ('actmt', 'act_math'),
                                ('actwr', 'act_writing')]:
        if f'{prefix}25' in df.columns:
            renames[f'{prefix}25'] = f'{new_prefix}_25th'
        if f'{prefix}75' in df.columns:
            renames[f'{prefix}75'] = f'{new_prefix}_75th'
    
    # SAT Evidence-Based Reading and Writing (newer)
    if 'satvr25' not in df.columns:
        for old, new in [('saterdw25', 'sat_ebrw_25th'), ('saterdw75', 'sat_ebrw_75th')]:
            if old in df.columns:
                renames[old] = new
    
    df = df.rename(columns=renames)
    df = clean_numeric_columns(df)
    return df


def harmonize_efia(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize 12-month instructional activity."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_effy(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize 12-month unduplicated headcount."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_ef_enrollment(df: pd.DataFrame, year: int, component: str) -> pd.DataFrame:
    """Harmonize fall enrollment data (A, B, C, D components)."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_completions(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize completions awards data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    
    renames = {}
    # Award level
    if 'awlevel' in df.columns:
        renames['awlevel'] = 'award_level'
    if 'majornum' in df.columns:
        renames['majornum'] = 'major_number'
    
    # CIP code
    for old in ['cipcode', 'cip2dig', 'cip4dig', 'cip6dig']:
        if old in df.columns and old != 'cipcode':
            pass  # keep cipcode as primary
    
    df = df.rename(columns=renames)
    df = clean_numeric_columns(df)
    return df


def harmonize_sfa(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize student financial aid data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_gr(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize graduation rate data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_gr200(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize 200% graduation rate data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_om(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize outcome measures data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_eap(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize employees by assigned position."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_sal(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize salary data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_al(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize academic library data."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


def harmonize_flags(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Harmonize response flags."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    return df


def harmonize_generic(df: pd.DataFrame, year: int) -> pd.DataFrame:
    """Generic harmonization for any survey."""
    df = clean_column_names(df)
    df = coerce_unitid(df)
    df['year'] = year
    df = clean_numeric_columns(df)
    return df


# Map survey -> harmonization function
HARMONIZERS = {
    'hd': harmonize_hd,
    'ic': harmonize_ic,
    'ic_ay': harmonize_ic_ay,
    'ic_py': harmonize_ic_py,
    'adm': harmonize_adm,
    'efia': harmonize_efia,
    'effy': harmonize_effy,
    'ef_a': lambda df, y: harmonize_ef_enrollment(df, y, 'A'),
    'ef_b': lambda df, y: harmonize_ef_enrollment(df, y, 'B'),
    'ef_c': lambda df, y: harmonize_ef_enrollment(df, y, 'C'),
    'ef_d': lambda df, y: harmonize_ef_enrollment(df, y, 'D'),
    'c_a': harmonize_completions,
    'sfa': harmonize_sfa,
    'gr': harmonize_gr,
    'gr200': harmonize_gr200,
    'om': harmonize_om,
    'eap': harmonize_eap,
    'sal_is': harmonize_sal,
    'sal': harmonize_sal,
    'al': harmonize_al,
    'flags': harmonize_flags,
}


# ---------------------------------------------------------------------------
# Schema harmonization across years 
# ---------------------------------------------------------------------------

def unify_schemas(frames: list[pd.DataFrame]) -> list[pd.DataFrame]:
    """
    Given a list of DataFrames for the same survey across years,
    unify their schemas by filling missing columns with NaN.
    """
    if not frames:
        return frames
    
    # Collect all columns
    all_cols = []
    seen = set()
    for df in frames:
        for c in df.columns:
            if c not in seen:
                all_cols.append(c)
                seen.add(c)
    
    # Reindex each frame
    unified = []
    for df in frames:
        for col in all_cols:
            if col not in df.columns:
                df[col] = None
        unified.append(df[all_cols])
    
    return unified


# ---------------------------------------------------------------------------
# DuckDB loading
# ---------------------------------------------------------------------------

def load_to_duckdb(con: duckdb.DuckDBPyConnection, table_name: str, df: pd.DataFrame):
    """Load a DataFrame into DuckDB, creating or appending to the table."""
    if df is None or len(df) == 0:
        return
    
    # DuckDB can ingest pandas directly
    con.execute(f"CREATE TABLE IF NOT EXISTS {table_name} AS SELECT * FROM df WHERE 1=0")
    
    # Check for schema differences and add missing columns
    existing_cols = set()
    try:
        result = con.execute(f"PRAGMA table_info('{table_name}')").fetchall()
        existing_cols = {row[1] for row in result}
    except:
        pass
    
    for col in df.columns:
        if col not in existing_cols and existing_cols:
            # Determine type
            dtype = df[col].dtype
            if pd.api.types.is_integer_dtype(dtype):
                sql_type = "BIGINT"
            elif pd.api.types.is_float_dtype(dtype):
                sql_type = "DOUBLE"
            else:
                sql_type = "VARCHAR"
            try:
                con.execute(f"ALTER TABLE {table_name} ADD COLUMN \"{col}\" {sql_type}")
            except Exception:
                pass  # Column might already exist with different case
    
    con.execute(f"INSERT INTO {table_name} SELECT * FROM df")


# ---------------------------------------------------------------------------
# Main pipeline
# ---------------------------------------------------------------------------

def process_survey(con: duckdb.DuckDBPyConnection, survey: str, 
                   file_list: list, harmonizer) -> dict:
    """Process all years for a single survey component."""
    log.info(f"{'='*60}")
    log.info(f"Processing survey: {survey}")
    log.info(f"{'='*60}")
    
    frames = []
    year_stats = {}
    failed_years = []
    
    for year, filename in file_list:
        zip_path = download_file(filename)
        if zip_path is None:
            log.warning(f"  {filename}: download failed, skipping")
            failed_years.append(year)
            continue
        
        df = read_csv_from_zip(zip_path)
        if df is None:
            log.warning(f"  {filename}: CSV read failed, skipping")
            failed_years.append(year)
            continue
        
        # Apply harmonization
        try:
            df = harmonizer(df, year)
        except Exception as e:
            log.warning(f"  {filename}: harmonization error: {e}")
            failed_years.append(year)
            continue
        
        row_count = len(df)
        col_count = len(df.columns)
        log.info(f"  {filename}: {row_count:,} rows × {col_count} cols")
        year_stats[year] = {'rows': row_count, 'cols': col_count}
        frames.append(df)
    
    if not frames:
        log.warning(f"  No data loaded for {survey}")
        return {'years_loaded': 0, 'total_rows': 0, 'failed_years': failed_years}
    
    # Unify schemas across years
    frames = unify_schemas(frames)
    
    # Concatenate
    combined = pd.concat(frames, ignore_index=True)
    log.info(f"  Combined: {len(combined):,} rows × {len(combined.columns)} cols")
    
    # Ensure numeric columns use float64 to avoid INT overflow
    for col in combined.columns:
        if combined[col].dtype == 'int64':
            # Check for large values that might overflow INT32
            if combined[col].abs().max() > 2**31:
                combined[col] = combined[col].astype('float64')
        # Convert remaining object columns that look numeric
        if combined[col].dtype == object:
            try:
                converted = pd.to_numeric(combined[col], errors='coerce')
                non_null_orig = combined[col].notna().sum()
                non_null_conv = converted.notna().sum()
                if non_null_orig > 0 and non_null_conv / max(non_null_orig, 1) > 0.7:
                    combined[col] = converted
            except:
                pass
    
    # Log column inventory
    col_types = combined.dtypes.value_counts()
    log.info(f"  Column types: {dict(col_types)}")
    
    # Load into DuckDB
    table_name = survey
    con.execute(f"DROP TABLE IF EXISTS {table_name}")
    con.register('_temp_df', combined)
    con.execute(f"CREATE TABLE {table_name} AS SELECT * FROM _temp_df")
    con.unregister('_temp_df')
    
    final_count = con.execute(f"SELECT COUNT(*) FROM {table_name}").fetchone()[0]
    log.info(f"  Loaded {final_count:,} rows into table '{table_name}'")
    
    return {
        'years_loaded': len(frames),
        'total_rows': final_count,
        'columns': len(combined.columns),
        'failed_years': failed_years,
        'year_stats': year_stats,
    }


def create_views(con: duckdb.DuckDBPyConnection):
    """Create useful analytical views."""
    
    # View: Institution summary with latest HD info
    con.execute("""
        CREATE OR REPLACE VIEW v_institutions AS
        SELECT * FROM (
            SELECT *, ROW_NUMBER() OVER (PARTITION BY unitid ORDER BY year DESC) as rn
            FROM hd
        ) WHERE rn = 1
    """)
    
    # View: Admissions rates
    try:
        con.execute("""
            CREATE OR REPLACE VIEW v_admission_rates AS
            SELECT 
                a.unitid,
                a.year,
                h.institution_name,
                h.state,
                a.applicants_total,
                a.admissions_total,
                a.enrolled_total,
                CASE WHEN a.applicants_total > 0 
                     THEN ROUND(a.admissions_total * 100.0 / a.applicants_total, 1)
                     ELSE NULL END as admit_rate_pct,
                CASE WHEN a.admissions_total > 0 
                     THEN ROUND(a.enrolled_total * 100.0 / a.admissions_total, 1)
                     ELSE NULL END as yield_rate_pct
            FROM adm a
            LEFT JOIN (
                SELECT unitid, institution_name, state,
                       ROW_NUMBER() OVER (PARTITION BY unitid ORDER BY year DESC) as rn
                FROM hd
            ) h ON a.unitid = h.unitid AND h.rn = 1
        """)
    except Exception as e:
        log.warning(f"Could not create v_admission_rates: {e}")
    
    # View: Tuition trends
    try:
        con.execute("""
            CREATE OR REPLACE VIEW v_tuition_trends AS
            SELECT 
                ay.unitid,
                ay.year,
                h.institution_name,
                h.state,
                h.sector,
                ay.tuition_in_state,
                ay.tuition_out_state,
                ay.tuition_in_district
            FROM ic_ay ay
            LEFT JOIN (
                SELECT unitid, institution_name, state, sector,
                       ROW_NUMBER() OVER (PARTITION BY unitid ORDER BY year DESC) as rn
                FROM hd
            ) h ON ay.unitid = h.unitid AND h.rn = 1
            WHERE ay.tuition_in_state IS NOT NULL
               OR ay.tuition_out_state IS NOT NULL
        """)
    except Exception as e:
        log.warning(f"Could not create v_tuition_trends: {e}")
    
    log.info("Created analytical views")


def create_metadata(con: duckdb.DuckDBPyConnection, survey_stats: dict):
    """Create a metadata table documenting the database."""
    
    # Table inventory
    tables = con.execute("""
        SELECT table_name, 
               estimated_size as row_count
        FROM duckdb_tables()
        WHERE schema_name = 'main'
        ORDER BY table_name
    """).fetchall()
    
    rows = []
    for table_name, _ in tables:
        if table_name == '_metadata':
            continue
        count = con.execute(f"SELECT COUNT(*) FROM {table_name}").fetchone()[0]
        cols = con.execute(f"SELECT COUNT(*) FROM information_schema.columns WHERE table_name = '{table_name}'").fetchone()[0]
        
        # Get year range
        try:
            year_range = con.execute(f"SELECT MIN(year), MAX(year) FROM {table_name}").fetchone()
            min_year, max_year = year_range
        except:
            min_year, max_year = None, None
        
        rows.append({
            'table_name': table_name,
            'row_count': count,
            'column_count': cols,
            'min_year': min_year,
            'max_year': max_year,
        })
    
    meta_df = pd.DataFrame(rows)
    con.execute("DROP TABLE IF EXISTS _metadata")
    con.register('_meta', meta_df)
    con.execute("CREATE TABLE _metadata AS SELECT * FROM _meta")
    con.unregister('_meta')


def main():
    if '--help' in sys.argv or '-h' in sys.argv:
        print(__doc__)
        print("Available tables:", ', '.join(sorted(build_manifest().keys())))
        sys.exit(0)
    
    # Remove existing DB
    if DB_PATH.exists():
        DB_PATH.unlink()
    
    con = duckdb.connect(str(DB_PATH))
    
    manifest = build_manifest()
    
    # Allow filtering to specific surveys via command line
    if len(sys.argv) > 1:
        selected = set(sys.argv[1:])
        manifest = {k: v for k, v in manifest.items() if k in selected}
    
    survey_stats = {}
    
    for survey, file_list in manifest.items():
        harmonizer = HARMONIZERS.get(survey, harmonize_generic)
        stats = process_survey(con, survey, file_list, harmonizer)
        survey_stats[survey] = stats
    
    # Create analytical views
    create_views(con)
    
    # Create metadata
    create_metadata(con, survey_stats)
    
    # Summary
    log.info(f"\n{'='*60}")
    log.info("DATABASE SUMMARY")
    log.info(f"{'='*60}")
    
    tables = con.execute("SELECT * FROM _metadata ORDER BY table_name").fetchdf()
    for _, row in tables.iterrows():
        yr = f"{int(row['min_year'])}-{int(row['max_year'])}" if row['min_year'] else "N/A"
        log.info(f"  {row['table_name']:15s}: {row['row_count']:>10,} rows | {row['column_count']:>4} cols | years {yr}")
    
    total_rows = tables['row_count'].sum()
    log.info(f"\n  Total rows: {total_rows:,}")
    
    # Log inconsistencies/notes
    log.info(f"\n{'='*60}")
    log.info("HARMONIZATION NOTES")
    log.info(f"{'='*60}")
    
    for survey, stats in survey_stats.items():
        if stats.get('failed_years'):
            log.info(f"  {survey}: failed years = {stats['failed_years']}")
    
    # Check for column count variation across years
    for survey, stats in survey_stats.items():
        if 'year_stats' in stats and stats['year_stats']:
            col_counts = [v['cols'] for v in stats['year_stats'].values()]
            if len(set(col_counts)) > 1:
                min_c, max_c = min(col_counts), max(col_counts)
                log.info(f"  {survey}: column count varies {min_c}-{max_c} across years (harmonized via union)")
    
    db_size = DB_PATH.stat().st_size / (1024 * 1024)
    log.info(f"\n  Database size: {db_size:.1f} MB")
    log.info(f"  Database path: {DB_PATH.absolute()}")
    
    con.close()
    log.info("Done!")


if __name__ == "__main__":
    main()
