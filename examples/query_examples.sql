-- =============================================================
-- IPEDS Database: Example Queries
-- =============================================================
-- Open with: duckdb ipeds.duckdb < examples/query_examples.sql
-- Or paste individual queries into any DuckDB client.
-- =============================================================


-- 1. Database overview: what tables are available?
SELECT * FROM _metadata ORDER BY table_name;


-- 2. Admission rates at selective universities (2023)
SELECT
    a.year,
    h.institution_name,
    h.state,
    a.applicants_total,
    a.admissions_total,
    ROUND(a.admissions_total * 100.0 / a.applicants_total, 1) AS admit_rate
FROM adm a
JOIN hd h ON a.unitid = h.unitid AND a.year = h.year
WHERE a.year = 2023
  AND a.applicants_total > 20000
ORDER BY admit_rate ASC
LIMIT 20;


-- 3. Tuition trends at a specific university
SELECT year, tuition_in_state, tuition_out_state
FROM v_tuition_trends
WHERE institution_name ILIKE '%university of michigan%ann arbor%'
ORDER BY year;


-- 4. Total bachelor's degrees per year (all institutions)
SELECT
    year,
    COUNT(DISTINCT unitid) AS institutions,
    SUM(CAST(COALESCE(ctotalt, crace24, crace15 + crace16) AS BIGINT)) AS total_bachelors
FROM c_a
WHERE award_level = 5
GROUP BY year
ORDER BY year;


-- 5. MBA degrees over time (CIP 52.0201, award level 7 = Master's)
SELECT
    year,
    SUM(CAST(COALESCE(ctotalt, crace24, crace15 + crace16) AS BIGINT)) AS mba_degrees
FROM c_a
WHERE CAST(cipcode AS VARCHAR) = '52.0201'
  AND award_level = 7
GROUP BY year
ORDER BY year;


-- 6. Top 10 CIP codes for master's degrees in 2023
SELECT
    CAST(cipcode AS VARCHAR) AS cip,
    SUM(CAST(COALESCE(ctotalt, crace24, crace15 + crace16) AS BIGINT)) AS degrees
FROM c_a
WHERE award_level = 7 AND year = 2023
GROUP BY cip
ORDER BY degrees DESC
LIMIT 10;


-- 7. In-state vs out-of-state first-time undergrads (2022, full coverage year)
WITH inst_state AS (
    SELECT unitid, year, CAST(fips_state AS INT) AS fips
    FROM hd
)
SELECT
    SUM(CASE WHEN CAST(c.efcstate AS INT) = h.fips
             THEN CAST(c.efres01 AS BIGINT) END) AS in_state,
    SUM(CASE WHEN CAST(c.efcstate AS INT) != h.fips
              AND CAST(c.efcstate AS INT) NOT IN (90, 98, 99)
              AND c.line != 999
             THEN CAST(c.efres01 AS BIGINT) END) AS out_of_state,
    SUM(CASE WHEN CAST(c.efcstate AS INT) = 90
             THEN CAST(c.efres01 AS BIGINT) END) AS foreign_students,
    SUM(CASE WHEN CAST(c.efcstate AS INT) = 99
             THEN CAST(c.efres01 AS BIGINT) END) AS total
FROM ef_c c
JOIN inst_state h ON c.unitid = h.unitid AND c.year = h.year
WHERE c.year = 2022;


-- 8. Retention rates by sector over time
SELECT
    h.year,
    CASE h.control
        WHEN 1 THEN 'Public'
        WHEN 2 THEN 'Private nonprofit'
        WHEN 3 THEN 'Private for-profit'
    END AS sector,
    ROUND(AVG(d.ret_pcf), 1) AS avg_ft_retention_pct,
    COUNT(*) AS n_institutions
FROM ef_d d
JOIN hd h ON d.unitid = h.unitid AND d.year = h.year
WHERE d.ret_pcf IS NOT NULL AND d.ret_pcf > 0
GROUP BY h.year, h.control
ORDER BY h.year, h.control;


-- 9. Institutions that closed since 2010
SELECT institution_name, state, close_date, sector, control
FROM hd
WHERE close_date IS NOT NULL
  AND year >= 2010
ORDER BY close_date DESC
LIMIT 20;


-- 10. Top endowments (FY2023)
--     F1A = public institutions (GASB), F2 = private nonprofits (FASB)
--     f1h02/f2h02 = endowment value, end of fiscal year
SELECT institution_name, state, endowment_eoy,
       CASE WHEN source = 'f2' THEN 'Private' ELSE 'Public' END AS sector
FROM (
    SELECT h.institution_name, h.state, CAST(f.f2h02 AS BIGINT) AS endowment_eoy, 'f2' AS source
    FROM   f2 f JOIN hd h ON f.unitid = h.unitid AND h.year = 2023
    WHERE  f.year = 2023 AND f.f2h02 > 0
    UNION ALL
    SELECT h.institution_name, h.state, CAST(f.f1h02 AS BIGINT), 'f1a'
    FROM   f1a f JOIN hd h ON f.unitid = h.unitid AND h.year = 2023
    WHERE  f.year = 2023 AND f.f1h02 > 0
) combined
ORDER BY endowment_eoy DESC
LIMIT 20;


-- 11. Research doctorates by broad field (2024)
--      Award level 17 = Research/scholarship doctorate (2008+)
--      First 2 digits of CIP = broad field
SELECT
    LEFT(CAST(cipcode AS VARCHAR), 2) AS cip_2digit,
    SUM(CAST(COALESCE(ctotalt, crace24, crace15 + crace16) AS BIGINT)) AS doctorates
FROM c_a
WHERE award_level = 17 AND year = 2024
GROUP BY cip_2digit
ORDER BY doctorates DESC
LIMIT 15;
