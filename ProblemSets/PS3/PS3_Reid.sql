-- =========================
-- PS3_Reid.sql
-- Florida Insurance SQLite
-- =========================

.open fl_insurance.db
.mode csv
.import ProblemSets/PS3/FL_insurance_sample.csv insurance

.headers on
.mode column

SELECT * FROM insurance LIMIT 10;

SELECT DISTINCT county
FROM insurance
ORDER BY county;

SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation
FROM insurance;

SELECT construction, COUNT(*) AS n
FROM insurance
GROUP BY construction
ORDER BY n DESC;

SELECT construction,
       COUNT(*) AS n,
       COUNT(*) * 1.0 / (SELECT COUNT(*) FROM insurance) AS fraction
FROM insurance
GROUP BY construction
ORDER BY fraction DESC;

