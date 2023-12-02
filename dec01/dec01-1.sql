DROP TABLE IF EXISTS input;
CREATE TABLE input (input TEXT NOT NULL);
\copy input FROM 'input.txt';
SELECT
    SUM(
        CASE LENGTH(result)
            WHEN 2 THEN result
            WHEN 1 THEN CONCAT(result, result)
        END :: INTEGER
    )
    FROM (SELECT REGEXP_REPLACE(input, '^[^0-9]*([0-9]?).*([0-9])[^0-9]*$', '\1\2') AS result FROM input);