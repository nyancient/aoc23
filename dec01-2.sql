DROP TABLE IF EXISTS input;
CREATE TABLE input (input TEXT NOT NULL);
\copy input FROM 'input.txt';

CREATE OR REPLACE FUNCTION words_to_digits(str TEXT)
RETURNS TEXT
LANGUAGE PLPGSQL
AS $$
BEGIN
    FOR i IN 1..9 LOOP
        str = REPLACE(str, (ARRAY['one','two','three','four','five','six','seven','eight','nine'])[i], i :: TEXT);
    END LOOP;
    RETURN str;
END $$;

SELECT
    SUM(
        (
            RIGHT(
                words_to_digits(
                    REGEXP_REPLACE(input, '([0-9]|one|two|three|four|five|six|seven|eight|nine).*', '\1')
                ),
                1
            ) ||
            LEFT(
                words_to_digits(
                    REGEXP_REPLACE(input, '.*([0-9]|one|two|three|four|five|six|seven|eight|nine)', '\1')
                ),
                1
            )
        ) :: INTEGER
    )
FROM input;
