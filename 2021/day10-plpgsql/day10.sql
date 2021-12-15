DROP TABLE IF EXISTS input;
CREATE TABLE input(
    line varchar
);

DROP TABLE IF EXISTS error_scores;
CREATE TABLE error_scores(
    ch varchar,
    score int
);

INSERT INTO error_scores(ch, score) VALUES (')', 3), (']', 57), ('}', 1197), ('>', 25137);

COPY input FROM '/inputs/10.input';

DROP FUNCTION IF EXISTS first_illegal_character;
CREATE OR REPLACE FUNCTION first_illegal_character(str varchar)
RETURNS varchar
AS $$
DECLARE
    curr varchar := '';
    ch varchar := ' ';
    ans varchar := '';
BEGIN
    CREATE TABLE stack (
        id SERIAL,
        elem varchar
    );

    FOREACH ch IN ARRAY regexp_split_to_array(str, '') LOOP
    IF ch = '(' OR ch = '[' OR ch = '{' OR ch = '<' THEN
        INSERT INTO stack(elem) VALUES(ch);
    ELSE
        curr := (SELECT elem FROM stack ORDER BY id DESC LIMIT 1);
        IF (curr = '(' AND ch <> ')') OR (curr = '[' AND ch <> ']') OR (curr = '{' AND ch <> '}') OR (curr = '<' AND ch <> '>') THEN
            ans := ch;
            EXIT;
        END IF;
        DELETE FROM stack WHERE id IN (SELECT id FROM stack ORDER BY id DESC LIMIT 1);
    END IF;
    END LOOP;

    DROP TABLE stack;

    RETURN ans;
END;
$$ language 'plpgsql';

DROP FUNCTION IF EXISTS completion_score;
CREATE OR REPLACE FUNCTION completion_score(str varchar)
RETURNS bigint
AS $$
DECLARE
    score bigint := 0;
    curr varchar := '';
    ch varchar := ' ';
BEGIN
    CREATE TABLE stack (
        id SERIAL,
        elem varchar
    );

    FOREACH ch IN ARRAY regexp_split_to_array(str, '') LOOP
    IF ch = '(' OR ch = '[' OR ch = '{' OR ch = '<' THEN
        INSERT INTO stack(elem) VALUES(ch);
    ELSE
        DELETE FROM stack WHERE id IN (SELECT id FROM stack ORDER BY id DESC LIMIT 1);
    END IF;
    END LOOP;

    WHILE (SELECT COUNT(*) FROM stack) > 0 LOOP
        curr := (SELECT elem FROM stack ORDER BY id DESC LIMIT 1);
        score := score * 5;
        IF curr = '(' THEN
            score := score + 1;
        ELSIF curr = '[' THEN
            score := score + 2;
        ELSIF curr = '{' THEN
            score := score + 3;
        ELSIF curr = '<' THEN
            score := score + 4;
        END IF;
        DELETE FROM stack WHERE id IN (SELECT id FROM stack ORDER BY id DESC LIMIT 1);
    END LOOP;

    DROP TABLE stack;
    RETURN score;
END;
$$ language 'plpgsql';

DROP FUNCTION IF EXISTS part1;
CREATE FUNCTION part1()
RETURNS int
AS $$
DECLARE
    ans int := 0;
BEGIN
    ans := (SELECT SUM(t.score) FROM (
        SELECT i.*, illegal_ch, e.score
        FROM input i, LATERAL first_illegal_character(i.line) illegal_ch
        INNER JOIN error_scores e ON e.ch = illegal_ch) t);
    RAISE NOTICE 'Part 1: %', ans;
    RETURN ans;
END;
$$ language 'plpgsql';

DROP FUNCTION IF EXISTS part2;
CREATE FUNCTION part2()
RETURNS bigint
AS $$
DECLARE
    ans bigint := 0;
BEGIN
    DROP VIEW IF EXISTS tmp_scores;
    CREATE VIEW tmp_scores
    AS (SELECT i.*, score
        FROM input i, LATERAL first_illegal_character(i.line) illegal_ch, LATERAL completion_score(i.line) score
        WHERE illegal_ch = '');
    ans := (SELECT PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY score) FROM tmp_scores);
    RAISE NOTICE 'Part 2: %', ans;
    RETURN ans;
END;
$$ language 'plpgsql';

SELECT part1();
SELECT part2();
