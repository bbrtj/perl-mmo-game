-- Revert gamedb:0002-charactervariables-table from pg

BEGIN;

DROP TABLE character_variables;

COMMIT;

