-- Revert gamedb:0002-characters-table from pg

BEGIN;

DROP TABLE characters;

COMMIT;

