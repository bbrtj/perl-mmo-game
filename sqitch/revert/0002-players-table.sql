-- Revert gamedb:0002-players-table from pg

BEGIN;

DROP TABLE players;

COMMIT;

