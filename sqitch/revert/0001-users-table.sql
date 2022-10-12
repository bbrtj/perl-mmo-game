-- Revert gamedb:0001-users-table from pg

BEGIN;

DROP TABLE users;

COMMIT;

