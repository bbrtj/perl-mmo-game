-- Deploy gamedb:0000-types to pg

BEGIN;

CREATE DOMAIN ulid AS CHAR(26);
CREATE DOMAIN lore_id AS VARCHAR(32);

COMMIT;

