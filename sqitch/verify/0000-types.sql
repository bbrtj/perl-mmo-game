-- Verify gamedb:0000-types on pg

BEGIN;

DROP DOMAIN ulid;
DROP DOMAIN lore_id;

ROLLBACK;

