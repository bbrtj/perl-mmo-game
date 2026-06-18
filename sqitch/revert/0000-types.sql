-- Revert gamedb:0000-types from pg

BEGIN;

DROP DOMAIN ulid;
DROP DOMAIN lore_id;

COMMIT;

