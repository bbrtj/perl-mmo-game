-- Verify gamedb:0002-players-table on pg

BEGIN;

SELECT
	id,
	user_id,
	online,
	last_online,
	created_at
FROM players;

ROLLBACK;

