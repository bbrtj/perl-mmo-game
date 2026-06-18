-- Verify gamedb:0002-characters-table on pg

BEGIN;

SELECT
	id,
	player_id,
	class_id,
	race_id,
	alliance_id,
	name
FROM characters;

ROLLBACK;

