-- Verify gamedb:0002-characters-table on pg

BEGIN;

SELECT
	id,
	player_id,
	npc_id,
	class_id,
	name,
	base_stats
FROM characters;

ROLLBACK;

