-- Verify gamedb:0002-charactervariables-table on pg

BEGIN;

SELECT
	id,
	experience,
	location_id,
	pos_x,
	pos_y,
	health,
	energy
FROM character_variables;

ROLLBACK;

