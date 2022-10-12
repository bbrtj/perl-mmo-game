-- Verify gamedb:0001-users-table on pg

BEGIN;

SELECT
	id,
	email,
	password,
	status,
	created_at
FROM users;

ROLLBACK;

