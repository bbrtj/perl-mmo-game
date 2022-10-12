-- Deploy gamedb:0002-players-table to pg

BEGIN;

CREATE TABLE players (
	id CHAR(26) primary key,
	user_id CHAR(26) NOT NULL,
	online BOOLEAN NOT NULL DEFAULT false,
	last_online TIMESTAMPTZ NULL,
	created_at TIMESTAMPTZ NOT NULL,
	CONSTRAINT fk_user
		FOREIGN KEY(user_id)
		REFERENCES users(id)
);

CREATE INDEX ind_users_user_id ON players (user_id);
CREATE INDEX ind_users_online ON players (online);

COMMIT;

