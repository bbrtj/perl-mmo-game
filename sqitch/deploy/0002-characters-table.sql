-- Deploy gamedb:0002-characters-table to pg
-- requires: 0002-players-table

BEGIN;

CREATE TABLE characters (
	id CHAR(26) primary key,
	player_id CHAR(26) NULL,
	npc_id VARCHAR(32) NULL,
	class_id VARCHAR(32) NOT NULL,
	name VARCHAR(32) NOT NULL,
	base_stats VARCHAR NOT NULL,
	CONSTRAINT fk_player
		FOREIGN KEY(player_id)
		REFERENCES players(id)
);

CREATE UNIQUE INDEX ind_characters_lookup_player ON characters (player_id);
CREATE UNIQUE INDEX ind_characters_name ON characters (name);

COMMIT;

