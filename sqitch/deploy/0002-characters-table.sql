-- Deploy gamedb:0002-characters-table to pg
-- requires: 0002-players-table

BEGIN;

CREATE TABLE characters (
	id ulid primary key,
	player_id ulid NULL,
	class_id lore_id NOT NULL,
	race_id lore_id NOT NULL,
	alliance_id lore_id NOT NULL,
	name VARCHAR(32) NOT NULL,
	CONSTRAINT fk_player
		FOREIGN KEY(player_id)
		REFERENCES players(id)
);

CREATE UNIQUE INDEX ind_characters_lookup_player ON characters (player_id);
CREATE UNIQUE INDEX ind_characters_name ON characters (name);

COMMIT;

