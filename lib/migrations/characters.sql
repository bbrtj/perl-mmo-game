-- 5 up

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

CREATE TABLE characters (
	id CHAR(26) primary key,
	player_id CHAR(26) NULL,
	npc_id CHAR(26) NULL,
	class_id VARCHAR(20) NOT NULL,
	name VARCHAR(32) NOT NULL,
	stats VARCHAR NOT NULL,
	CONSTRAINT fk_player
		FOREIGN KEY(player_id)
		REFERENCES players(id),
	CONSTRAINT fk_class
		FOREIGN KEY(class_id)
		REFERENCES gd_classes(id)
);

CREATE UNIQUE INDEX ind_characters_lookup_player ON characters (player_id);
CREATE INDEX ind_characters_lookup_npc ON characters (npc_id);

CREATE TABLE character_variables (
	id CHAR(26) primary key,
	experience BIGINT NOT NULL DEFAULT 0,
	location VARCHAR(20) NOT NULL,
	health FLOAT NOT NULL,
	mana FLOAT NOT NULL,
	CONSTRAINT fk_character
		FOREIGN KEY(id)
		REFERENCES characters(id)
);

CREATE INDEX ind_character_variables_lookup_location ON character_variables (location);

-- this can be moved to redis later
-- (no foreign key for this reason)
CREATE TABLE character_cache (
	id CHAR(26) primary key,
	data VARCHAR NOT NULL
);

-- 5 down

DROP TABLE character_cache;
DROP TABLE character_variables;
DROP TABLE characters;
DROP TABLE players;
