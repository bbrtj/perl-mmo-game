-- 2 up

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

CREATE TABLE character_variables (
	id CHAR(26) primary key,
	experience BIGINT NOT NULL DEFAULT 0,
	location_id VARCHAR(32) NOT NULL,
	pos_x FLOAT NOT NULL,
	pos_y FLOAT NOT NULL,
	health FLOAT NOT NULL,
	energy FLOAT NOT NULL,
	CONSTRAINT fk_character
		FOREIGN KEY(id)
		REFERENCES characters(id)
);

CREATE INDEX ind_character_variables_lookup_location ON character_variables (location_id);

-- 2 down

DROP TABLE character_variables;
DROP TABLE characters;
DROP TABLE players;

