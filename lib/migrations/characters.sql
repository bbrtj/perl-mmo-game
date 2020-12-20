-- 5 up

CREATE TABLE players (
	id uuid primary key,
	user_id uuid,
	online BOOLEAN NOT NULL DEFAULT false,
	last_online TIMESTAMP NULL,
	created_at TIMESTAMP NOT NULL,
	CONSTRAINT fk_user
		FOREIGN KEY(user_id)
		REFERENCES users(id)
);

CREATE INDEX ind_users_user_id ON players (user_id);
CREATE INDEX ind_users_online ON players (online);

CREATE TABLE characters (
	id uuid primary key,
	player_id uuid NOT NULL,
	npc_id uuid NOT NULL,
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

CREATE INDEX ind_characters_lookup_player ON characters (player_id);
CREATE INDEX ind_characters_lookup_npc ON characters (npc_id);

CREATE TABLE character_variables (
	id uuid primary key,
	experience BIGINT NOT NULL DEFAULT 0,
	location VARCHAR(20) NOT NULL,
	health FLOAT NOT NULL,
	focus FLOAT NOT NULL,
	CONSTRAINT fk_character
		FOREIGN KEY(id)
		REFERENCES characters(id)
);

CREATE INDEX ind_character_variables_lookup_location ON character_variables (location);

-- this can be moved to redis later
-- (no foreign key for this reason)
CREATE TABLE character_calculations (
	id uuid primary key,
	level INT NOT NULL,
	health_max INT NOT NULL,
	health_regen FLOAT NOT NULL,
	focus_max INT NOT NULL,
	focus_regen FLOAT NOT NULL,
	stats VARCHAR NOT NULL
);

-- 5 down

DROP TABLE character_calculations;
DROP TABLE character_variables;
DROP TABLE characters;
DROP TABLE players;
