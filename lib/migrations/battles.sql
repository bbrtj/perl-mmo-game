-- 6 up

CREATE TABLE battles (
	id CHAR(26) primary key,
	location_id VARCHAR(20) NULL,
	size_x INT NOT NULL,
	size_y INT NOT NULL,
	turn INT NOT NULL DEFAULT 0,
	CONSTRAINT fk_location_id
		FOREIGN KEY(location_id)
		REFERENCES gd_locations(id)
);

CREATE INDEX ind_battles_lookup ON battles (location_id);

CREATE TABLE battle_contestants (
	id CHAR(26) primary key,
	battle_id CHAR(26) NOT NULL,
	character_id CHAR(26) NOT NULL,
	pos_x FLOAT NOT NULL,
	pos_y FLOAT NOT NULL,
	initiative INT NOT NULL,
	team INT NOT NULL,
	CONSTRAINT fk_battle_id
		FOREIGN KEY(battle_id)
		REFERENCES battles(id),
	CONSTRAINT fk_character_id
		FOREIGN KEY(character_id)
		REFERENCES characters(id)
);

CREATE INDEX ind_battle_contestants_lookup ON battle_contestants (battle_id, character_id);

-- 6 down

DROP TABLE battle_contestants;
DROP TABLE battles;
