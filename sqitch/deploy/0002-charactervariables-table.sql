-- Deploy gamedb:0002-charactervariables-table to pg
-- requires: 0002-characters-table

BEGIN;

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

COMMIT;

