-- 1 up

CREATE TABLE gd_languages (
	id VARCHAR(2) PRIMARY KEY,
	complete BOOLEAN NOT NULL DEFAULT false
);

CREATE TABLE gd_lores (
	id VARCHAR(20) PRIMARY KEY
);

CREATE TABLE gd_lore_names (
	id SERIAL PRIMARY KEY,
	lore_id VARCHAR(20) NOT NULL,
	language_id VARCHAR(2) NOT NULL,
	name VARCHAR(255) NOT NULL,
	CONSTRAINT fk_lore
		FOREIGN KEY(lore_id)
		REFERENCES gd_lores(id),
	CONSTRAINT fk_language
		FOREIGN KEY(language_id)
		REFERENCES gd_languages(id)
);

CREATE INDEX ind_lore_names_lookup ON gd_lore_names (language_id, lore_id);

CREATE TABLE gd_lore_descriptions (
	id SERIAL PRIMARY KEY,
	lore_id VARCHAR(20) NOT NULL,
	language_id VARCHAR(2) NOT NULL,
	description TEXT NOT NULL,
	CONSTRAINT fk_lore
		FOREIGN KEY(lore_id)
		REFERENCES gd_lores(id),
	CONSTRAINT fk_language
		FOREIGN KEY(language_id)
		REFERENCES gd_languages(id)
);

CREATE INDEX ind_lore_descriptions_lookup ON gd_lore_descriptions (language_id, lore_id);

CREATE TABLE gd_ability_effect_types (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_ability_attributes (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_abilities (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_classes (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_class_abilities (
	class_id VARCHAR(20) NOT NULL,
	ability_id VARCHAR(20) NOT NULL,
	CONSTRAINT fk_class
		FOREIGN KEY(class_id)
		REFERENCES gd_classes(id),
	CONSTRAINT fk_ability
		FOREIGN KEY(ability_id)
		REFERENCES gd_abilities(id),
	PRIMARY KEY (class_id, ability_id)
);

CREATE TABLE gd_areas (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_locations (
	id VARCHAR(20) PRIMARY KEY,
	area_id VARCHAR(20) NULL,
	CONSTRAINT fk_area_id
		FOREIGN KEY(area_id)
		REFERENCES gd_areas(id),
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE INDEX ind_locations_lookup ON gd_locations (area_id);

CREATE TABLE gd_location_paths (
	id SERIAL PRIMARY KEY,
	location_from_id VARCHAR(20) NOT NULL,
	location_to_id VARCHAR(20) NOT NULL,
	travel_distance FLOAT NOT NULL,
	CONSTRAINT fk_location_from_id
		FOREIGN KEY(location_from_id)
		REFERENCES gd_locations(id),
	CONSTRAINT fk_location_to_id
		FOREIGN KEY(location_to_id)
		REFERENCES gd_locations(id)
);

CREATE INDEX ind_location_paths_lookup_1 ON gd_location_paths (location_from_id, location_to_id);
CREATE INDEX ind_location_paths_lookup_2 ON gd_location_paths (location_to_id);

CREATE TABLE gd_ratings (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_statistics (
	id VARCHAR(20) PRIMARY KEY,
	is_primary BOOLEAN NOT NULL,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_item_weapons (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);

CREATE TABLE gd_item_armor (
	id VARCHAR(20) PRIMARY KEY,
	CONSTRAINT fk_lore
		FOREIGN KEY(id)
		REFERENCES gd_lores(id)
);



-- 1 down

DROP TABLE gd_item_weapons;
DROP TABLE gd_item_armor;
DROP TABLE gd_ratings;
DROP TABLE gd_statistics;
DROP TABLE gd_class_abilities;
DROP TABLE gd_abilities;
DROP TABLE gd_ability_effect_types;
DROP TABLE gd_ability_attributes;
DROP TABLE gd_location_paths;
DROP TABLE gd_classes;
DROP TABLE gd_locations;
DROP TABLE gd_areas;
DROP TABLE gd_lore_names;
DROP TABLE gd_lore_descriptions;
DROP TABLE gd_lores;
DROP TABLE gd_languages;
