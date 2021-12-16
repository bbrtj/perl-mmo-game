-- 3 up

CREATE TABLE config (
	key VARCHAR(64) PRIMARY KEY,
	value TEXT
);

CREATE TABLE users (
	id CHAR(26) primary key,
	email VARCHAR(128) NOT NULL,
	password VARCHAR(64) NOT NULL,
	salt VARCHAR(16) NOT NULL,
	status INT NOT NULL DEFAULT 1,
	created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX ind_users_lookup ON users (email);

-- 3 down

DROP TABLE config;
DROP TABLE users;
