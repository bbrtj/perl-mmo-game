-- 1 up

CREATE TABLE users (
	id CHAR(26) primary key,
	email VARCHAR(128) NOT NULL,
	password VARCHAR(60) NOT NULL,
	status INT NOT NULL DEFAULT 1,
	created_at TIMESTAMPTZ NOT NULL
);

CREATE INDEX ind_users_lookup ON users (email);

-- 1 down

DROP TABLE users;

