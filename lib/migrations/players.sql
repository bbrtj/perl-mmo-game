-- 5 up

CREATE TABLE users (
	id uuid primary key,
	email VARCHAR(128) NOT NULL,
	password VARCHAR(64) NOT NULL,
	salt VARCHAR(16) NOT NULL,
	status INT NOT NULL DEFAULT 1,
	created_at TIMESTAMP NOT NULL
);

CREATE INDEX ind_users_lookup ON users (email);

CREATE TABLE players (
	id uuid primary key,
	user_id uuid,
	class_id VARCHAR(20) NOT NULL,
	name VARCHAR(32) NOT NULL,
	online BOOLEAN NOT NULL DEFAULT false,
	created_at TIMESTAMP NOT NULL,
	last_online TIMESTAMP NULL,
	CONSTRAINT fk_user
		FOREIGN KEY(user_id)
		REFERENCES users(id),
	CONSTRAINT fk_class
		FOREIGN KEY(class_id)
		REFERENCES gd_classes(id)

);

CREATE INDEX ind_users_user_id ON players (user_id);
CREATE INDEX ind_users_online ON players (online);

-- 5 down

DROP TABLE players;
DROP TABLE users;
