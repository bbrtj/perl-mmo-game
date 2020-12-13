-- 3 up

CREATE TABLE config (
	key VARCHAR(64) PRIMARY KEY,
	value TEXT
);

-- 3 down

DROP TABLE config;
