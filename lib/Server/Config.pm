package Server::Config;

use header;

use constant {
	GAME_SERVER_PORT => 14832,
	GAME_SERVER_TIMEOUT => 30,

	PROTOCOL_CONTROL_CHARACTER => ';',
	PROTOCOL_MAX_LENGTH => 4 * 2 ** 10,
};

