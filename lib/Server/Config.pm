package Server::Config;

use header;

use constant {

	# the minimum amount of time the server understands
	SERVER_TICK => 0.5,

	GAME_SERVER_PORT => 14832,
	GAME_SERVER_TIMEOUT => 30,

	PROTOCOL_CONTROL_CHARACTER => ';',
	PROTOCOL_SEPARATOR => '~',
	PROTOCOL_MAX_LENGTH => 4 * 2**10,

	DEBUG => DI->get('env')->getenv('DEBUG'),
};

