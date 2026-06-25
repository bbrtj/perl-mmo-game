package Server::Config;

use header;

use constant {

	# the minimum amount of time the server understands, in seconds
	TICK => 0.05,

	TRANSPORT_FLOAT_PRECISION => 1e4,

	GAME_SERVER_PORT => 14832,
	GAME_SERVER_TIMEOUT => 120,

	PROTOCOL_CONTROL_CHARACTER => ';',
	PROTOCOL_SEPARATOR => '~',
	PROTOCOL_MAX_LENGTH => 4 * 2**10,

	DEBUG => DI->get('env')->getenv('DEBUG'),
};

