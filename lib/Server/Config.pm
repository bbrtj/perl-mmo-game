package Server::Config;

use header;

use constant {

	# the minimum amount of time the server understands
	# (as BPM: 60 / BPM)
	SERVER_TICK => 60 / 180,

	GAME_SERVER_PORT => 14832,
	GAME_SERVER_TIMEOUT => 120,

	PROTOCOL_CONTROL_CHARACTER => ';',
	PROTOCOL_SEPARATOR => '~',
	PROTOCOL_MAX_LENGTH => 4 * 2**10,

	DEBUG => DI->get('env')->getenv('DEBUG'),
};

