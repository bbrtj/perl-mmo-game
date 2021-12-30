package Server::Config;

use header;

use constant {

	# NOTE: maintain a list of currently available actions here
	actions => {
		map { $_ => 1 }
			qw(
			list_characters
			)
	},
};
