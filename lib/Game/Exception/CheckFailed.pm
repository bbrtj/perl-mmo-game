package Game::Exception::CheckFailed;

use Moo;

use header;

extends 'Game::ExceptionBase';

has 'message' => (
	is => 'ro',
);

