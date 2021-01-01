package Game::Exception::CheckFailed;

use header;
use Moo;

extends 'Game::ExceptionBase';

has 'message' => (
	is => 'ro',
);

1;
