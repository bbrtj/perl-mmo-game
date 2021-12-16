package Game::Exception::CheckFailed;

use Moo;

use header;

extends 'Game::Exception';

has 'message' => (
	is => 'ro',
);

