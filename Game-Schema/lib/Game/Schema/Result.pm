package Game::Schema::Result;

use header;
use Game::Model;

use base 'DBIx::Class::Core';

sub to_model ($self)
{
	return Game::Model->from_result($self);
}

1;
