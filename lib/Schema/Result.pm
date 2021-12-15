package Schema::Result;

use Game::Model;

use header;

use parent 'DBIx::Class::Core';

sub to_model ($self)
{
	return Game::Model->from_result($self);
}

