package Game::Schema::Result;

use Mojo::Base 'DBIx::Class::Core', -signatures;
use Game::Model;

sub to_model ($self)
{
	return Game::Model->from_result($self);
}

1;
