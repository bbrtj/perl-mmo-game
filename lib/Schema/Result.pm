package Schema::Result;

use Model;

use header;

use parent 'DBIx::Class::Core';

sub to_model ($self)
{
	return Model->from_result($self);
}

