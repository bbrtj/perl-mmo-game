package Factory;

use My::Moose;
use DI;

use header;

has 'dbc' => (
	is => 'ro',
	default => sub { DI->get('db')->dbc },
);

# free to use fields
sub fetch ($self, @)
{
	...
}

# should not require an instantiated $self
sub create ($self, @)
{
	...
}

