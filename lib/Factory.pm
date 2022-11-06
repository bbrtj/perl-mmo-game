package Factory;

use My::Moose;

use header;

has injected 'db' => (
	handles => [
		qw(
			dbc
		)
	]
);

# free to use fields
sub fetch ($self, @)
{
	...;
}

# should not require an instantiated $self
sub create ($self, @)
{
	...;
}

