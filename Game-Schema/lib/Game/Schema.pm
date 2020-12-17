package Game::Schema;

use header;
use Game::Common::Container qw(resolve add_to_container);

use base qw(DBIx::Class::Schema);

our $VERSION = "0.001";

__PACKAGE__->load_namespaces();

sub bootstrap ($class)
{
	add_to_container('dbc', $class->connect(sub { resolve('db')->dbh }));
}

1;
