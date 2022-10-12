package Component::DB;

use My::Moose;
use DBI;
use Schema;

use header;

with 'Component::Role::HasEnv';

has 'dbh' => (
	is => 'ro',
	isa => Types::InstanceOf ['DBI::db'],
	lazy => 1,
	default => sub ($self) {
		my $dbh = DBI->connect(
			$self->env->getenv('DB_CONNECTION'),
			$self->env->getenv('DB_USER'),
			$self->env->getenv('DB_PASS'),
		);

		return $dbh if $dbh;

		croak $DBI::errstr;
	},
);

has 'dbc' => (
	is => 'ro',
	isa => Types::InstanceOf ['Schema'],
	lazy => 1,
	default => sub ($self) {
		return Schema->connect(sub { $self->dbh });
	},
);

sub transaction ($self, $code)
{
	return $self->dbc->txn_do($code);
}

