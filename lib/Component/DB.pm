package Component::DB;

use My::Moose;
use DBI;
use Schema;

use header;

with 'Component::Role::HasEnv';

has param 'dbh' => (
	isa => Types::InstanceOf ['DBI::db'],
	lazy => sub ($self) {
		my $dbh = DBI->connect(
			$self->env->getenv('DB_CONNECTION'),
			$self->env->getenv('DB_USER'),
			$self->env->getenv('DB_PASS'),
			{RaiseError => 1},
		);

		return $dbh if $dbh;

		croak $DBI::errstr;
	},
);

has param 'dbc' => (
	isa => Types::InstanceOf ['Schema'],
	lazy => sub ($self) {
		return Schema->connect(sub { $self->dbh });
	},
);

sub transaction ($self, $code)
{
	return $self->dbc->txn_do($code);
}

