package DatabaseTest;

use v5.30;
use warnings;
use Test::DB;
use Game::Common;
use Game::Common::Container qw(add_to_container);
use Game::Repository;
use Game::Schema;
use Mojo::Pg;
use Syntax::Keyword::Try;
use Mojolicious::Lite;

sub test
{
	my ($class, $sub) = @_;

	my $config = Game::Common->get_config(app, '../../vars.conf');
	my $testdb = Test::DB->new;

	$ENV{TESTDB_DATABASE} = 'postgres';
	my $cloned = $testdb->clone(
		hostname => $config->{db}{host},
		hostport => $config->{db}{port},
		username => $config->{db}{username},
		password => $config->{db}{password},
		template => $config->{db}{database},
	);

	die 'database clone error' unless defined $cloned;

	my $db = Mojo::Pg->new->dsn($cloned->dsn)
		->username($config->{db}{username})
		->password($config->{db}{password})
		->db;
	add_to_container('db', $db);

	Game::Repository->bootstrap;
	Game::Schema->bootstrap;

	try {
		$sub->();
	}
	finally {
		$db->disconnect;
		$cloned->destroy;
	}
}

1;
