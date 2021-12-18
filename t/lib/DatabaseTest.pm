package DatabaseTest;

use Test::DB;
use Game::Common;
use DI;
use Game::Repository;
use Schema;
use Game::Model;
use Mojo::Pg;
use Component::DB;

use header;

sub test ($class, $sub)
{
	my $env = DI->get('env');
	my $testdb = Test::DB->new;

	$ENV{TESTDB_DATABASE} = 'postgres';
	my $cloned = $testdb->clone(
		hostname => $env->getenv('DB_HOST'),
		hostport => $env->getenv('DB_PORT'),
		username => $env->getenv('DB_USER'),
		password => $env->getenv('DB_PASS'),
		template => $env->getenv('DB_DATABASE'),
	);

	die 'database clone error' unless defined $cloned;

	try {
		DI->forget('db');

		my $pg = Mojo::Pg->new->dsn($cloned->dsn)
			->username($env->getenv('DB_USER'))
			->password($env->getenv('DB_PASS'));

		my $db = Component::DB->new(env => $env, dbh => $pg);
		DI->set('db', $db);

		Game::Model->bootstrap;

		$sub->();
	}
	catch ($e) {
		require Test::More;
		Test::More::fail("fatal error during database testing: $e");
	}

	# finally
	DI->get('db')->db->disconnect;
	$cloned->destroy;
	return;
}

