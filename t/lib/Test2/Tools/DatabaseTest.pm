package Test2::Tools::DatabaseTest;

use Exporter qw(import);
use Test2::API qw(context);
use Test::DB;
use DI;
use Utils;
use Mojo::Pg;
use Component::DB;

use header -noclean;

our @EXPORT = qw(database_test);

sub database_test :prototype(&) ($sub)
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

		Utils->bootstrap_models;

		$sub->();
	}
	catch ($e) {
		my $ctx = context;
		$ctx->fail("fatal error during database testing: $e");
		$ctx->release;
	}

	# finally
	DI->get('db')->db->disconnect;
	$cloned->destroy;
	return;
}

