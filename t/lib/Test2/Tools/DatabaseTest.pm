package Test2::Tools::DatabaseTest;

use Exporter qw(import);
use Test2::API qw(context);
use Test::DB;
use Utils;
use Component::DB;

use header;

our @EXPORT = qw(database_test);

sub database_test : prototype(&) ($sub)
{
	my $env = DI->get('env');
	my $testdb = Test::DB->new;

	local $ENV{TESTDB_DATABASE} = 'postgres';
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

		my $db = Component::DB->new(env => $env, dbh => $cloned->dbh);
		DI->set('db', $db);

		$sub->();
	}
	catch ($e) {
		my $ctx = context;
		$ctx->fail("fatal error during database testing: $e");
		$ctx->release;
	}

	# finally
	DI->get('db')->dbh->disconnect;
	$cloned->destroy;
	return;
}

