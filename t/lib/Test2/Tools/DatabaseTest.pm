package Test2::Tools::DatabaseTest;

use Exporter qw(import);
use Test2::API qw(context);
use Test::DB;
use Utils;
use Component::DB;

use header;

our @EXPORT = qw(
	manual_database_test
	database_test
);

sub manual_database_test ()
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

	DI->forget('db');

	my $db = Component::DB->new(env => $env, dbh => $cloned->dbh);
	DI->set('db', $db);

	return sub {
		DI->get('db')->dbh->disconnect;
		$cloned->destroy;
	};
}

sub database_test : prototype(&) ($sub)
{
	my $cleanup = manual_database_test;

	defer {
		$cleanup->();
	}

	try {
		$sub->();
	}
	catch ($e) {
		my $ctx = context;
		$ctx->fail("fatal error during database testing: $e");
		$ctx->release;
	}

	return;
}

