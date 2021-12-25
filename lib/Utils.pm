package Utils;

use Mojo::File qw(path);
use Schema;
use Mojo::Pg;
use DI;

use header;

sub bootstrap ($class, $app)
{
	my $config = DI->get('env');
	$app->plugin(Minion => {Redis => $config->getenv('REDIS_CONNECTION')});
	my $pg = Mojo::Pg->new($config->getenv('DB_CONNECTION'));

	$class->bootstrap_models;

	return $config;
}

sub bootstrap_models ($class)
{
	$class->load_classes('Model', 'Model/*.pm');
}

sub load_classes ($class, $namespace, $pattern)
{
	if ($pattern !~ m{^/}) {
		$pattern = path((caller)[1])->dirname->to_string . "/$pattern";
	}

	my @classes = map { m{/(\w+)\.pm$}; "${namespace}::$1" } glob $pattern;
	for my $class (@classes) {
		my $success = eval "use $class; 1";
		die "error loading $class: $@" unless $success;
	}

	return @classes;
}

