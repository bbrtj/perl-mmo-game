package Utils;

use Mojo::File qw(path);
use Schema;
use Mojo::Pg;
use DI;
use MojoX::Log::Dispatch::Simple;

use header;

sub bootstrap ($class, $app)
{
	my $config = DI->get('env');
	$app->plugin('My::Mojolicious::Plugin::Minion');

	$app->log(
		MojoX::Log::Dispatch::Simple->new(
			dispatch => DI->get('log')->logger,
			level => 'debug',
		)
	);

	$class->bootstrap_models;

	return $config;
}

sub bootstrap_models ($class)
{
	$class->load_classes('Model', 'Model/*.pm');

	return;
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

