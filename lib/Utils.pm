package Utils;

use Mojo::File qw(path);
use Schema;
use Mojo::Pg;
use DI;
use MojoX::Log::Dispatch::Simple;
use Mojo::IOLoop;
use Game::LoreLoader;

use header;

sub bootstrap ($class, $app)
{
	my $config = DI->get('env');

	$app->log(
		MojoX::Log::Dispatch::Simple->new(
			dispatch => DI->get('log')->logger,
			level => 'debug',
		)
	);

	$class->bootstrap_models;
	$class->bootstrap_lore;

	return $config;
}

sub bootstrap_models ($class)
{
	$class->load_classes('Model', 'Model/*.pm');

	return;
}

sub bootstrap_lore ($class)
{
	$class->load_classes('Game::Lore', 'Game/Lore/*.pm');
	Game::LoreLoader->load_all;

	return;
}

sub bootstrap_mojo ($class)
{
	my $config = DI->get('env');

	my $reactor = Mojo::IOLoop->singleton->reactor;

	$reactor->unsubscribe('error');
	$reactor->on(error => sub ($reactor, $err) {
		DI->get('log')->critical($err);
	});

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

