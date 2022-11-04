package Utils;

use Model;
use Game::Lore;

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

	return $config;
}

sub bootstrap_lore ($class)
{
	Game::LoreLoader->load_all;

	return;
}

sub bootstrap_mojo ($class)
{
	my $config = DI->get('env');
	$class->handle_errors;

	return;
}

sub handle_errors ($class)
{
	my $reactor = Mojo::IOLoop->singleton->reactor;

	$reactor->unsubscribe('error');
	$reactor->on(
		error => sub ($reactor, $err) {
			DI->get('log')->critical($err);
		}
	);
}

