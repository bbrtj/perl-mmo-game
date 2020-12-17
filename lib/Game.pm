package Game;

use Mojo::Base 'Mojolicious', -signatures;
use Mojo::Pg;
use Game::Common::Container;
use Game::Common;

# This method will run once at server start
sub startup ($self)
{
	Game::Common->bootstrap($self, 'vars.conf');

	my $config = load_config($self);
	load_commands($self, $config);
	load_routes($self, $config);
	load_plugins($self, $config);
	load_helpers($self, $config);
}

sub load_config ($self)
{
	my $config = resolve('config');

	# Configure the application
	$self->mode($config->{mode} // "development");
	$self->secrets($config->{secrets});

	return $config;
}

sub load_commands ($self, $config)
{
	push $self->commands->namespaces->@*, "Game::Command";
}

sub load_routes ($self, $config)
{
	my $r = $self->routes;

	# Normal route to controller
	$r->get('/')->to('main#index');
}

sub load_plugins ($self, $config)
{
}

sub load_helpers ($self, $config)
{
	$self->helper(
		db => sub {
			state $pg = resolve('pg');
		}
	);
}

1;
