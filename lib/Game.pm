package Game;

use header;
use Mojo::Base 'Mojolicious';
use Game::Common::Container;
use Game::Bootstrap;
use Game::Middleware;

no header;

# This method will run once at server start
sub startup ($self)
{
	my $config = bootstrap($self);

	load_config($self, $config);
	load_commands($self, $config);
	load_routes($self, $config);
	load_plugins($self, $config);
	load_helpers($self, $config);
}

sub load_config ($self, $config)
{
	# Configure the application
	$self->mode($config->{mode} // "development");
	$self->secrets($config->{secrets});
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

	$r->post('/user/login')->to('user#login');
	$r->post('/user/register')->to('user#register');

	$r->under('/api' => Game::Middleware->can('is_user'));
	$r->under('/api/game' => Game::Middleware->can('is_player'));
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

	$self->helper(
		dbc => sub ($self, $resultset) {
			state $schema = resolve('dbc');
			return $schema->resultset($resultset);
		}
	);
}

1;
