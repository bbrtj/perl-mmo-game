package Game;

use Mojo::Base 'Mojolicious';
use Game::Bootstrap;

use header;

# This method will run once at server start
sub startup ($self)
{
	my $env = bootstrap($self);

	load_config($self, $env);
	load_commands($self, $env);
	load_routes($self, $env);
	load_plugins($self, $env);
	load_helpers($self, $env);
}

sub load_config ($self, $env)
{
	# Configure the application
	$self->mode($env->getenv('APP_MODE'));
	$self->secrets($env->getenv('APP_SECRETS'));
}

sub load_commands ($self, $env)
{
	push $self->commands->namespaces->@*, "Game::Command";
}

sub load_routes ($self, $env)
{
	my $r = $self->routes;

	# Normal route to controller
	$r->get('/')->to('main#index');

	$r->post('/user/login')->to('user#login');
	$r->post('/user/register')->to('user#register');

	$r->under('/api')->to('middleware#is_user');
	$r->under('/api/game')->to('middleware#is_player');
}

sub load_plugins ($self, $env)
{
}

sub load_helpers ($self, $env)
{
}

