package Game;

use Mojo::Base 'Mojolicious';
use Game::Bootstrap;
use DI;

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

	return;
}

sub load_config ($self, $env)
{
	# Configure the application
	$self->mode($env->getenv('APP_MODE'));
	$self->secrets($env->getenv('APP_SECRETS'));

	$self->log(DI->get('log'));

	return;
}

sub load_commands ($self, $env)
{
	push $self->commands->namespaces->@*, "Game::Command";

	return;
}

sub load_routes ($self, $env)
{
	my $r = $self->routes;

	my $main = $r->under('/')->to('middleware#stash_user');

	# Normal route to controller
	$main->get('/')->to('main#main_page');
	$main->get('/play')->to('main#play');

	my $user = $main->under('/user');
	$user->get('/login')->to('user#login_page');
	$user->post('/login')->to('user#login');
	$user->post('/logout')->to('user#logout');
	$user->get('/register')->to('user#register_page');
	$user->post('/register')->to('user#register');

	my $api = $main->under('/api')->to('middleware#is_user');
	my $game = $api->under('/game')->to('middleware#is_player');

	return;
}

sub load_plugins ($self, $env)
{
	return;
}

sub load_helpers ($self, $env)
{
	return;
}

