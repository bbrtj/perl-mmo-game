package Game;

use Mojo::Base 'Mojolicious';
use Game::Bootstrap;
use Mojo::Log;
use Mojo::File qw(curfile);

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

	my $log = Mojo::Log->new(
		path => curfile->dirname->sibling('logs')->child('application.log'),
		level => 'error',
	);
	$self->log($log);

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

	# Normal route to controller
	$r->get('/')->to('main#main_page');

	$r->post('/user/login')->to('user#login');
	$r->post('/user/register')->to('user#register');

	$r->under('/api')->to('middleware#is_user');
	$r->under('/api/game')->to('middleware#is_player');

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

