package Web;

use Moo;
use Utils;
use DI;

use header;

extends 'Mojolicious';

# This method will run once at server start
sub startup ($self)
{
	my $env = Utils->bootstrap($self);

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
	$self->secrets([split ',', $env->getenv('APP_SECRETS')]);

	$self->log(DI->get('log'));

	return;
}

sub load_commands ($self, $env)
{
	push $self->commands->namespaces->@*, "Web::Command";

	return;
}

sub load_routes ($self, $env)
{
	my $r = $self->routes;

	my $main = $r->under('/')->to('middleware#prepare_request');

	# Normal route to controller
	$main->get('/')->to('main#main_page');
	$main->get('/lang/:lang')->to('main#set_lang');
	$main->get('/play')->to('main#play');

	my $user = $main->under('/user');
	$user->any([qw(GET POST)] => '/login')->to('user#login');
	$user->post('/logout')->to('user#logout');
	$user->any([qw(GET POST)] => '/register')->to('user#register');

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
	$self->helper(_tt => sub { shift; _tt(@_) });
	$self->helper(render_lang => sub ($self, @args) {
		local $i18n::CURRENT_LANG = $self->session->{lang};
		return $self->render(@args);
	});
	return;
}

