package Game::Controller::User;

use Mojo::Base 'Mojolicious::Controller';
use Game::Form::Login;

use header;

sub login ($self)
{
	$self->render(json => 'Welcome to the Mojolicious real-time web framework!');
	return;
}

