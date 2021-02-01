package Game::Controller::User;

use header;
use Mojo::Base 'Mojolicious::Controller';
use Game::Form::Login;

no header;

sub login ($self)
{
	$self->render(json => 'Welcome to the Mojolicious real-time web framework!');
}

1;
