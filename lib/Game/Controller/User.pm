package Game::Controller::User;

use Moo;
use Game::Form::Login;

use header;

extends 'Mojolicious::Controller';

sub login_page ($self)
{
}

sub login ($self)
{
}

sub register_page ($self)
{
}

sub register ($self)
{
}

sub logout ($self)
{
	delete $self->session->{user};
	delete $self->session->{player};
	$self->redirect_to('/');
}
