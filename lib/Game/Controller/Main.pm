package Game::Controller::Main;

use Moo;

use header;

extends 'Mojolicious::Controller';

sub main_page ($self)
{
	$self->render('main/main_page');

	return;
}

sub play ($self)
{
}

