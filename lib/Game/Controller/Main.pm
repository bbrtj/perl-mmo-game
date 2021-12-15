package Game::Controller::Main;

use Mojo::Base 'Mojolicious::Controller';

use header;

# This action will render a template
sub index ($self)
{
	$self->minion->enqueue('test');
	$self->render(msg => 'Welcome to the Mojolicious real-time web framework!');
}

