package Game::Controller::Main;

use Mojo::Base 'Mojolicious::Controller';

use header;

# This action will render a template
sub main_page ($self)
{
	$self->minion->enqueue('test');
	$self->render(msg => 'Welcome to the Mojolicious real-time web framework!');

	return;
}

