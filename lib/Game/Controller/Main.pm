package Game::Controller::Main;

use Mojo::Base 'Mojolicious::Controller', -signatures;

# This action will render a template
sub index($self)
{
	$self->minion->enqueue('test');
	$self->render(msg => 'Welcome to the Mojolicious real-time web framework!');
}

1;
