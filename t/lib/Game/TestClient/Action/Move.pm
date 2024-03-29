package Game::TestClient::Action::Move;

use My::Moose;
use Game::Config;
use Game::Object::Movement;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

use constant requires => ['EnterGame'];

has param ['x', 'y'] => (
	isa => Types::PositiveNum,
);

sub send_queue ($self)
{
	return (
		['move', $self->x, $self->y],
	);
}

sub receive_queue ($self)
{
	my $movement = Game::Object::Movement->new(
		variables => $self->client->actor->variables,
		speed => $self->client->actor->stats->speed,
		x => $self->x,
		y => $self->y,
		time => 1,    # don't care
	);

	$self->client->actor->stats->set_movement($movement);

	return (
		Resource::ActorMovement->new(subject => $self->client->actor),
	);
}

