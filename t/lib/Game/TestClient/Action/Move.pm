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
		speed => Game::Config->config->{base_speed},    # TODO: how to get this?
		x => $self->x,
		y => $self->y,
		time => 1,    # don't care
	);

	return (
		Resource::ActorMovement->new(subject => $self->client->actor, movement => $movement),
	);
}

