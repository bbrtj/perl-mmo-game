package Game::TestClient::Action::UseAbility;

use My::Moose;
use Game::Object::Action;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

use constant requires => ['EnterGame'];

has param 'actor' => (
	isa => InstanceOf ['Unit::Actor'],
);

has param 'lore' => (
	isa => InstanceOf ['Game::Lore::Ability'],
);

has param 'x' => (
	isa => Num,
	default => 0,
);

has param 'y' => (
	isa => Num,
	default => 0,
);

sub send_queue ($self)
{
	return (
		['use_ability', $self->lore->id, $self->x, $self->y],
	);
}

sub receive_queue ($self)
{
	# TODO: how to get duration here?
	my $action = Game::Object::Action::Ability->new(
		x => $self->x,
		y => $self->y,
		lore => $self->lore,
		actor => $self->actor,
		duration => 1,
	);

	return (
		Resource::ActorAction->new(subject => $action),
	);
}

