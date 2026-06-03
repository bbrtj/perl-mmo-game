package Game::TestClient::Action::UseAbility;

use My::Moose;
use Game::Object::Action;
use all 'Resource';

use header;

extends 'Game::TestClient::Action';

use constant requires => ['EnterGame'];

has param 'actor' => (
	isa => Types::InstanceOf ['Unit::Actor'],
);

has param 'lore_id' => (
	isa => Types::LoreId,
);

sub send_queue ($self)
{
	return (
		['use_ability', __serialize({lore_id => $self->lore_id})],
	);
}

sub receive_queue ($self)
{
	# TODO: how to get duration here?
	my $action = Game::Object::Action::Ability->new(
		lore_id => $self->lore_id,
		actor => $self->actor,
		duration => 1,
	);

	return (
		Resource::ActorAction->new(subject => $action),
	);
}

