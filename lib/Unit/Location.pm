package Unit::Location;

use My::Moose;

use header;

extends 'Unit';

has param 'actors' => (
	isa => Types::HashRef [Types::InstanceOf ['Unit::Actor']],
	'handles{}' => {
		'get_actor' => 'get',
	},
	default => sub { {} },
);

has param 'lore' => (
	isa => Types::InstanceOf ['Game::Lore::Location'],
);

sub get_player ($self, $actor_id)
{
	my $actor = $self->get_actor($actor_id);
	return $actor->is_player ? $actor : undef;
}

sub get_players ($self)
{
	return [grep { $_->is_player } values $self->actors->%*];
}

sub add_actor ($self, $actor)
{
	$self->actors->{$actor->id} = $actor;

	return;
}

sub remove_actor ($self, $actor)
{
	delete $self->actors->{$actor->id};

	return;
}

sub models ($self)
{
	return [
		map { $_->models->@* } grep { $_->is_player } values $self->actors->%*,
	];
}

