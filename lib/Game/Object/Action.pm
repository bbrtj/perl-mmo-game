package Game::Object::Action;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param 'actor' => (
	lax_isa => InstanceOf ['Unit::Actor'],
);

has param 'lore_id' => (
	lax_isa => LoreId,
);

has param 'duration' => (
	lax_isa => PositiveNum,
);

has field 'start_time' => (
	lax_isa => PositiveNum,
	default => sub { server_time },
);

has field 'eta' => (
	lax_isa => PositiveNum,
	writer => 1,
);

has field 'cancelled' => (
	lax_isa => Bool,
	writer => 1,
	default => false,
);

sub server_method ($self)
{
	...;
}

sub BUILD ($self, $)
{
	$self->set_eta($self->start_time + $self->duration);

	return;
}

sub finished ($self, $time = server_time)
{
	return $time >= $self->eta || $self->cancelled;
}

