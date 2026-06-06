package Game::Object::Projectile;

use My::Moose;
use Game::Mechanics::Generic;
use Game::Object::Effect;

use header;

has param 'actor' => (
	lax_isa => InstanceOf ['Unit::Actor'],
);

has param 'effect' => (
	lax_isa => InstanceOf ['Game::Object::Effect'],
);

has param 'speed' => (
	lax_isa => PositiveNum,
);

has param 'angle' => (
	lax_isa => Num,
);

has param 'time' => (
	lax_isa => PositiveOrZeroNum,
	writer => 1,
	default => sub { time },
);

has param 'max_distance' => (
	lax_isa => PositiveNum,
);

has param 'radius' => (
	lax_isa => PositiveNum,
);

has field 'eta' => (
	lax_isa => PositiveOrZeroNum,
	writer => 1,
);

has field 'discovered_by' => (
	lax_isa => ArrayRef [ULID],
	writer => 1,
);

# x, y of the destination
with qw(
	Role::Identified
	Game::Object::Role::HasPosition
);

sub BUILD ($self, $)
{
	$self->set_eta($self->time + $self->max_distance / $self->speed);
}

sub finished ($self)
{
	return $self->time >= $self->eta;
}

sub set_finished ($self)
{
	$self->set_eta(0);
	return;
}

