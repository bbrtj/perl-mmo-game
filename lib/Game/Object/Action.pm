package Game::Object::Action;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param 'actor' => (

	# isa => Types::InstanceOf ['Unit::Actor'],
);

has param 'lore_id' => (

	# isa => Types::LoreId
);

has param 'duration' => (

	# isa => Types::PositiveNum,
);

has field 'start_time' => (

	# isa => Types::PositiveNum,
	default => sub { server_time },
);

has field 'eta' => (

	# isa => Types::PositiveNum,
	writer => 1,
);

has field 'cancelled' => (

	# isa => Types::Bool,
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

