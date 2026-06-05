package Game::Object::Action;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param 'actor' => (

	# isa => InstanceOf ['Unit::Actor'],
);

has param 'lore_id' => (

	# isa => LoreId
);

has param 'duration' => (

	# isa => PositiveNum,
);

has field 'start_time' => (

	# isa => PositiveNum,
	default => sub { server_time },
);

has field 'eta' => (

	# isa => PositiveNum,
	writer => 1,
);

has field 'cancelled' => (

	# isa => Bool,
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

