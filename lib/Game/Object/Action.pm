package Game::Object::Action;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param 'method' => (

	# isa => Types::SimpleStr,
);

has param 'args' => (

	# isa => ArrayRef,
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

sub BUILD ($self, $)
{
	$self->set_eta($self->start_time + $self->duration);

	return;
}

sub finished ($self, $time = server_time)
{
	return $time >= $self->eta || $self->cancelled;
}

