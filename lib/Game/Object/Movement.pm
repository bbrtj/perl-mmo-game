package Game::Object::Movement;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param ['x', 'y'] => (

	# isa => Types::PositiveNum,
);

has param 'variables' => (

	# isa => Types::InstanceOf ['Model::CharacterVariables'],
);

has param 'speed' => (

	# isa => Types::PositiveNum,
);

has param 'time' => (

	# isa => Types::PositiveNum,
	writer => 1,
);

has field 'eta' => (

	# isa => Types::PositiveNum,
	writer => 1,
);

has field 'angle' => (
	# isa => Types::Num,

	writer => 1,
);

sub BUILD ($self, $)
{
	my ($angle, $distance) = Game::Mechanics::Generic->calculate_angle_and_diagonal(
		$self->variables->pos_x, $self->variables->pos_y,
		$self->x, $self->y,
	);

	$self->set_eta($self->time + $distance / $self->speed);
	$self->set_angle($angle);

	return;
}

sub finished ($self)
{
	return $self->eta == $self->time;
}

