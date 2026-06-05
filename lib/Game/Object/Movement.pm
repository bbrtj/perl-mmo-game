package Game::Object::Movement;

use My::Moose;
use Game::Mechanics::Generic;

use header;

has param ['x', 'y'] => (
	lax_isa => PositiveNum,
);

has param 'variables' => (
	lax_isa => InstanceOf ['Model::CharacterVariables'],
);

has param 'speed' => (
	lax_isa => PositiveNum,
);

has param 'time' => (
	lax_isa => PositiveOrZeroNum,
	writer => 1,
);

has field 'eta' => (
	lax_isa => PositiveNum,
	writer => 1,
);

has field 'angle' => (
	lax_isa => Num,
	writer => 1,
);

sub BUILD ($self, $)
{
	my ($angle, $distance) = Game::Mechanics::Generic->calculate_angle_and_diagonal(
		$self->variables->xy,
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

