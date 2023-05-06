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

has field 'coeffs' => (

	# isa => Types::Tuple [Types::Num, Types::Num],
	writer => 1,
);

sub BUILD ($self, $)
{
	my $x_dist = $self->x - $self->variables->pos_x;
	my $y_dist = $self->y - $self->variables->pos_y;
	my $distance = Game::Mechanics::Generic->calculate_diagonal($x_dist, $y_dist);

	$self->set_eta($self->time + $distance / $self->speed);
	$self->set_coeffs($distance > 0 ? [$x_dist / $distance, $y_dist / $distance] : [0, 0]);

	return;
}

sub get_angle ($self)
{
	return Game::Mechanics::Generic->calculate_angle($self->coeffs->@*);
}

sub finished ($self)
{
	return $self->eta == $self->time;
}

