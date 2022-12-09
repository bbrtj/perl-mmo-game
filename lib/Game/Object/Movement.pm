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
	my ($x1, $y1) = ($self->variables->pos_x, $self->variables->pos_y);
	my ($x2, $y2) = ($self->x, $self->y);
	my $distance = Game::Mechanics::Generic->calculate_distance($x1, $y1, $x2, $y2);

	$self->set_eta($self->time + $distance / $self->speed);
	$self->set_coeffs([($x2 - $x1) / $distance, ($y2 - $y1) / $distance]);

	return;
}

