package Game::Object::Movement;

use PCClass qw(actor x y speed time x_coeff y_coeff map);
use Game::Mechanics::Generic;

use header;

sub new_with_coeffs ($class, %params)
{
	my $self = $class->new(%params);

	my $variables = $self->actor->variables;
	my $distance = Game::Mechanics::Generic->calculate_distance(
		$variables->pos_x,
		$variables->pos_y,
		$self->x,
		$self->y
	);

	$self->set_x_coeff(($variables->pos_x - $self->x) / $distance);
	$self->set_y_coeff(($variables->pos_y - $self->y) / $distance);

	return $self;
}

