package Game::Object::Role::HasPosition;

use My::Moose::Role;

use header;

has param 'x' => (
	lax_isa => Num,
	writer => 1,
);

has param 'y' => (
	lax_isa => Num,
	writer => 1,
);

sub xy ($self)
{
	return ($self->x, $self->y);
}

