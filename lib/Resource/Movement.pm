package Resource::Movement;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::Maybe [Types::InstanceOf ['Game::Object::Movement']],
);

sub serialize ($self)
{
	my $subject = $self->subject;

	return 0 unless $subject;

	return {
		speed => $subject->speed,
		x => $subject->variables->pos_x,
		y => $subject->variables->pos_y,
	};
}

