package Resource::ActorsInfo;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => ArrayRef [InstanceOf ['Unit::Actor']],
);

use constant type => 'actors_info';

sub generate ($self)
{
	# TODO: more basic info
	my @actors = map {
		{
			id => $_->id,
			name => $_->character->name,
			class => $_->character->class_id,
			player => $_->is_player,
		}
	} $self->subject->@*;

	return \@actors;
}

