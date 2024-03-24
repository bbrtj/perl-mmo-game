package Resource::ActorsInfo;

use My::Moose;

use header;

extends 'Resource';

has extended 'subject' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Unit::Actor']],
);

use constant type => 'actors_info';

sub generate ($self)
{
	# TODO: more basic info
	my @actors = map {
		{
			id => $_->player->id,
			name => $_->character->name,
			class => $_->character->class_id,
		}
	} $self->subject->@*;

	return \@actors;
}

