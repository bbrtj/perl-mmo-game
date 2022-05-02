package Resource::CharacterList;

use My::Moose;

use header;

extends 'Resource';

has '+subject' => (
	isa => Types::InstanceOf ['Unit::User'],
);

sub serialize ($self)
{
	my @characters = map {
		{
			id => $_->player->id,
			name => $_->character->name,
			class => $_->character->class_id,
			last_online => $_->player->last_online,
		}
	} $self->subject->players->@*;

	return {characters => \@characters};
}

