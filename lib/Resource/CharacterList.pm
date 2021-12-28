package Resource::CharacterList;

use My::Moose;
use Game::Lore::Character::Class;

use header;

extends 'Resource';

sub hash ($self)
{
	my @characters = map {
		name => $_->character->name,
		class => Game::Lore::Character::Class->get($_->character->class_id)->lore_name,
		last_online => $_->last_online,
	} $self->subject->players->@*;

	return {
		characters => \@characters,
		$self->more_data->%*,
	};
}
