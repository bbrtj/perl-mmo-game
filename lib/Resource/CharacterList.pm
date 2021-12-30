package Resource::CharacterList;

use My::Moose;
use Game::Character::Class;

use header;

extends 'Resource';

sub hash ($self)
{
	my @characters = map {
		{
			name => $_->character->name,
			class => Game::Character::Class->get($_->character->class_id)->lore_name,
			last_online => $_->player->last_online,
		}
	} $self->subject->players->@*;

	return {
		characters => \@characters,
		$self->more_data->%*,
	};
}
