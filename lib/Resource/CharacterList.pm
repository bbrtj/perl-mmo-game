use experimental 'class';

class Resource::CharacterList :isa(Resource);

use header;

use constant type => 'character_list';

field $user :param(subject);    # Unit::User

method generate ()
{
	my @characters = map {
		{
			id => $_->player->id,
			name => $_->character->name,
			class => $_->character->class_id,
			last_online => $_->player->last_online,
		}
	} $user->players->@*;

	return \@characters;
}

