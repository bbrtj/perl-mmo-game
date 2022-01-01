package Server::Action::ListCharacters;

use My::Moose;
use DI;
use Resource::CharacterList;

use header;

extends 'Server::Action';

use constant name => 'list_characters';

augment handle => sub ($self, $id, $user_id, $data) {
	state $repo = DI->get('units');
	my $unit = $repo->get_user($user_id);
	inner;

	return Resource::CharacterList->new($unit, id => $id);
};
