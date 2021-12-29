package Server::Worker::Action::ListCharacters;

use My::Moose;
use DI;
use Resource::CharacterList;

use header;

extends 'Server::Worker::Action';

use constant name => 'list_characters';

augment handle => sub ($self, $job, $id, $user_id, $data) {
	state $repo = DI->get('units');
	my $unit = $repo->get_user($user_id);

	return Resource::CharacterList->new($unit, id => $id);
};
