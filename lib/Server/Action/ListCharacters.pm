package Server::Action::ListCharacters;

use My::Moose;
use DI;
use Resource::CharacterList;

use header;

extends 'Server::Action';

use constant name => 'list_characters';

sub handle ($self, $session_id, $id, $data)
{
	state $repo = DI->get('units');
	# TODO: get user from redis
	my $unit = $repo->get_user($id);

	$self->send_to($session_id, Resource::CharacterList->new($unit, n => $id));
};
