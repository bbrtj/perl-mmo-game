package Web::Message::ListCharacters;

use My::Moose;

use header;

extends 'Web::Message';

with qw(Web::Message::Role::WithRequest);

sub handle ($self, $user, $data)
{
	state $repo = DI->get('units');
	my $unit = $repo->get_user($user->id);

	return Resource::CharacterList->new($unit, id => $self->request_id);
}
