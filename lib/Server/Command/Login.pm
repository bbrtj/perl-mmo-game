package Server::Command::Login;

use My::Moose;
use Form::Login;
use Model;

use header;

extends 'Server::Command';

use constant name => 'login';
use constant required_state => Model::PlayerSession->STATE_NEW;

# only do basic validation here, leave the rest for a backend job
sub validate ($self, $data)
{
	die 'not a hash' unless is_hashref($data);
	return $data;
}

sub handle ($self, $session_id, $id, $data)
{
	my $form = Form::Login->new(input => $data);
	my $success = $form->valid;

	if ($success) {
		my $session = $self->cache->load(PlayerSession => $session_id);

		$session->set_user_id($form->user->id);
		$session->set_state($session->STATE_LOGGED_IN);

		$self->cache->save($session);
	}

	return $self->send_to(
		$session_id,
		{success => $success},
		id => $id,
		refresh => $success
	);
}

