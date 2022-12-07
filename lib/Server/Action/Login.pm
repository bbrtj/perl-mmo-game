package Server::Action::Login;

use My::Moose;
use Form::Login;
use all 'Model';

use header;

extends 'Server::Action';

use constant name => 'login';
use constant required_state => Model::PlayerSession->STATE_NEW;
use constant deserializes => 1;

# only do basic validation here, leave the rest for a backend job
sub validate ($self, $data)
{
	die 'not a hash' unless is_hashref($data);
	return $data;
}

sub handle ($self, $session_id, $id, $data)
{
	my $form = Form::Login->new;
	$form->set_input($data);
	my $success = $form->valid;

	if ($success) {
		my $session = $self->cache_repo->load(PlayerSession => $session_id);

		$session->set_logged_in($form->user->id);

		$self->cache_repo->save($session);
	}

	$self->send_to(
		$session_id,
		$success || 0,
		id => $id,
		refresh => $success
	);

	return;
}

