package Server::Action::Login;

use My::Moose;
use Form::Login;

use header;

extends 'Server::Action';

use constant name => 'login';

sub handle ($self, $session_id, $id, $data)
{
	my $session = $self->cache->load(PlayerSession => $session_id);
	my $result = {success => 0};
	my $feedback = {echo => {n => $id, d => $result}};

	# TODO: check if the user is already logged in?
	my $form = Form::Login->new;
	$form->set_input($data);
	if ($form->valid) {
		$session->set_user_id($form->user->id);
		# TODO: proper language, or move languages to the client fully
		$session->set_language('pl');
		$self->cache->save($session);

		$result->{success} = 1;
		$feedback->{refresh} = 1;
	}

	return $self->send_to($session_id, $feedback);
};

