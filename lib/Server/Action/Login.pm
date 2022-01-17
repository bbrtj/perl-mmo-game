package Server::Action::Login;

use My::Moose;
use Web::Form::Login;

use header;

extends 'Server::Action';

use constant name => 'login';

augment handle => sub ($self, $session_id, $id, $data) {
	my $result = {success => 0};
	my $feedback = {echo => {n => $id, d => $result}};

	# TODO: check if the user is already logged in?
	my $form = Web::Form::Login->new;
	$form->set_input($data);
	if ($form->valid) {
		$result->{success} = 1;
		$feedback->{login} = $form->user->id;
		# TODO: set user in redis for this session_id
	}

	return $feedback;
};
