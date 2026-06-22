package Server::Action::Login;

use My::Moose;
use Form::Login;
use all 'Model';
use X::Pub;
use Resource::Success;

use header;

extends 'Server::Action';

with qw(Server::Role::HandlesLogin);

use constant name => 'login';
use constant required_state => Model::PlayerSession->STATE_NEW;
use constant deserializes => 1;

# only do basic validation here, leave the rest for a backend job
sub validate ($self, $data)
{
	die 'not a hash' unless ref $data eq 'HASH';
	return $data;
}

sub handle ($self, $session_id, $id, $data)
{
	my $form = Form::Login->new;
	$form->set_input($data);

	X::Pub->raise(Err::LOGIN_FAILED)
		unless $form->valid;

	$self->login($session_id, $form->user->id);

	$self->send_to(
		$session_id,
		Resource::Success->new,
		id => $id,
		refresh => true,
	);

	return;
}

