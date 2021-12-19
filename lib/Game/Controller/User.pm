package Game::Controller::User;

use Moo;
use Game::Form::Login;
use Game::Form::Register;

use header;

extends 'Mojolicious::Controller';

sub login ($self)
{
	my $form = Game::Form::Login->new;

	if ($self->req->method eq 'POST') {
		# TODO: csrf
		$form->set_input($self->req->body_params->to_hash);

		if ($form->valid) {
			# TODO: referrer

			$self->session->{user} = $form->user->id;
			$self->redirect_to('/');
			return;
		}
		else {
			# TODO: throttle
		}
	}

	$self->stash('form', $form);
	$self->render('user/login');

	return;
}

sub register ($self)
{
}

sub logout ($self)
{
	delete $self->session->{user};
	delete $self->session->{player};
	$self->redirect_to('/');

	return;
}
