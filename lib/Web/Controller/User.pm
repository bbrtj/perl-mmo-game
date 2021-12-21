package Web::Controller::User;

use Moo;
use Web::Form::Login;
use Web::Form::Register;

use header;

extends 'Mojolicious::Controller';

sub login ($self)
{
	my $form = Web::Form::Login->new;

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
	$self->render_lang('user/login');

	return;
}

sub register ($self)
{
	my $form = Web::Form::Register->new;

	if ($self->req->method eq 'POST') {
		# TODO: csrf
		$form->set_input($self->req->body_params->to_hash);

		if ($form->valid) {
			DI->get('user_service')->register_user($form->fields);
		}
	}

	$self->stash('form', $form);
	$self->render_lang('user/register');

	return;
}

sub logout ($self)
{
	delete $self->session->{user};
	delete $self->session->{player};
	$self->redirect_to('/');

	return;
}
