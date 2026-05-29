package Web::Controller::User;

use My::Moose;
use Web::Form::Login;
use Web::Form::Register;
use Future::AsyncAwait;

use header;

extends 'Web::Controller';

sub build ($self)
{
	my $user = $self->router->find('global_bridge')->add('/user');

	$user->add(
		'/login' => {
			to => 'login',
			action => 'http.get',
			name => 'login',
		}
	);

	$user->add(
		'/login' => {
			to => 'login_submit',
			action => 'http.post',
		}
	);

	$user->add(
		'/logout' => {
			to => 'logout',
			name => 'logout',
		}
	);

	$user->add(
		'/register' => {
			to => 'register',
			action => 'http.get',
			name => 'register',
		}
	);

	$user->add(
		'/register' => {
			to => 'register_submit',
			action => 'http.post',
		}
	);

}

sub login ($self, $ctx)
{
	my $form = Web::Form::Login->new;

	return $self->template_lang($ctx, 'user/login', {form => $form});
}

async sub login_submit ($self, $ctx)
{
	my $form = Web::Form::Login->new;

	# TODO: csrf
	$form->set_input(await($ctx->req->form_params)->as_hashref);

	if ($form->valid) {

		# TODO: referrer

		# TODO: success flash message
		$ctx->session->set('user', $form->user->id);
		await $ctx->res->redirect($self->url_for('main_page'));
		return;
	}
	else {
		# TODO: throttle
	}

	return $self->template_lang($ctx, 'user/login', {form => $form});
}

sub register ($self, $ctx)
{
	my $form = Web::Form::Register->new;

	return $self->template_lang($ctx, 'user/register', {form => $form});
}

async sub register_submit ($self, $ctx)
{
	my $form = Web::Form::Register->new;

	# TODO: csrf
	$form->set_input(await($ctx->req->form_params)->as_hashref);

	if ($form->valid) {
		DI->get('user_service')->register_user($form->fields);

		# TODO: success flash message
		await $ctx->res->redirect($self->url_for('login'));
		return;
	}

	return $self->template_lang($ctx, 'user/register', {form => $form});
}

sub logout ($self, $ctx)
{
	$ctx->session->delete('user');
	$ctx->res->redirect($self->url_for('main_page'));
}

