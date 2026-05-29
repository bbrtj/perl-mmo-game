package Web::Controller::Bridges;

use My::Moose;
use Future::AsyncAwait;

use header;

extends 'Web::Controller';

has injected 'models_repo';

sub build ($self)
{
	my $main = $self->router->add(
		'/', {
			to => 'prepare_request',
			name => 'global_bridge',
		}
	);
}

sub prepare_request ($self, $ctx)
{
	my $user_id = $ctx->session->get('user', undef);
	my $user;

	# TODO: this can be cached
	$user = $self->models_repo->load(User => $user_id)
		if $user_id;
	$ctx->stash->set('user', $user);

	# TODO: make this 'en' after lore translations are done
	$ctx->stash->set('lang', $ctx->session->get('lang', 'pl'));

	return undef;
}

