package Web::Controller::API::Socket;

use My::Moose -constr;
use DI;
use Web::Message;

use header;

extends 'Mojolicious::Controller';

has 'pubsub' => (
	is => 'ro',
	default => sub { DI->get('redis')->pubsub },
);

has 'encoder' => (
	is => 'ro',
	default => sub { DI->get('encoder') },
);

sub websocket ($self)
{
	local $i18n::CURRENT_LANG = $self->session->{lang};
	my $user = $self->stash('user');
	$self->inactivity_timeout(3600);

	# react to websocket messages
	# TODO: should exceptions be caught?
	# TODO: save user game session state
	$self->on(
		json => sub ($, $msg) {
			my ($id, $type, $data) = $msg->@{qw(n t d)};
			$data //= {};

			Web::Message->handle($id, $type, $user, $data);
		}
	);

	# send async stuff back from backend
	my $key = 'server_echo:' . $user->user_id;

	my $cb = $self->pubsub->listen(
		$key => sub ($, $data) {
			my $json = $self->decoder->decode($data);
			$self->send({json => $json});
		}
	);

	$self->on(
		finish => sub ($, $code, $reason = undef) {
			$self->pubsub->unlisten($key => $cb);
		}
	);
}

