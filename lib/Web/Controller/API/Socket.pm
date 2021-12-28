package Web::Controller::API::Socket;

use My::Moose -constr;
use DI;
use Web::Message;
use Exception::WebSocket::CorruptedInput;

use header;

extends 'Mojolicious::Controller';

has 'pubsub' => (
	is => 'ro',
	default => sub { DI->get('redis')->pubsub },
);

sub websocket ($self)
{
	my $user = $self->stash('user');
	$self->inactivity_timeout(3600);

	# react to websocket messages
	# TODO: should exceptions be caught?
	$self->on(json => sub ($, $msg) {
		my ($type, $data) = $msg->@{qw(type data)};
		$data //= {};

		Exception::WebSocket::CorruptedInput->throw
			if !defined $type
			|| ref $data ne 'HASH';

		my $ret = Web::Message->get_handler($type)->handle($user, $data);
		$self->send_lang($ret->hash)
			if $ret isa 'Resource';
	});

	# send async stuff back from backend
	my $key = 'server_echo:' . $user->user_id;

	# TODO: how to translate strings in this?
	my $cb = $self->pubsub->json($key)->listen($key => sub ($, $json) {
		$self->send_lang($json);
	});

	$self->on(finish => sub ($, $code, $reason = undef) {
		$self->pubsub->unlisten($key => $cb);
	});
}

sub send_lang ($self, $data)
{
	local $i18n::CURRENT_LANG = $self->session->{lang};
	return $self->send({json => $data});
}

