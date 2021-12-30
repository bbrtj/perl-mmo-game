package Web::Controller::API::Socket;

use My::Moose -constr;
use DI;
use Web::Message;
use Mojo::JSON qw(to_json);

use header;

extends 'Mojolicious::Controller';

has 'channel' => (
	is => 'ro',
	default => sub { DI->get('channel_service') },
);

sub websocket ($self)
{
	my $user = $self->stash('user')->id;
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

	my $cb = $self->channel->listen(
		$user,
		sub ($struct) {
			$self->reply($struct);
		}
	);

	my $cb_global = $self->channel->listen(
		undef,
		sub ($struct) {
			$self->reply($struct);
		}
	);

	$self->on(
		finish => sub ($, $code, $reason = undef) {
			$self->channel->unlisten($user, $cb);
			$self->channel->unlisten(undef, $cb_global);
		}
	);

	return;
}

sub reply ($self, $struct)
{
	local $i18n::CURRENT_LANG = $self->stash('lang');
	return $self->send(to_json($struct));
}

