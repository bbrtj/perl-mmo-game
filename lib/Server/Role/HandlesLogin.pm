package Server::Role::HandlesLogin;

use My::Moose::Role;

use header;

requires qw(cache_repo data_bus send_to);

has injected 'sessions_cache' => (
	'handles->' => {
		'save_session' => 'save',
		'load_session' => 'load',
		'remove_session' => 'remove',
	}
);

sub login ($self, $session_id, $user_id)
{
	my $session = $self->cache_repo->load(PlayerSession => $session_id);
	return if $session->is_logged_in;

	try {
		my $other_session_id = $self->load_session($user_id);
		$self->send_to(
			$other_session_id,
			undef,
			drop => !!1,
		);
	}
	catch ($e) {

		# No other sessions? Great!
		die $e unless $e isa 'X::RecordDoesNotExist';
	}

	$session->set_logged_in($user_id);

	$self->cache_repo->save($session);
	$self->save_session($user_id => $session_id);

	return;
}

# NOTE: this does not have to notify the TCP server, as it came after the server dropped the connection
sub logout ($self, $session_id)
{
	my $session = $self->cache_repo->load(PlayerSession => $session_id);
	return unless $session->is_logged_in;

	my $user_id = $session->user_id;

	if ($session->is_playing) {
		$self->data_bus->dispatch($session->location_id, 'player_has_left_game', $session->player_id);
	}

	$self->cache_repo->remove($session);
	$self->remove_session($user_id);

	return;
}

