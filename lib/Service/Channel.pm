package Service::Channel;

use My::Moose;

use header;

has 'store' => (
	is => 'ro',
	handles => [qw(pubsub)],
);

has 'encoder' => (
	is => 'ro',
	handles => [qw(encode)],
);

has 'decoder' => (
	is => 'ro',
	handles => [qw(decode)],
);

sub get_key ($self, $user_id)
{
	$user_id = ":$user_id"
		if defined $user_id;

	return 'server_echo' . ($user_id // '');
}

sub broadcast ($self, $user_id, $data)
{
	$self->pubsub->notify($self->get_key($user_id) => $self->encode($data));
	return;
}

sub listen ($self, $user_id, $callback)
{
	my $wrapped_callback = $self->pubsub->listen(
		$self->get_key($user_id) => sub ($, $data) {
			my $struct = $self->decode($data);
			$callback->($struct);
		}
	);

	return $wrapped_callback;
}

sub unlisten ($self, $user_id, $wrapped_callback)
{
	$self->pubsub->unlisten($self->get_key($user_id) => $wrapped_callback);
	return;
}
