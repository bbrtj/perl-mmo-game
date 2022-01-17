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

has 'key' => (
	is => 'ro',
);

sub get_key ($self, $id)
{
	$id = ":$id"
		if defined $id;

	return $self->key . ($id // '');
}

sub broadcast ($self, $id, $data)
{
	$self->pubsub->notify($self->get_key($id) => $self->encode($data));
	return;
}

sub listen ($self, $id, $callback)
{
	my $wrapped_callback = $self->pubsub->listen(
		$self->get_key($id) => sub {
			@_ = ($self->decode($_[1]));
			goto $callback;
		}
	);

	return $wrapped_callback;
}

sub unlisten ($self, $id, $wrapped_callback)
{
	$self->pubsub->unlisten($self->get_key($id) => $wrapped_callback);
	return;
}
