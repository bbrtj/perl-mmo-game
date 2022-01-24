package Service::Channel;

use My::Moose;

use header;

has 'store' => (
	is => 'ro',
);

has 'encoder' => (
	is => 'ro',
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
	$self->store->pubsub->notify($self->get_key($id) => $self->encoder->encode($data));
	return;
}

sub listen ($self, $id, $callback)
{
	my $wrapped_callback = $self->store->pubsub->listen(
		$self->get_key($id) => sub {
			@_ = ($self->encoder->decode($_[1]));
			goto $callback;
		}
	);

	return $wrapped_callback;
}

sub unlisten ($self, $id, $wrapped_callback)
{
	$self->store->pubsub->unlisten($self->get_key($id) => $wrapped_callback);
	return;
}
