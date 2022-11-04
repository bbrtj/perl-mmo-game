package Service::Channel;

use My::Moose;

use header;

has injected 'store' => aliasing => 'redis';
has injected 'encoder';

has param 'key' => (
	isa => Types::SimpleStr
);

sub get_key ($self, $id)
{
	return $self->key . (defined $id ? ":$id" : '');
}

sub broadcast ($self, $id, $data)
{
	$self->store->pubsub->notify($self->get_key($id) => $self->encoder->encode($data));
	return;
}

# this is a homonym, but we use pubsub->listen, so ignore it
sub listen ($self, $id, $callback)    ## no critic 'Subroutines::ProhibitBuiltinHomonyms'
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

__END__

=pod

This is a communication channel between processes. It uses a KV store
(currently Redis) to do publish/subscribe across processes.

Multiple instances of this class will be defined in C<wire.yml> with different
configuration. This way one class can be used for different context.

