package Component::Cache;

use My::Moose;
use all 'X';
use Future::AsyncAwait;

use header;

has injected 'encoder';

has injected 'redis' => (
	handles => {
		'store' => 'redis'
	}
);

has param 'cache_name' => (
	isa => Types::SimpleStr,
	writer => 1,
	lazy => sub { croak 'cache_name was not set in Component::Cache' },
);

sub save ($self, $key, $value)
{
	return $self->store->hset($self->cache_name, $key, $self->encoder->encode($value));
}

sub remove ($self, $key)
{
	return $self->store->hdel($self->cache_name, $key);
}

async sub load ($self, $key)
{
	my $value = await $self->store->hget($self->cache_name, $key);
	X::RecordDoesNotExist->throw
		unless defined $value;

	return $self->encoder->decode($value);
}

