package Component::Cache;

use My::Moose;
use all 'X';

use header;

has injected 'redis' => (
	handles => {
		'store' => 'db'
	}
);

has param 'cache_name' => (
	isa => Types::SimpleStr,
	writer => 1,
	lazy => sub { croak 'cache_name was not set in Component::Cache' },
);

sub save ($self, $key, $value)
{
	$self->store->hset($self->cache_name, $key, $value);

	return;
}

sub remove ($self, $key)
{
	$self->store->hdel($self->cache_name, $key);

	return;
}

sub load ($self, $key)
{
	my $value = $self->store->hget($self->cache_name, $key);
	X::RecordDoesNotExist->throw
		unless defined $value;

	return $value;
}

