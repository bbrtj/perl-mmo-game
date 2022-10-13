package Repository::Cache;

use My::Moose;
use Model;
use X::RecordDoesNotExist;

use header;

with 'Repository::Role::Resource';

has 'redis' => (
	is => 'ro',
);

has 'encoder' => (
	is => 'ro',
);

sub save ($self, $model)
{
	my $type = $model->get_cache_name;
	$self->redis->db->hset($type, $model->id, $self->encoder->encode($model->serialize));

	return 1;
}

sub remove ($self, $model)
{
	my $type = $model->get_cache_name;
	$self->redis->db->hdel($type, $model->id);

	return 1;
}

sub load ($self, $type, $id)
{
	my $cache = $self->redis->db->hget($type, $id);
	X::RecordDoesNotExist->throw
		unless defined $cache;

	return Model->from_cache(
		$type,
		$cache ? $self->encoder->decode($cache) : {}
	);
}

