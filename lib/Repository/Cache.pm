package Repository::Cache;

use My::Moose;
use Model;

use header;

extends 'Repository';

has injected 'cache';

has injected 'encoder';

sub save ($self, $model)
{
	$self->cache->set_cache_name($model->get_cache_name);
	return $self->cache->save($model->id, $self->encoder->encode($model->serialize));
}

sub remove ($self, $model)
{
	$self->cache->set_cache_name($model->get_cache_name);
	return $self->cache->remove($model->id);
}

sub load ($self, $type, $id)
{
	$self->cache->set_cache_name($type);
	my $cache = $self->cache->load($id);

	return Model->from_cache(
		$type,
		$self->encoder->decode($cache)
	);
}

