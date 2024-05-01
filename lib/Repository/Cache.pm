package Repository::Cache;

use My::Moose;
use all 'Model';

use header;

extends 'Repository';

has injected 'cache';

has injected 'encoder';

sub save ($self, $model)
{
	$self->cache->set_cache_name($model->get_cache_name);
	return $self->cache->save(lc $model->id, $self->encoder->encode($model));
}

sub remove ($self, $type, $id = undef)
{
	if (!defined $id) {
		$id = $type->id;
		$type = $type->get_cache_name;
	}

	$self->cache->set_cache_name($type);
	return $self->cache->remove(lc $id);
}

sub load ($self, $type, $id)
{
	$self->cache->set_cache_name($type);
	return $self->encoder->decode($self->cache->load(lc $id));
}

