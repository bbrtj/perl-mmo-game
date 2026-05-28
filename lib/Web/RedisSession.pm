package Web::RedisSession;

use My::Moose -constr;
use Component::Cache;
use Future::AsyncAwait;

use header;

extends 'PAGI::Middleware::Session::Store';

has injected 'encoder';

has field 'cache' => (
	constructed => ['Component::Cache', cache_name => 'web_session'],
);

async sub get ($self, $id)
{
	try {
		return $self->encoder->decode(await $self->cache->load($id));
	}
	catch ($ex) {
		die $ex unless $ex isa 'X::RecordDoesNotExist';
		return undef;
	}
}

sub set ($self, $id, $data)
{
	return $self->cache->save($id, $self->encoder->encode($data))->then_done($id);
}

sub delete ($self, $id)
{
	return $self->cache->remove($id)->then_done(true);
}

