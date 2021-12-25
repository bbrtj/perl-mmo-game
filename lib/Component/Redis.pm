package Component::Redis;

use Types;
use Moo;
use Mojo::Redis;

use header;

with 'Component::Role::HasEnv';

has 'redis' => (
	is => 'ro',
	isa => Types::InstanceOf ['Mojo::Redis'],
	lazy => 1,
	default => sub ($self) {
		Mojo::Redis->new($self->env->getenv('REDIS_CONNECTION'));
	},
);

sub db ($self)
{
	return $self->redis->db;
}
