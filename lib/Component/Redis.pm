package Component::Redis;

use My::Moose;
use Mojo::Redis;

use header;

with 'Component::Role::HasEnv';

has param 'redis' => (
	isa => Types::InstanceOf ['Mojo::Redis'],
	lazy => sub ($self) {
		Mojo::Redis->new($self->env->getenv('REDIS_CONNECTION'));
	},
	handles => [qw(db pubsub)],
);

