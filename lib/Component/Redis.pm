package Component::Redis;

use My::Moose;
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
	handles => [qw(db pubsub)],
);

