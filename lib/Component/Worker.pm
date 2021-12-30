package Component::Worker;

use Types;
use My::Moose;
use Minion;

use header;

with 'Component::Role::HasEnv';

has 'minion' => (
	is => 'ro',
	isa => Types::InstanceOf ['Minion'],
	lazy => 1,
	default => sub ($self) {
		Minion->new(Redis => $self->env->getenv('REDIS_CONNECTION'));
	},
	handles => [
		qw(
			enqueue
		)
	],
);
