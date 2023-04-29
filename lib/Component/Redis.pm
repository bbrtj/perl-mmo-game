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
	handles => [qw(pubsub)],
);

has cached 'db' => (
	lazy => sub ($self) { $self->redis->db },
);

has cached 'nonblocking_connection' => (
	lazy => sub ($self) { $self->db->connection(0) },
);

sub fast_publish ($self, $channel, $message)
{
	# TODO: handle errors?
	# This should neither block nor fire a callback
	$self->nonblocking_connection->write(PUBLISH => $channel, $message);
	return;
}

