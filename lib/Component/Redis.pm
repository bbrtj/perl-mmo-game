package Component::Redis;

use My::Moose;
use Net::Async::Redis::XS;
use Future::AsyncAwait;

use header;

with 'Component::Role::HasEnv';

has param 'redis' => (
	isa => InstanceOf ['Net::Async::Redis::XS'],
	lazy => sub ($self) {
		my $redis = Net::Async::Redis::XS->new;
	},
	handles => [qw(publish subscribe unsubscribe)],
);

sub connect ($self, $loop)
{
	my $redis = $self->redis;

	$redis->configure(
		host => $self->env->getenv('REDIS_HOST'),
		port => $self->env->getenv('REDIS_PORT'),
	);

	$loop->add($redis);
	return $redis->connect;
}

