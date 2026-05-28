package Game::TestClientBag;

use My::Moose;
use Game::TestClient;
use Test2::API qw(context);
use IO::Async::Timer::Periodic;
use IO::Async::Timer::Countdown;

use header;

has field 'clients' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Game::TestClient']],
	default => sub { [] },
	'handles[]' => {
		'add_client' => 'push',
	}
);

has param 'timeout' => (
	isa => Types::PositiveInt,
	default => 10,
);

sub run ($self)
{
	my $loop = DI->get('loop');
	my @clients = $self->clients->@*;
	$_->run for @clients;

	my @notifiers;

	push @notifiers, IO::Async::Timer::Periodic->new(
		interval => 0.5,
		on_tick => sub {
			if (!@clients) {
				$loop->stop;
			}
			else {
				my @finished = grep { $_->finished } @clients;
				if (@finished) {
					my $ctx = context;
					for my $tester (@finished) {
						$ctx->ok($tester->success, 'tester finished normally');
					}
					$ctx->release;

					@clients = grep { !$_->finished } @clients;
				}
			}
		},
	)->start;

	push @notifiers, IO::Async::Timer::Countdown->new(
		delay => $self->timeout,
		on_expire => sub {
			my $ctx = context;
			my $count = @clients;

			if ($count) {
				my $report = join "\n", map {
					'client '
						. $_->actor->id . ': <'
						. $_->actions->[$_->action_index] . '> '
						. $_->actions->[$_->action_index]->get_expected_data
				} @clients;

				$ctx->fail("testing timed out with $count clients still running:\n$report");
			}
			else {
				$ctx->ok('all testers finished');
			}

			$ctx->release;
			$loop->stop;
		},
	)->start;

	$loop->add($_) for @notifiers;
	$loop->run;
	$loop->remove($_) for @notifiers;

	return;
}

