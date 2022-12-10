package Game::TestClientBag;

use My::Moose;
use Mojo::IOLoop;
use Game::TestClient;
use Test2::API qw(context);

use header;

has field 'clients' => (
	isa => Types::ArrayRef [Types::InstanceOf ['Game::TestClient']],
	default => sub { [] },
	'handles[]' => {
		'add_client' => 'push',
	}
);

has field 'timeout' => (
	isa => Types::PositiveInt,
	default => 10,
);

sub run ($self, $loop = Mojo::IOLoop->singleton)
{
	my @clients = $self->clients->@*;
	$_->run for @clients;

	my $finished_for = 0;
	$loop->recurring(
		1 => sub {
			if (!@clients && $finished_for++ == 2) {
				# stop the loop after waiting 2 secs to check if there is any
				# trailing data
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
		}
	);

	$loop->timer(
		$self->timeout => sub {
			my $ctx = context;
			$ctx->fail('testing timed out');
			$ctx->release;

			$loop->stop;
		}
	);

	$loop->start unless $loop->is_running;
	return;
}

