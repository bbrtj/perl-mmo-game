# HARNESS-CATEGORY-IMMISCIBLE

use E2ETest;
use Mojo::IOLoop;
use Mojo::JSON qw(from_json);

use ActorTest;
use Utils;

use testheader;

use constant CLIENTS_COUNT => 3;

e2e_test {

	my %finished;
	foreach my $client_n (1 .. CLIENTS_COUNT) {
		my $password = 'Testpassword123#';
		my ($actor, %related_models) = ActorTest->save_actor($password);
		my (\@send_queue, \@receive_queue) = ActorTest->actor_server_login_data($actor, $related_models{user}, $password);

		e2e_client(
			shift @send_queue,
			sub ($stream, $bytes, $receive_no) {
				die "unexpected data: $bytes"
					unless @receive_queue;

				my @parts = split Server::Config::PROTOCOL_CONTROL_CHARACTER, $bytes, 2;
				my @wanted = @{shift @receive_queue};

				if (is_ref $wanted[1]) {
					$parts[1] = from_json $parts[1];
				}

				is \@parts, \@wanted, 'data received ok for test ' . $receive_no;

				$stream->write(shift @send_queue)
					if @send_queue;
			}
		);

		Mojo::IOLoop->recurring(
			1 => sub {
				$finished{$client_n} = 1
					if !@receive_queue;
			}
		);
	}

	Mojo::IOLoop->recurring(
		1 => sub {
			Mojo::IOLoop->stop
				if (keys %finished) == CLIENTS_COUNT;
		}
	);

	Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
};

done_testing;

