# HARNESS-CATEGORY-IMMISCIBLE

use E2ETest;
use Mojo::IOLoop;
use Mojo::JSON qw(from_json to_json);

use Model;
use ActorTest;
use Utils;
use Resource::CharacterList;
use Resource::LocationData;

use testheader;

Utils->bootstrap_lore;

plan 4;

e2e_test sub ($server_port) {
	my $password = 'Testpassword123#';
	my ($actor, %related_models) = ActorTest->save_actor($password);

	my @send_queue = (
		'1;login;' . to_json({email => $related_models{user}->email, password => $password}),
		'2;list_characters',
		'3;enter_game;' . $related_models{player}->id,
	);

	my @receive_queue = (
		['1', '1'],
		['2', Resource::CharacterList->new(DI->get('units_repo')->load_user($related_models{user}->id))->serialize],
		['3', '1'],
		['', Resource::LocationData->new(DI->get('lore_data_repo')->load($related_models{variables}->location_id))->serialize],
	);

	my $test = 1;
	my $client = Mojo::IOLoop->client(
		{address => '127.0.0.1', port => $server_port},
		sub ($loop, $err, $stream) {
			die "error connecting: $err" if ($err);

			$stream->on(
				read => sub ($stream, $bytes) {
					chomp $bytes;

					die "unexpected data: $bytes"
						unless @receive_queue;

					my @parts = split ';', $bytes;
					my @wanted = @{shift @receive_queue};

					if (is_ref $wanted[1]) {
						$parts[1] = from_json $parts[1];
					}

					is \@parts, \@wanted, 'data received ok for test ' . $test++;

					Mojo::IOLoop->stop
						if !@receive_queue;

					$stream->write(shift @send_queue)
						if @send_queue;
				}
			);

			$stream->write(shift @send_queue);
		}
	);

	Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
};

done_testing;

