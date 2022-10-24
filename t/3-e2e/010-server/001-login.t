# HARNESS-CATEGORY-IMMISCIBLE

use Model::User;
use ActorTest;
use Utils;

use E2ETest;
use Mojo::IOLoop;
use Mojo::JSON qw(to_json);

use testheader;

Utils->bootstrap_lore;

e2e_test sub ($server_port) {
	my ($actor, %related_models) = ActorTest->save_actor;

	my $login_data = {
		email => $related_models{user}->email,
		password => 'asdfasdf',
	};

	my $client = Mojo::IOLoop->client({address => '127.0.0.1', port => $server_port} =>
		sub ($loop, $err, $stream) {
			$stream->on(read => sub ($stream, $bytes) {
				chomp $bytes;
				is $bytes, '1;1', 'login status ok';
				Mojo::IOLoop->stop;
			});

			$stream->write('1;login;' . to_json($login_data));
		}
	);

	Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
};

done_testing 1;

