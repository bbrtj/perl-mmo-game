use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use all 'Model', 'X';
use DateTime;
use Data::ULID qw(ulid);

use header;

use Benchmark qw(cmpthese);

my $ulid = ulid;

cmpthese -2, {
	'DateTime' => sub {
		DateTime->from_epoch(time);
	},
	'ulid' => sub {
		ulid;
	},
	'Model::Player' => sub {
		Model::Player->new(user_id => $ulid);
	},
	'Model::Character' => sub {
		Model::Character->new(player_id => $ulid, name => 'DUMMY', class_id => 'DUMMY');
	},
	'X::PlayerNotFound' => sub {
		X::PlayerNotFound->new(msg => 'player was not found');
	},
};

