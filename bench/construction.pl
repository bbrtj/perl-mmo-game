use lib 'local/lib/perl5';
use lib 'lib-base';
use lib 'lib';

use all 'Model', 'X', 'Unit';
use Game::Object::Movement;
use Model::PlayerSession;
use DateTime;
use Data::ULID::XS qw(ulid);
use Time::HiRes qw(time);
use Utils;

use header;

use Benchmark qw(cmpthese);

my $ulid = ulid;
my $character = DI->get('faker_service')->fake_character;
my $variables = DI->get('faker_service')->fake_variables;
my $actor = Unit::Actor->new(character => $character, variables => $variables);

cmpthese - 2, {
	'DateTime' => sub {
		DateTime->from_epoch(time);
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
	'Model::PlayerSession' => sub {
		Model::PlayerSession->new();
	},
	'Game::Object::Movement' => sub {
		Game::Object::Movement->new(
			variables => $variables,
			x => 5.3,
			y => 6.5,
			speed => 9,
			time => time,
		);
	},
};

