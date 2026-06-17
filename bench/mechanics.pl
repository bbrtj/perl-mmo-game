use Game::Helpers;
use Game::Lore::Location;
use Game::Mechanics::Check::Map qw(can_see);
use Game::Mechanics::Movement qw(move);
use Game::Mechanics::Character::Statistics qw(get_current_level get_max_health);
use Game::Object::Movement;
use Model::CharacterVariables;
use Utils;

use header;

use Benchmark::Dumb qw(cmpthese);

my $location = Game::Lore::Location->new(id => 'TEST', name => 'test', map => 'test_map');
my $map = $location->map;

my $variables = Model::CharacterVariables->new(
	pos_x => 4,
	pos_y => 3,
	health => 0,
	energy => 0,
	location_id => 'TEST',
);

my $movement = Game::Object::Movement->new(
	variables => $variables,
	x => 7.3,
	y => 8.5,
	speed => 0.1,
	time => time,
);

my $class = lore_class 'Warden';
my $stats = {
	'sstat.stam' => 10,
	'pstat.con' => 10,
};

cmpthese 200.01, {
	line_of_sight => sub {
		die unless can_see($map, [4.5, 3.8], [7.9, 8.3])->result;
	},
	movement => sub {
		die unless move($movement, $map, time);
	},
	level => sub {
		die unless get_current_level(250) == 3;
	},
	health => sub {
		die unless get_max_health($class, $stats) == 80 * 1.6 * 1.5;
	},
};

